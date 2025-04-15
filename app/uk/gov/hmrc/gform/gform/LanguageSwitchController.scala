/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.gform

import cats.implicits._
import play.api.i18n.{ I18nSupport, Lang }
import play.api.mvc._
import uk.gov.hmrc.gform.auth.models.{ IsAgent, OperationWithForm }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.lookup.{ AjaxLookup, LookupLabel, LookupRegistry, RadioLookup }
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendHeaderCarrierProvider
import uk.gov.hmrc.play.language.{ LanguageController, LanguageUtils }

import scala.concurrent.{ ExecutionContext, Future }

class LanguageSwitchController(
  auth: AuthenticatedRequestActionsAlgebra[Future],
  languageUtils: LanguageUtils,
  lookupRegistry: LookupRegistry,
  gformConnector: GformConnector,
  config: FrontendAppConfig,
  controllerComponents: ControllerComponents,
  actionBuilder: ActionBuilder[Request, AnyContent]
)(implicit ec: ExecutionContext)
    extends LanguageController(languageUtils, controllerComponents) with FrontendHeaderCarrierProvider
    with I18nSupport {

  protected def fallbackURL: String = "/"

  protected def languageMap: Map[String, Lang] = config.availableLanguages

  def switchToLanguageDataChange(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    language: String
  ): Action[AnyContent] =
    actionBuilder.async(request =>
      auth
        .getLoggedIn(request)
        .flatMap(maybeAuthSuccessful =>
          maybeAuthSuccessful.fold(switchToLanguage(language)(request)) { authSuccessful =>
            for {
              maybeFormTemplate <- gformConnector.maybeFormTemplate(formTemplateId)(hc(request), ec)
              maybeDrm <-
                maybeFormTemplate
                  .map { formTemplate =>
                    formTemplate.draftRetrieval
                      .flatMap(dr => authSuccessful.retrievals.getAffinityGroup.flatMap(ag => dr.mapping.get(ag)))
                      .collect {
                        case BySubmissionReference                => BySubmissionReference
                        case FormAccessCode(continueOrDeletePage) => FormAccessCodeForAgents(continueOrDeletePage)
                      }
                      .getOrElse((formTemplate.draftRetrievalMethod, authSuccessful.retrievals) match {
                        case (BySubmissionReference, _)                    => BySubmissionReference
                        case (drm @ FormAccessCodeForAgents(_), IsAgent()) => drm
                        case (other, _)                                    => other
                      })
                  }
                  .pure[Future]
              res <- (maybeDrm, maybeAccessCode) match {
                       // If access code/submission ref required by form and there isn't one, then just do language
                       // switch without attempting to change & save data - to cater for pages where access code not
                       // yet entered
                       case (Some(FormAccessCodeForAgents(_)), None) => switchToLanguage(language)(request)
                       case (Some(BySubmissionReference), None)      => switchToLanguage(language)(request)
                       case (_, _)                                   => switchLanguageWithDataChange(formTemplateId, maybeAccessCode, language)(request)
                     }
            } yield res
          }
        )
    )

  def switchToLanguageNoDataChange(language: String): Action[AnyContent] =
    switchToLanguage(language)

  private def switchLanguageWithDataChange(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    language: String
  ): Action[AnyContent] =
    auth
      .authAndRetrieveForm[SectionSelectorType.Normal](
        formTemplateId,
        maybeAccessCode,
        OperationWithForm.SwitchLanguage
      ) { implicit request => l => cache => sse => formModelOptics =>
        val lookups: List[(ModelComponentId, Register)] =
          formModelOptics.formModelRenderPageOptics.allFormComponents.collect {
            case fc @ IsText(Text(Lookup(register, _), _, _, _, _, _, _)) => fc.id.modelComponentId                                 -> register
            case fc @ IsOverseasAddress(_)                                => fc.id.toAtomicFormComponentId(OverseasAddress.country) -> Register.Country
          }

        val maybeLanguageToSwitchTo: Option[LangADT] =
          languageMap.get(language).map(l => LangADT.stringToLangADT(l.code))

        maybeLanguageToSwitchTo
          .map { languageToSwitchTo =>
            val switchedLabel: List[(ModelComponentId, LookupLabel)] =
              lookups.flatMap { case (modelComponentId, register) =>
                val formField: FormField =
                  formModelOptics.formModelRenderPageOptics.toFormField(modelComponentId)

                switchLookupLabelLanguage(register, LookupLabel(formField.value), l, languageToSwitchTo).map(
                  modelComponentId -> _
                )
              }

            val newFormData: List[FormField] = switchedLabel.map { case (modelComponentId, lookupLabel) =>
              FormField(modelComponentId, lookupLabel.label)
            }

            val form = cache.form
            val formIdData: FormIdData = FormIdData.fromForm(form, maybeAccessCode)

            val mergedFormData = form.formData ++ FormData(newFormData)
            val updatedFormData = resetAllConfirmationPages(formModelOptics, mergedFormData)

            val userData: UserData = UserData(
              formData = updatedFormData,
              formStatus = form.status,
              visitsIndex = form.visitsIndex,
              thirdPartyData = form.thirdPartyData,
              componentIdToFileId = form.componentIdToFileId,
              taskIdTaskStatus = form.taskIdTaskStatus
            )

            gformConnector.updateUserData(formIdData, userData).flatMap { _ =>
              switchToLanguage(language)(request)
            }
          }
          .getOrElse {
            switchToLanguage(language)(request)
          }
      }

  private def resetAllConfirmationPages(
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    formData: FormData
  ): FormData = {
    val confirmationPages = formModelOptics.formModelRenderPageOptics.formModel.confirmationPageMap.map(_._2)
    val modelComponentIds = confirmationPages.map(_.question.id.modelComponentId)
    formData.copy(fields = formData.fields.filterNot(field => modelComponentIds.contains(field.id)))
  }

  private def switchLookupLabelLanguage(
    register: Register,
    lookupLabel: LookupLabel,
    languageToSwichFrom: LangADT,
    languageToSwitchTo: LangADT
  ): Option[LookupLabel] =
    lookupRegistry.get(register).flatMap { lookupType =>
      val options = lookupType match {
        case RadioLookup(options)      => options
        case AjaxLookup(options, _, _) => options
      }
      val maybeLookupInfo = options.lookupInfo(lookupLabel)(languageToSwichFrom)
      maybeLookupInfo.flatMap { lookupInfo =>
        val newLookupOptions = options.m.get(languageToSwitchTo)
        newLookupOptions.flatMap { lookupOptions =>
          lookupOptions.options
            .find { case (key, lookupInfo2) =>
              lookupInfo.index === lookupInfo2.index
            }
            .map { case (lookupLabel, _) => lookupLabel }
        }
      }
    }

}
