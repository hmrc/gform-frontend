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

package uk.gov.hmrc.gform.addresslookup

import cats.data.NonEmptyList
import cats.implicits._
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Action, AnyContent, Call, MessagesControllerComponents, Request, Result }
import play.twirl.api.Html
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.{ Envelope, EnvelopeWithMapping }
import uk.gov.hmrc.gform.gform.FastForwardService
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.lookup.LocalisedLookupOptions
import uk.gov.hmrc.gform.models.{ Basic, Bracket, FormModelBuilder, Visibility }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ FastForward, SectionSelectorType }
import uk.gov.hmrc.gform.monoidHtml
import uk.gov.hmrc.gform.sharedmodel.form.FormData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, Expr, FormComponent, FormKind, FormTemplateContext, Page, Section }
import uk.gov.hmrc.gform.sharedmodel.{ LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId, SectionNumber, SectionTitle4Ga, SuppressErrors }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.gform.views.html.addresslookup
import uk.gov.hmrc.gform.views.html.hardcoded.pages.br
import uk.gov.hmrc.http.{ HeaderCarrier, NotFoundException }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class AddressLookupController(
  auth: AuthenticatedRequestActions,
  addressLookupService: AddressLookupService[Future],
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  messagesControllerComponents: MessagesControllerComponents,
  recalculation: Recalculation[Future, Throwable],
  formControllerRequestHandler: FormControllerRequestHandler,
  validationService: ValidationService,
  fastForwardService: FastForwardService,
  lookupOptions: LocalisedLookupOptions
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  def chooseAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        val maybeAddressLookupResult: Option[AddressLookupResult] =
          cache.form.thirdPartyData.postcodeLookup.flatMap(_.get(formComponentId))
        maybeAddressLookupResult
          .fold(
            Future.failed[Result](
              new NotFoundException(s"No addresslookup found for FormComponentId: ${formComponentId.value}.")
            )
          ) { addressLookupResult =>
            addressLookupResult.response.addresses
              .fold(
                Ok(
                  addresslookup.no_address_found(
                    cache.formTemplate,
                    frontendAppConfig,
                    addressLookupResult.request.postcode,
                    routes.AddressLookupController
                      .tryDifferentAddress(
                        cache.formTemplate._id,
                        maybeAccessCode,
                        formComponentId,
                        sectionNumber
                      ),
                    routes.AddressLookupController
                      .enterAddress(
                        cache.formTemplate._id,
                        maybeAccessCode,
                        formComponentId,
                        sectionNumber,
                        SuppressErrors.Yes
                      ),
                    goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode)
                  )
                ).pure[Future]
              ) { addressRecords =>
                if (addressRecords.size === 1) {
                  val addressId = addressRecords.head.id
                  addressLookupService
                    .saveAddress(cache.form, maybeAccessCode, formComponentId, addressId)
                    .as(
                      Redirect(
                        routes.AddressLookupController
                          .confirmAddress(
                            formTemplateId,
                            maybeAccessCode,
                            formComponentId,
                            sectionNumber
                          )
                      )
                    )
                } else
                  Ok(
                    renderChooseAddressPage(
                      formComponentId,
                      addressIdForm,
                      addressRecords,
                      cache,
                      formModelOptics,
                      sectionNumber,
                      maybeAccessCode,
                      addressLookupResult
                    )
                  ).pure[Future]
              }

          }
    }

  private val addressIdForm: play.api.data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "addressId" -> play.api.data.Forms.nonEmptyText
    )
  )

  private def goToSectionNumberLink(
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode]
  ): Call = {

    val sectionTitle4Ga = SectionTitle4Ga.sectionTitle4GaFactory(
      formModelOptics.formModelVisibilityOptics.formModel(sectionNumber),
      sectionNumber
    )

    uk.gov.hmrc.gform.gform.routes.FormController.form(
      cache.formTemplate._id,
      maybeAccessCode,
      sectionNumber,
      sectionTitle4Ga,
      SuppressErrors.Yes,
      List(FastForward.Yes)
    )
  }

  private def renderChooseAddressPage(
    formComponentId: FormComponentId,
    form: play.api.data.Form[String],
    addressRecords: NonEmptyList[PostcodeLookup.AddressRecord],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    addressLookupResult: AddressLookupResult
  )(implicit request: Request[AnyContent], l: LangADT, sse: SmartStringEvaluator): Html = {
    val addressSelectionPage = new AddressSelectionPage(
      addressRecords,
      form,
      cache.form.thirdPartyData.addressSelectionFor(formComponentId)
    )

    val backHref = goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode)

    addresslookup.choose_address(
      cache.formTemplate,
      frontendAppConfig,
      addressSelectionPage,
      routes.AddressLookupController
        .submitAddress(cache.formTemplate._id, maybeAccessCode, formComponentId, sectionNumber),
      routes.AddressLookupController
        .enterAddress(
          cache.formTemplate._id,
          maybeAccessCode,
          formComponentId,
          sectionNumber,
          SuppressErrors.Yes
        ),
      backHref,
      addressLookupResult
    )
  }

  def submitAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        addressIdForm
          .bindFromRequest()
          .fold(
            errorForm =>
              cache.form.thirdPartyData
                .addressesFor(formComponentId)
                .fold(
                  throw new NotFoundException(s"No addresses found for FormComponentId: ${formComponentId.value}.")
                ) { case (addressRecords, addressLookupResult) =>
                  BadRequest(
                    renderChooseAddressPage(
                      formComponentId,
                      errorForm,
                      addressRecords,
                      cache,
                      formModelOptics,
                      sectionNumber,
                      maybeAccessCode,
                      addressLookupResult
                    )
                  )
                }
                .pure[Future],
            addressId =>
              addressLookupService
                .saveAddress(cache.form, maybeAccessCode, formComponentId, addressId)
                .as(
                  Redirect(
                    routes.AddressLookupController
                      .confirmAddress(
                        formTemplateId,
                        maybeAccessCode,
                        formComponentId,
                        sectionNumber
                      )
                  )
                )
          )

    }

  def confirmAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        cache.form.thirdPartyData.addressLines(formComponentId) match {
          case None => BadRequest("TRY AGAIN").pure[Future]
          case Some(addressLines) =>
            val backHref =
              if (cache.form.thirdPartyData.enteredAddressFor(formComponentId).isDefined) {
                routes.AddressLookupController
                  .enterAddress(
                    formTemplateId,
                    maybeAccessCode,
                    formComponentId,
                    sectionNumber,
                    SuppressErrors.Yes
                  )
              } else {
                val isSingleAddress = cache.form.thirdPartyData.addressesFor(formComponentId).fold(false) {
                  case (addressRecords, _) => addressRecords.size === 1
                }

                if (isSingleAddress) {
                  goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode)
                } else {
                  routes.AddressLookupController
                    .chooseAddress(formTemplateId, maybeAccessCode, formComponentId, sectionNumber)
                }
              }

            val address =
              addressLines
                .map(addresslookup.address_line(_))
                .intercalate(br())

            val confirmAddressAndContinue = routes.AddressLookupController
              .confirmAddressAndContinue(
                formTemplateId,
                maybeAccessCode,
                formComponentId,
                sectionNumber
              )
            val enterAddressHref = routes.AddressLookupController
              .enterAddress(
                cache.formTemplate._id,
                maybeAccessCode,
                formComponentId,
                sectionNumber,
                SuppressErrors.Yes
              )
            Ok(
              addresslookup
                .review_and_confirm_address(
                  cache.formTemplate,
                  frontendAppConfig,
                  address,
                  confirmAddressAndContinue,
                  enterAddressHref,
                  backHref
                )
            )
              .pure[Future]
        }
    }

  def tryDifferentAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => l => cache => sse => formModelOptics =>
        addressLookupService
          .cleanAddress(cache.form, maybeAccessCode, formComponentId)
          .as(
            Redirect(goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode))
          )
    }

  def confirmAddressAndContinue(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => l => cache => sse => formModelOptics =>
        def resolveRedirect: Call = {
          val ff =
            routes.AddressLookupController.fastForwardAfterConfirmation(
              formTemplateId,
              maybeAccessCode,
              None // TODO JoVl fastforward in tasklist will start with a first task, which is not what we want
            )
          val bracket: Bracket[Visibility] =
            formModelOptics.formModelVisibilityOptics.formModel.brackets.withSectionNumber(sectionNumber)

          bracket.fold { nonRepeatingPage =>
            ff
          } { repeatingPage =>
            ff
          } { addToList =>
            val iteration: Bracket.AddToListIteration[Visibility] = addToList.iterationForSectionNumber(sectionNumber)
            routes.AddressLookupController.fastForwardAfterConfirmation(
              formTemplateId,
              maybeAccessCode,
              iteration.checkYourAnswers
                .map(_.sectionNumber)
                .orElse(iteration.allSingletonSectionNumbers.find(_ > sectionNumber))
            )
          }
        }

        val updatedForm = addressLookupService
          .populatePostcodeIfEmpty(
            cache.form,
            maybeAccessCode,
            formComponentId,
            formModelOptics.formModelVisibilityOptics,
            sectionNumber
          )

        addressLookupService
          .flagAddressAsConfirmed(updatedForm, maybeAccessCode, formComponentId)
          .as(Redirect(resolveRedirect))
    }

  def fastForwardAfterConfirmation(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    maybeSectionNumber: Option[SectionNumber]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => sse => formModelOptics =>
        maybeSectionNumber.fold(
          fastForwardService
            .redirectFastForward[SectionSelectorType.Normal](
              cache,
              maybeAccessCode,
              formModelOptics,
              None
            ) // TODO JoVl Revisit maybeCoordinates param
        ) { sn =>
          fastForwardService.redirectStopAt[SectionSelectorType.Normal](sn, cache, maybeAccessCode, formModelOptics)
        }

    }

  def enterAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    se: SuppressErrors
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => realFormModelOptics =>
        mkSyntheticFormModelOptics(formComponentId, cache, maybeAccessCode) {
          syntheticFormComponent => syntheticCache => formModelOptics =>
            val cacheData = syntheticCache.toCacheData

            val envelopeWithMapping: EnvelopeWithMapping = EnvelopeWithMapping(Envelope.empty, syntheticCache.form)

            formControllerRequestHandler
              .handleSuppressErrors(
                formModelOptics,
                List(cache.formTemplate.sectionNumberZero),
                cacheData,
                envelopeWithMapping,
                validationService.validatePageModel,
                se
              )
              .map { formHandlerResult =>
                val formFieldValidationResult: FormFieldValidationResult =
                  formHandlerResult.validationResult(syntheticFormComponent)

                val enterAddressPage = new EnterAddressPage(syntheticFormComponent, formFieldValidationResult)
                val formAction =
                  routes.AddressLookupController
                    .enterAddressSubmit(
                      formTemplateId,
                      maybeAccessCode,
                      formComponentId,
                      sectionNumber
                    )
                val backHref =
                  if (cache.form.thirdPartyData.addressRecordFor(formComponentId).isDefined) {
                    routes.AddressLookupController
                      .chooseAddress(
                        formTemplateId,
                        maybeAccessCode,
                        formComponentId,
                        sectionNumber
                      )
                  } else {
                    goToSectionNumberLink(cache, realFormModelOptics, sectionNumber, maybeAccessCode)
                  }
                Ok(
                  addresslookup
                    .enter_address(
                      cache.formTemplate,
                      frontendAppConfig,
                      enterAddressPage,
                      formAction,
                      backHref
                    )
                )
              }
        }
    }

  def enterAddressSubmit(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => _ =>
        mkSyntheticFormModelOptics(formComponentId, cache, maybeAccessCode) {
          syntheticFormComponent => syntheticCache => formModelOptics =>
            processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics, None) {
              requestRelatedData => variadicFormData => enteredVariadicFormData =>
                val browserFormModelOpticsF = FormModelOptics
                  .mkFormModelOptics[DataOrigin.Browser, Future, SectionSelectorType.Normal](
                    variadicFormData,
                    syntheticCache,
                    recalculation
                  )

                val envelopeWithMapping: EnvelopeWithMapping = EnvelopeWithMapping(Envelope.empty, syntheticCache.form)

                val cacheData = syntheticCache.toCacheData

                browserFormModelOpticsF.flatMap { browserFormModelOptics =>
                  addressLookupService.saveEnteredAddress(
                    cache.form,
                    maybeAccessCode,
                    formComponentId,
                    variadicFormData.toFormData
                  ) >>
                    formControllerRequestHandler
                      .handleFormValidation(
                        browserFormModelOptics,
                        cache.formTemplate.sectionNumberZero,
                        cacheData,
                        envelopeWithMapping,
                        validationService.validatePageModel,
                        enteredVariadicFormData
                      )
                      .map { formValidationOutcome =>
                        if (formValidationOutcome.isValid) {
                          Redirect(
                            routes.AddressLookupController.confirmAddressAndContinue(
                              formTemplateId,
                              maybeAccessCode,
                              formComponentId,
                              sectionNumber
                            )
                          )
                        } else {
                          Redirect(
                            routes.AddressLookupController
                              .enterAddress(
                                formTemplateId,
                                maybeAccessCode,
                                formComponentId,
                                sectionNumber,
                                SuppressErrors.No
                              )
                          )
                        }
                      }
                }
            }
        }
    }

  private def mkSyntheticFormComponent(formComponentId: FormComponentId): FormComponent =
    FormComponent(
      id = formComponentId,
      `type` = Address(false, List.empty[Address.Configurable.Mandatory], false, None),
      label = SmartString(LocalisedString(Map.empty), List.empty[Expr]),
      helpText = None,
      shortName = None,
      includeIf = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = false,
      errorMessage = None
    )

  private def mkSyntheticCache(
    cache: AuthCacheWithForm,
    syntheticFormComponent: FormComponent
  ): AuthCacheWithForm = {
    val title = SmartString(LocalisedString(Map.empty), List.empty[Expr])
    val fields = List(syntheticFormComponent)
    val page: Page[Basic] = Page(
      title = title,
      id = None,
      noPIITitle = None,
      description = None,
      shortName = None,
      caption = None,
      includeIf = None,
      validators = None,
      fields = fields,
      continueLabel = None,
      continueIf = None,
      instruction = None,
      presentationHint = None,
      dataRetrieve = None,
      confirmation = None,
      redirects = None,
      hideSaveAndComeBackButton = None
    )
    val enterAddressSection = Section.NonRepeatingPage(page)
    val syntheticFormTemplate = cache.formTemplate.copy(formKind =
      cache.formTemplate.formKind
        .fold[FormKind](classic => classic.copy(sections = List(enterAddressSection)))(taskList =>
          taskList.copy(sections =
            taskList.sections.map(taskSection =>
              taskSection.copy(tasks = taskSection.tasks.map(_.copy(sections = NonEmptyList.of(enterAddressSection))))
            )
          )
        )
    )

    val formData =
      cache.form.thirdPartyData
        .enteredAddressDataForWithFallback(syntheticFormComponent.id)
        .getOrElse(FormData(List.empty))

    cache
      .copy(formTemplateContext = FormTemplateContext.basicContext(syntheticFormTemplate, None))
      .copy(form = cache.form.copy(formData = formData))
  }

  private def mkSyntheticFormModelOptics(
    formComponentId: FormComponentId,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode]
  )(f: FormComponent => AuthCacheWithForm => FormModelOptics[DataOrigin.Mongo] => Future[Result])(implicit
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier
  ): Future[Result] = {

    val syntheticFormComponent = mkSyntheticFormComponent(formComponentId)
    val syntheticCache = mkSyntheticCache(cache, syntheticFormComponent)

    val formModelBuilder = new FormModelBuilder[Throwable, Future](
      syntheticCache.retrievals,
      syntheticCache.formTemplate,
      syntheticCache.form.thirdPartyData,
      syntheticCache.form.envelopeId,
      maybeAccessCode,
      recalculation,
      FormComponentIdToFileIdMapping.empty,
      lookupOptions
    )

    val data = syntheticCache.variadicFormData[SectionSelectorType.Normal]

    val formModelVisibilityOpticsF: Future[FormModelVisibilityOptics[DataOrigin.Mongo]] =
      formModelBuilder.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

    formModelVisibilityOpticsF
      .map { formModelVisibilityOptics =>
        formModelBuilder
          .renderPageModel[DataOrigin.Mongo, SectionSelectorType.Normal](formModelVisibilityOptics, None)
      }
      .flatMap(f(syntheticFormComponent)(syntheticCache))
  }
}
