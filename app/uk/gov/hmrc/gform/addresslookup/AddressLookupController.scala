/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, Request, Result }
import play.twirl.api.Html
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FastForward, SectionSelectorType }
import uk.gov.hmrc.gform.monoidHtml
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId, SectionNumber, SectionTitle4Ga, SuppressErrors }
import uk.gov.hmrc.gform.views.html.addresslookup
import uk.gov.hmrc.gform.views.html.hardcoded.pages.br
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class AddressLookupController(
  auth: AuthenticatedRequestActions,
  addressLookupService: AddressLookupService[Future],
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  def chooseAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    maybeSectionNumber: Option[SectionNumber]
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
            addressLookupResult.addresses
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
                        sectionNumber,
                        maybeSectionNumber
                      ),
                    backLink(cache, formModelOptics, sectionNumber, maybeAccessCode)
                  )
                )
              ) { addressRecords =>
                Ok(
                  renderChooseAddressPage(
                    formComponentId,
                    addressIdForm,
                    addressRecords,
                    cache,
                    formModelOptics,
                    sectionNumber,
                    maybeAccessCode,
                    maybeSectionNumber
                  )
                )
              }
              .pure[Future]
          }
    }

  private val addressIdForm: play.api.data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "addressId" -> play.api.data.Forms.nonEmptyText
    )
  )

  private def backLink(
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode]
  ) = {

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
      FastForward.Yes
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
    maybeSectionNumber: Option[SectionNumber]
  )(implicit request: Request[AnyContent], l: LangADT, sse: SmartStringEvaluator): Html = {
    val addressSelectioPage = new AddressSelectionPage(
      addressRecords,
      form,
      cache.form.thirdPartyData.addressSelectionFor(formComponentId)
    )

    val backHref = backLink(cache, formModelOptics, sectionNumber, maybeAccessCode)

    addresslookup.choose_address(
      cache.formTemplate,
      frontendAppConfig,
      addressSelectioPage,
      routes.AddressLookupController
        .submitAddress(cache.formTemplate._id, maybeAccessCode, formComponentId, sectionNumber, maybeSectionNumber),
      backHref
    )
  }

  def submitAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    maybeSectionNumber: Option[SectionNumber]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        addressIdForm.bindFromRequest
          .fold(
            errorForm =>
              cache.form.thirdPartyData.postcodeLookup
                .flatMap(_.get(formComponentId))
                .flatMap(_.addresses)
                .fold(
                  throw new NotFoundException(s"No addresses found for FormComponentId: ${formComponentId.value}.")
                ) { addressRecords =>
                  BadRequest(
                    renderChooseAddressPage(
                      formComponentId,
                      errorForm,
                      addressRecords,
                      cache,
                      formModelOptics,
                      sectionNumber,
                      maybeAccessCode,
                      maybeSectionNumber
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
                        sectionNumber,
                        maybeSectionNumber
                      )
                  )
                )
          )

    }

  def confirmAddress(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    maybeSectionNumber: Option[SectionNumber]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        cache.form.thirdPartyData.addressFor(formComponentId) match {
          case None => BadRequest("TRY AGAIN").pure[Future]
          case Some(addressRecord) =>
            val backHref =
              routes.AddressLookupController
                .chooseAddress(formTemplateId, maybeAccessCode, formComponentId, sectionNumber, maybeSectionNumber)

            import addressRecord.address._
            val address = List(line1, line2, line3, line4, town, postcode)
              .filter(_.nonEmpty)
              .map(addresslookup.address_line(_))
              .intercalate(br())

            val confirmAddressAndContinue = routes.AddressLookupController
              .confirmAddressAndContinue(
                formTemplateId,
                maybeAccessCode,
                formComponentId,
                sectionNumber,
                maybeSectionNumber
              )
            Ok(
              addresslookup
                .review_and_confirm_address(
                  cache.formTemplate,
                  frontendAppConfig,
                  address,
                  confirmAddressAndContinue,
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
    sectionNumber: SectionNumber,
    maybeSectionNumber: Option[SectionNumber]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => l => cache => sse => formModelOptics =>
        addressLookupService
          .cleanAddress(cache.form, maybeAccessCode, formComponentId)
          .as(
            Redirect(backLink(cache, formModelOptics, sectionNumber, maybeAccessCode))
          )
    }

  def confirmAddressAndContinue(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    maybeSectionNumber: Option[SectionNumber]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      request => l => cache => sse => formModelOptics =>
        maybeSectionNumber match {
          case None =>
            Redirect(
              uk.gov.hmrc.gform.gform.routes.SummaryController.summaryById(cache.formTemplateId, maybeAccessCode)
            ).pure[Future]
          case Some(sn) => Redirect(backLink(cache, formModelOptics, sn, maybeAccessCode)).pure[Future]
        }
    }
}
