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

package uk.gov.hmrc.gform.gform.processor

import play.api.i18n.Messages
import play.api.mvc.{ AnyContent, Request, Result }
import play.api.mvc.Results.{ Ok, Redirect }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.gform.{ EnrolmentFormNotValid, NoIdentifierProvided, SubmitEnrolmentError }
import uk.gov.hmrc.gform.gform.RegimeIdNotMatch
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.notificationbanner.NotificationBanner
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EnrolmentSection, FormTemplate }
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.ErrorLink

class EnrolmentResultProcessor(
  renderEnrolmentSection: RenderEnrolmentSection,
  formTemplate: FormTemplate,
  retrievals: MaterialisedRetrievals,
  enrolmentSection: EnrolmentSection,
  formModelOptics: FormModelOptics[DataOrigin.Mongo],
  frontendAppConfig: FrontendAppConfig
) {

  private def getResult(
    validationResult: ValidationResult,
    globalErrors: List[ErrorLink],
    notificatioBanner: Option[NotificationBanner]
  ): Result =
    Ok(
      renderEnrolmentSection(
        formTemplate,
        retrievals,
        enrolmentSection,
        formModelOptics,
        globalErrors,
        validationResult,
        notificatioBanner
      )
    )

  def recoverEnrolmentError(
    validationResult: ValidationResult,
    notificatioBanner: Option[NotificationBanner]
  )(implicit messages: Messages): SubmitEnrolmentError => Result =
    enrolmentError => {

      def convertEnrolmentError(see: SubmitEnrolmentError): (ValidationResult, List[ErrorLink]) =
        see match {
          case RegimeIdNotMatch(identifierRecipe) =>
            val regimeIdError = messages("enrolment.error.regimeId")
            val validationResultUpd = validationResult.toError(identifierRecipe.value.formComponentId, regimeIdError)
            (validationResultUpd, List.empty)
          case NoIdentifierProvided =>
            val globalError = ErrorLink(
              content = content.Text(messages("enrolment.error.missingIdentifier"))
            )
            (ValidationResult.empty, globalError :: Nil)
          case EnrolmentFormNotValid =>
            (validationResult, List.empty)
        }
      val (validationResultFinal, globalErrors) = convertEnrolmentError(enrolmentError)
      getResult(validationResultFinal, globalErrors, notificatioBanner)
    }

  def processEnrolmentResult(notificatioBanner: Option[NotificationBanner])(
    authRes: CheckEnrolmentsResult
  )(implicit request: Request[AnyContent], messages: Messages, l: LangADT): Result =
    authRes match {
      case CheckEnrolmentsResult.Conflict =>
        Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.error_enrolment_conflict(formTemplate, frontendAppConfig))
      case CheckEnrolmentsResult.Successful =>
        Redirect(uk.gov.hmrc.gform.gform.routes.NewFormController.dashboard(formTemplate._id).url)
      case CheckEnrolmentsResult.InvalidIdentifiers | CheckEnrolmentsResult.InvalidCredentials |
          CheckEnrolmentsResult.InsufficientEnrolments =>
        val globalError: ErrorLink = ErrorLink(
          content = content.HtmlContent(html.form.errors.error_global_enrolment(formTemplate._id))
        )

        val globalErrors = globalError :: Nil
        val validationResult = ValidationResult.empty
        getResult(validationResult, globalErrors, notificatioBanner)
      case CheckEnrolmentsResult.Failed =>
        // Nothing we can do here, so technical difficulties it is.
        throw new Exception("Enrolment has failed. Most probable reason is enrolment service being unavailable")
    }
}
