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

import cats.data.NonEmptyList
import play.api.i18n.Messages
import play.api.mvc.Result
import play.api.mvc.Results.{ Ok, Redirect }
import uk.gov.hmrc.gform.auth.{ Identifier, Verifier }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.gform.{ EnrolmentFormNotValid, NoIdentifierProvided, SubmitEnrolmentError }
import uk.gov.hmrc.gform.gform.RegimeIdNotMatch
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EnrolmentOutcomes, EnrolmentSection, FormTemplate }
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.ErrorLink

class EnrolmentResultProcessor(
  renderEnrolmentSection: RenderEnrolmentSection,
  formTemplate: FormTemplate,
  retrievals: MaterialisedRetrievals,
  enrolmentSection: EnrolmentSection,
  formModelOptics: FormModelOptics[DataOrigin.Mongo]
) {

  def recoverEnrolmentError(
    validationResult: ValidationResult
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
      Ok(
        renderEnrolmentSection(
          formTemplate,
          retrievals,
          enrolmentSection,
          formModelOptics,
          globalErrors,
          validationResultFinal
        )
      )
    }

  def processEnrolmentResult(enrolmentOutcomes: EnrolmentOutcomes)(
    authRes: CheckEnrolmentsResult
  ): Result =
    authRes match {
      case CheckEnrolmentsResult.Conflict(identifiers, verifiers) =>
        val params = toQueryParams(identifiers, verifiers)
        Redirect(
          uk.gov.hmrc.gform.gform.routes.EnrolmentController
            .alreadyLinkedPage(formTemplate._id, params)
        )

      case CheckEnrolmentsResult.Successful =>
        Redirect(uk.gov.hmrc.gform.gform.routes.EnrolmentController.successPage(formTemplate._id))
      case CheckEnrolmentsResult.InvalidIdentifiers | CheckEnrolmentsResult.InvalidCredentials =>
        Redirect(
          uk.gov.hmrc.gform.gform.routes.EnrolmentController
            .technicalFailurePage(formTemplate._id)
        )

      case CheckEnrolmentsResult.InsufficientEnrolments(identifiers, verifiers) =>
        val params = toQueryParams(identifiers, verifiers)
        Redirect(
          uk.gov.hmrc.gform.gform.routes.EnrolmentController
            .notMatchedPage(formTemplate._id, params)
        )

      case CheckEnrolmentsResult.Failed =>
        // Nothing we can do here, so technical difficulties it is.
        throw new Exception("Enrolment has failed. Most probable reason is enrolment service being unavailable")
    }

  private def toQueryParams(identifiers: NonEmptyList[Identifier], verifiers: List[Verifier]): String = {
    val identifiersParam = identifiers.map(i => i.key + "=" + i.value).toList
    val verifiersParam = verifiers.map(v => v.key + "=" + v.value)
    (identifiersParam ++ verifiersParam).mkString("~")
  }
}
