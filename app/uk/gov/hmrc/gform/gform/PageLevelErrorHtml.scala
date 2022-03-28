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

package uk.gov.hmrc.gform.gform

import cats.data.NonEmptyList
import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, IsAddress, IsChoice, IsOverseasAddress, IsRevealingChoice, OverseasAddress }
import uk.gov.hmrc.gform.validation.{ ComponentField, FormFieldValidationResult, HtmlFieldId }
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }

object PageLevelErrorHtml {
  def generatePageLevelErrorHtml(
    listValidation: List[FormFieldValidationResult],
    globalErrors: List[ErrorLink]
  )(implicit
    messages: Messages
  ): HasErrors = {

    val errorsHtml: List[ErrorLink] = globalErrors ++ listValidation
      .filter(_.isNotOk)
      .flatMap { formFieldValidationResult =>
        formFieldValidationResult match {
          case ComponentField(formComponent @ IsAddress(_), data) =>
            addressFieldSorted(Address.fields(formComponent.modelComponentId.indexedComponentId), data)
          case ComponentField(formComponent @ IsOverseasAddress(overseasAddress), data) =>
            addressFieldSorted(OverseasAddress.fields(formComponent.modelComponentId.indexedComponentId), data)
          case ComponentField(_, data) => data.toList.collectFirst(toErrorLink)
          case otherwise =>
            otherwise.fieldErrors
              .map { errorMessage =>
                val formComponent = otherwise.formComponent
                val multiFieldId =
                  otherwise.formComponent match {
                    case IsChoice(_) | IsRevealingChoice(_) => HtmlFieldId.indexed(formComponent.id, "0")
                    case _                                  => HtmlFieldId.pure(formComponent.modelComponentId)
                  }
                ErrorLink(
                  href = Some("#" + multiFieldId.toHtmlId),
                  content = content.Text(errorMessage)
                )
              }
        }
      }

    if (errorsHtml.nonEmpty) {

      val errorSummary = ErrorSummary(
        errorList = errorsHtml,
        title = content.Text(messages("error.summary.heading"))
      )

      val errorHtml: Html = new components.GovukErrorSummary()(errorSummary)

      Errors(errorHtml)
    } else
      NoErrors
  }

  def noJSFileUploadError(message: String, fileId: Option[String])(implicit messages: Messages): HasErrors = {
    val errorSummary = ErrorSummary(
      errorList = List(
        ErrorLink(href = fileId.map("#" + _), content = content.Text(message))
      ),
      title = content.Text(messages("error.summary.heading"))
    )

    val errorHtml: Html = new components.GovukErrorSummary()(errorSummary)

    Errors(errorHtml)
  }

  private val toErrorLink: PartialFunction[(HtmlFieldId, FormFieldValidationResult), ErrorLink] = {
    case (multiFieldId, ffvr) if ffvr.isNotOk =>
      ErrorLink(
        href = Some("#" + multiFieldId.toHtmlId),
        content = content.Text(ffvr.fieldErrors.headOption.getOrElse(""))
      )
  }
  private def addressFieldSorted(
    fields: NonEmptyList[ModelComponentId.Atomic],
    data: Map[HtmlFieldId, FormFieldValidationResult]
  ) =
    // We need to sort errors based on elements position on screen
    fields.toList
      .flatMap { modelComponentId =>
        val multiFieldId = HtmlFieldId.pure(modelComponentId)
        data.get(multiFieldId).map(multiFieldId -> _)
      }
      .collect(toErrorLink)

}

sealed trait HasErrors {

  def hasErrors: Boolean = this match {
    case Errors(_) => true
    case NoErrors  => false
  }

  def render: Html = this match {
    case Errors(html) => html
    case NoErrors     => Html("")
  }
}

case object NoErrors extends HasErrors
case class Errors(html: Html) extends HasErrors
