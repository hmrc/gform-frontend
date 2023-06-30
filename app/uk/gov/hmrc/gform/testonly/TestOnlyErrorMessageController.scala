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

package uk.gov.hmrc.gform.testonly

import play.twirl.api.{ Html, HtmlFormat }
import play.api.libs.json.{ Json, OFormat }

import cats.implicits._
import play.api.mvc._

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping

import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.IsText
import play.api.i18n.I18nSupport
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.LangADT

class TestOnlyErrorMessageController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  controllerComponents: MessagesControllerComponents,
  validationReportService: ValidationService,
  validationFullReportService: ValidationService
)(implicit ec: ExecutionContext)
    extends FrontendController(controllerComponents: MessagesControllerComponents) {

  def errorMessages(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    fullReport: Boolean,
    jsonReport: Boolean
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        import play.api.i18n._
        val validationService =
          if (fullReport)
            validationFullReportService
          else
            validationReportService

        val messagesApi: MessagesApi = controllerComponents.messagesApi
        val englishMessages: Messages = messagesApi.preferred(Seq(Lang("en")))
        val welshMessages: Messages = messagesApi.preferred(Seq(Lang("cy")))
        for {
          envelopeWithMapping <- Future.successful(EnvelopeWithMapping.empty)
          englishValidationResult <-
            validationService
              .validateFormModel(
                cache.toCacheData,
                envelopeWithMapping,
                formModelOptics.formModelVisibilityOptics,
                None
              )(implicitly[HeaderCarrier], englishMessages, l, sse)
          welshValidationResult <-
            validationService
              .validateFormModel(
                cache.toCacheData,
                envelopeWithMapping,
                formModelOptics.formModelVisibilityOptics,
                None
              )(implicitly[HeaderCarrier], welshMessages, l, sse)
          englishReport = reports(englishValidationResult.formFieldValidationResults)(LangADT.En)
          welshReport = reports(welshValidationResult.formFieldValidationResults)(LangADT.Cy)
        } yield
          if (jsonReport)
            Ok(Json.toJson(englishReport ++ welshReport)).as("application/json")
          else
            Ok(toHtml(FieldErrorAllReport.makeEnCy(englishReport, welshReport), formTemplateId)).as("text/html")
    }

  private def reports(
    listValidation: List[FormFieldValidationResult]
  )(implicit l: LangADT): List[FieldErrorReport] =
    listValidation
      .filter(_.isNotOk)
      .flatMap { formFieldValidationResults =>
        val formComponent = formFieldValidationResults.formComponent
        formFieldValidationResults match {
          case ComponentField(_, data) =>
            data.map { case (key, value) =>
              FieldErrorReport.make(key.toHtmlId, formComponent, value.fieldErrors.toList)
            }
          case otherwise =>
            List(FieldErrorReport.make(formComponent.id.value, formComponent, otherwise.fieldErrors.toList))
        }
      }

  case class FieldErrorReport(
    fieldId: String,
    errorMessageType: String,
    debugInfo: String,
    label: String,
    shortName: String,
    errorShortName: String,
    errorShortNameStart: String,
    errorExample: String,
    errorMessage: String,
    messages: List[String]
  )

  object FieldErrorReport {
    implicit val format: OFormat[FieldErrorReport] = Json.format[FieldErrorReport]
    def make(fieldId: String, formComponent: FormComponent, messages: List[String])(implicit l: LangADT) =
      FieldErrorReport(
        fieldId = fieldId,
        errorMessageType = "Dynamic",
        debugInfo = (formComponent match {
          case IsText(t) => t.constraint.getClass().toString
          case othewise  => othewise.toString
        }) + "mandatory: " + formComponent.mandatory.toString,
        label = formComponent.label.rawValue,
        shortName = formComponent.shortName.map(_.rawValue).getOrElse(""),
        errorShortName = formComponent.errorShortName.map(_.rawValue).getOrElse(""),
        errorShortNameStart = formComponent.errorShortNameStart.map(_.rawValue).getOrElse(""),
        errorExample = formComponent.errorExample.map(_.rawValue).getOrElse(""),
        errorMessage = formComponent.errorMessage.map(_.rawValue).getOrElse(""),
        messages = messages
      )

    def makeBlank(): FieldErrorReport =
      new FieldErrorReport(
        fieldId = "",
        errorMessageType = "",
        debugInfo = "",
        label = "",
        shortName = "",
        errorShortName = "",
        errorShortNameStart = "",
        errorExample = "",
        errorMessage = "",
        messages = List.empty[String]
      )
  }

  case class FieldErrorAllReport(
    fieldId: String,
    errorMessageType: String,
    debugInfo: String,
    label_En: String,
    label_Cy: String,
    shortName_En: String,
    shortName_Cy: String,
    errorShortName_En: String,
    errorShortName_Cy: String,
    errorShortNameStart_En: String,
    errorShortNameStart_Cy: String,
    errorExample_En: String,
    errorExample_Cy: String,
    errorMessage_En: String,
    errorMessage_Cy: String,
    messages_En: List[String],
    messages_Cy: List[String]
  )

  object FieldErrorAllReport {

    implicit val format: OFormat[FieldErrorAllReport] = Json.format[FieldErrorAllReport]
    def make(reportEn: FieldErrorReport, reportCy: FieldErrorReport): FieldErrorAllReport =
      new FieldErrorAllReport(
        fieldId = reportEn.fieldId,
        errorMessageType = reportEn.errorMessageType,
        debugInfo = reportEn.debugInfo,
        label_En = reportEn.label,
        label_Cy = reportCy.label,
        shortName_En = reportEn.shortName,
        shortName_Cy = reportCy.shortName,
        errorShortName_En = reportEn.errorShortName,
        errorShortName_Cy = reportCy.errorShortName,
        errorShortNameStart_En = reportEn.errorShortNameStart,
        errorShortNameStart_Cy = reportCy.errorShortNameStart,
        errorExample_En = reportEn.errorExample,
        errorExample_Cy = reportCy.errorExample,
        errorMessage_En = reportEn.errorMessage,
        errorMessage_Cy = reportCy.errorMessage,
        messages_En = reportEn.messages,
        messages_Cy = reportCy.messages
      )
    def htmlEscape(report: FieldErrorAllReport): FieldErrorAllReport =
      new FieldErrorAllReport(
        fieldId = HtmlFormat.escape(report.fieldId).toString,
        errorMessageType = HtmlFormat.escape(report.errorMessageType).toString,
        debugInfo = HtmlFormat.escape(report.debugInfo).toString,
        label_En = HtmlFormat.escape(report.label_En).toString,
        label_Cy = HtmlFormat.escape(report.label_Cy).toString,
        shortName_En = HtmlFormat.escape(report.shortName_En).toString,
        shortName_Cy = HtmlFormat.escape(report.shortName_Cy).toString,
        errorShortName_En = HtmlFormat.escape(report.errorShortName_En).toString,
        errorShortName_Cy = HtmlFormat.escape(report.errorShortName_Cy).toString,
        errorShortNameStart_En = HtmlFormat.escape(report.errorShortNameStart_En).toString,
        errorShortNameStart_Cy = HtmlFormat.escape(report.errorShortNameStart_Cy).toString,
        errorExample_En = HtmlFormat.escape(report.errorExample_En).toString,
        errorExample_Cy = HtmlFormat.escape(report.errorExample_Cy).toString,
        errorMessage_En = HtmlFormat.escape(report.errorMessage_En).toString,
        errorMessage_Cy = HtmlFormat.escape(report.errorMessage_Cy).toString,
        messages_En = report.messages_En.map(m => HtmlFormat.escape(m).toString),
        messages_Cy = report.messages_Cy.map(HtmlFormat.escape(_).toString)
      )
    def makeEnCy(reportEn: List[FieldErrorReport], reportCy: List[FieldErrorReport]): List[FieldErrorAllReport] = {
      val unionList = (reportEn ++ reportCy).groupBy(_.fieldId).map(_._2.head).toList

      unionList.map { element =>
        val en = reportEn.find(_.fieldId == element.fieldId).getOrElse(FieldErrorReport.makeBlank())
        val cy = reportCy.find(_.fieldId == element.fieldId).getOrElse(FieldErrorReport.makeBlank())
        FieldErrorAllReport.make(en, cy)
      }
    }
  }

  def toHtml(report: List[FieldErrorAllReport], formTemplateId: FormTemplateId) = {
    def reportToRows(report: FieldErrorAllReport): String = {
      val labelsAndValues = List(
        ("fieldId", report.fieldId),
        // ("errorMessageType", report.errorMessageType),
        ("label En", report.label_En),
        ("label Cy", report.label_Cy),
        ("shortName En", report.shortName_En),
        ("shortName Cy", report.shortName_Cy),
        ("errorShortName En", report.errorShortName_En),
        ("errorShortName Cy", report.errorShortName_Cy),
        ("errorShortNameStart En", report.errorShortNameStart_En),
        ("errorShortNameStart Cy", report.errorShortNameStart_Cy),
        ("errorExample En", report.errorExample_En),
        ("errorExample Cy", report.errorExample_Cy),
        ("errorMessage En", report.errorMessage_En),
        ("errorMessage Cy", report.errorMessage_Cy)
      )

      val messagesCombined = report.messages_En.zipAll(report.messages_Cy, "", "")

      val combined = labelsAndValues.zipAll(messagesCombined, ("", ""), ("", ""))

      def res(list: List[((String, String), (String, String))]) =
        list
          .map { case ((label, value), (enMessage, cyMessage)) =>
            s"""
               |<tr class="govuk-table__row">
               |  <th style="white-space: nowrap; text-align: left;" scope="row" class="govuk-table__header">$label</th>
               |  <td class="govuk-table__cell" >$value</td>
               |  <td style="color: #d4351c;" class="govuk-table__cell" >$enMessage</td>
               |  <td style="color: #d4351c;" class="govuk-table__cell" >$cyMessage</td>
               |</tr>
    """.stripMargin
          }
          .mkString("")

      res(combined) ++ s"""
                          |<tr>
                          |      <th scope="col" style="border:1px solid #b1b4b6;" class="govuk-table__header app-custom-class">Property</th>
                          |      <th scope="col" style="border:1px solid #b1b4b6;" class="govuk-table__header app-custom-class">Property value</th>
                          |      <th scope="col" style="border:1px solid #b1b4b6;" class="govuk-table__header app-custom-class">English error messages</th>
                          |      <th scope="col" style="border:1px solid #b1b4b6;" class="govuk-table__header app-custom-class">Welsh error messages</th>
                          |</tr>
    """.stripMargin
    }

    val tableRows = report.map(r => FieldErrorAllReport.htmlEscape(r)).flatMap(reportToRows).mkString("")
    // val tableRows = report.map(r => r).flatMap(reportToRows).mkString("")

    val r = s"""
               |<table class="govuk-table">
               |<caption class="govuk-table__caption govuk-table__caption--xl">Error messages for ${formTemplateId.value}</caption>
               | <thead class="govuk-table__head">
               |    <tr class="govuk-table__row">
               |      <th scope="col" style="border:1px solid #b1b4b6; white-space: nowrap;" class="govuk-table__header app-custom-class">Property</th>
               |      <th scope="col" style="border:1px solid #b1b4b6;" class="govuk-table__header app-custom-class">Property value</th>
               |      <th scope="col" style="border:1px solid #b1b4b6;" class="govuk-table__header app-custom-class">English error messages</th>
               |      <th scope="col" style="border:1px solid #b1b4b6;" class="govuk-table__header app-custom-class">Welsh error messages</th>
               |    </tr>
               |  </thead>
               |<tbody class="govuk-table__body">
               |  $tableRows
               |</tbody>
               |</table>
""".stripMargin

    Html(r)

  }

  def errorsJsonFull(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      _ => _ => _ => _ => _ =>
        Redirect(routes.TestOnlyErrorMessageController.errorMessages(formTemplateId, maybeAccessCode, true, true))
          .pure[Future]
    }
  def errorsJson(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      _ => _ => _ => _ => _ =>
        Redirect(routes.TestOnlyErrorMessageController.errorMessages(formTemplateId, maybeAccessCode, false, true))
          .pure[Future]
    }
  def errorsHtmlFull(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      _ => _ => _ => _ => _ =>
        Redirect(routes.TestOnlyErrorMessageController.errorMessages(formTemplateId, maybeAccessCode, true, false))
          .pure[Future]
    }
  def errorsHtml(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      _ => _ => _ => _ => _ =>
        Redirect(routes.TestOnlyErrorMessageController.errorMessages(formTemplateId, maybeAccessCode, false, false))
          .pure[Future]
    }
}
