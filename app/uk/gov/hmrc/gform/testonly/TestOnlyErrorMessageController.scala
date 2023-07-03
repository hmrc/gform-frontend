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

import play.api.libs.json.{ Json, OFormat }

import uk.gov.hmrc.gform.views.html
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
          else {
            val table = toTableCells(EnCyReport.makeEnCy(englishReport, welshReport), formTemplateId)
            val title = if (fullReport) "Full Error Report" else "Error Report"
            Ok(html.debug.errorReport(title, table)).as("text/html")
          }
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
    messages: List[String],
    validators: List[(String, String)]
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
        messages = messages,
        validators =
          formComponent.validators.map(s => (s.errorMessage.rawValue(LangADT.En), s.errorMessage.rawValue(LangADT.Cy)))
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
        messages = List.empty[String],
        validators = List.empty[(String, String)]
      )
  }

  case class EnCyReport(
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
    messages_Cy: List[String],
    validators: List[(String, String)]
  )

  object EnCyReport {

    implicit val format: OFormat[EnCyReport] = Json.format[EnCyReport]
    def make(reportEn: FieldErrorReport, reportCy: FieldErrorReport): EnCyReport =
      new EnCyReport(
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
        messages_En = if (reportEn.validators.isEmpty) reportEn.messages else reportEn.validators.map(_._1),
        messages_Cy = if (reportCy.validators.isEmpty) reportCy.messages else reportEn.validators.map(_._2),
        validators = reportEn.validators
      )
    def makeEnCy(reportEn: List[FieldErrorReport], reportCy: List[FieldErrorReport]): List[EnCyReport] = {
      val unionList = (reportEn ++ reportCy).groupBy(_.fieldId).map(_._2.head).toList

      unionList.map { element =>
        val en = reportEn.find(_.fieldId == element.fieldId).getOrElse(FieldErrorReport.makeBlank())
        val cy = reportCy.find(_.fieldId == element.fieldId).getOrElse(FieldErrorReport.makeBlank())
        EnCyReport.make(en, cy)
      }
    }
  }

  def toTableCells(enCyReports: List[EnCyReport], formTemplateId: FormTemplateId): List[List[(String, String, String, String)]] = {
    def reportToRows(enCyReport: EnCyReport): List[(String, String, String, String)] = {
      val validatorsLabelAndValues = enCyReport.validators.zipWithIndex.flatMap {
        case ((enValidator, cyValidator), index) =>
          List((s"validator $index En", enValidator), (s"validator $index Cy", cyValidator))
      }
      val labelsAndValues = List(
        ("fieldId", enCyReport.fieldId),
        ("label En", enCyReport.label_En),
        ("label Cy", enCyReport.label_Cy),
        ("shortName En", enCyReport.shortName_En),
        ("shortName Cy", enCyReport.shortName_Cy),
        ("errorShortName En", enCyReport.errorShortName_En),
        ("errorShortName Cy", enCyReport.errorShortName_Cy),
        ("errorShortNameStart En", enCyReport.errorShortNameStart_En),
        ("errorShortNameStart Cy", enCyReport.errorShortNameStart_Cy),
        ("errorExample En", enCyReport.errorExample_En),
        ("errorExample Cy", enCyReport.errorExample_Cy),
        ("errorMessage En", enCyReport.errorMessage_En),
        ("errorMessage Cy", enCyReport.errorMessage_Cy)
      ) ++ validatorsLabelAndValues

      val messagesCombined = enCyReport.messages_En.zipAll(enCyReport.messages_Cy, "", "")

      val combined = labelsAndValues.zipAll(messagesCombined, ("", ""), ("", ""))

      combined.map { case ((label, value), (enMessage, cyMessage)) =>
        (label, value, enMessage, cyMessage)
      }
    }
    enCyReports.map(reportToRows)
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
