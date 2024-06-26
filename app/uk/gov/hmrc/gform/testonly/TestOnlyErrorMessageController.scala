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

import cats.data.Validated
import cats.data.Validated.Valid
import cats.implicits._
import play.api.i18n.I18nSupport
import play.api.i18n._
import play.api.libs.json.Json
import play.api.libs.json.OFormat
import play.api.mvc._
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.IsText
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.validation.ValidationUtil.GformError
import uk.gov.hmrc.gform.validation._
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.sharedmodel.SmartString

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class TestOnlyErrorMessageController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  controllerComponents: MessagesControllerComponents,
  validationReportService: ValidationService,
  lookupRegistry: LookupRegistry
)(implicit ec: ExecutionContext)
    extends FrontendController(controllerComponents: MessagesControllerComponents) {

  def errorMessages(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    jsonReport: Boolean,
    inputBaseComponentId: Option[String],
    isUsageReport: Boolean = false
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => _ => cache => _ => formModelOptics =>
        val messagesApi: MessagesApi = controllerComponents.messagesApi
        val englishMessages: Messages = messagesApi.preferred(Seq(Lang("en")))
        val welshMessages: Messages = messagesApi.preferred(Seq(Lang("cy")))
        val factory = new RealSmartStringEvaluatorFactory(englishMessages)

        val englishSse: SmartStringEvaluator =
          factory.apply(formModelOptics.formModelVisibilityOptics)(englishMessages, LangADT.En)
        val welshSse: SmartStringEvaluator =
          factory.apply(formModelOptics.formModelVisibilityOptics)(welshMessages, LangADT.Cy)
        val formComponents =
          formModelOptics.formModelRenderPageOptics.formModel.pages
            .flatMap(_.allFormComponents)
            .map(_.copy(includeIf = None))
            .map(fc =>
              if (!isUsageReport) fc
              else
                fc.copy(
                  errorShortName =
                    Some(SmartString.blank.transform(_ => "<b>errorShortName</b>", _ => "<b>errorShortName</b>")),
                  errorShortNameStart = Some(
                    SmartString.blank.transform(
                      _ => "<b>errorShortNameStart</b>",
                      _ => "<b>errorShortNameStart</b>"
                    )
                  ),
                  errorExample =
                    Some(SmartString.blank.transform(_ => "<b>errorExample</b>", _ => "<b>errorExample</b>"))
                )
            )
        for {
          englishReports <-
            fieldErrorReportsF(formComponents, formModelOptics, cache, inputBaseComponentId)(
              englishMessages,
              LangADT.En,
              englishSse
            )
          welshReports <-
            fieldErrorReportsF(formComponents, formModelOptics, cache, inputBaseComponentId)(
              welshMessages,
              LangADT.Cy,
              welshSse
            )
        } yield {
          val report = EnCyReport.makeEnCy(englishReports, welshReports)
          if (jsonReport)
            Ok(Json.toJson(report)).as("application/json")
          else {
            val table = toTableCells(report, formTemplateId)
            val title = s"Error Report for ${formTemplateId.value}"
            Ok(html.debug.errorReport(title, table)).as("text/html")
          }

        }
    }

  case class FieldErrorReport(
    fieldId: String,
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
        debugInfo = (formComponent match {
          case IsText(t) => t.constraint.getClass().toString
          case othewise  => othewise.toString
        }) + "mandatory: " + formComponent.mandatory.toString,
        label = formComponent.label.rawDefaultValue,
        shortName = formComponent.shortName.map(_.rawDefaultValue).getOrElse(""),
        errorShortName = formComponent.errorShortName.map(_.rawDefaultValue).getOrElse(""),
        errorShortNameStart = formComponent.errorShortNameStart.map(_.rawDefaultValue).getOrElse(""),
        errorExample = formComponent.errorExample.map(_.rawDefaultValue).getOrElse(""),
        errorMessage = formComponent.errorMessage.map(_.rawDefaultValue).getOrElse(""),
        messages = messages,
        validators = formComponent.validators.map(s =>
          (
            s.errorMessage.rawDefaultValue(LangADT.En),
            s.errorMessage.rawDefaultValue(LangADT.Cy)
          )
        )
      )

    def make(formComponent: FormComponent, gformError: GformError, inputBaseComponentId: Option[String])(implicit
      l: LangADT
    ): List[FieldErrorReport] =
      gformError
        .filter { case (id, _) => inputBaseComponentId.forall(id.baseComponentId.value == _) }
        .map { case (id, errors) => make(id.toMongoIdentifier, formComponent, errors.toList) }
        .toList

    def makeBlank(): FieldErrorReport =
      new FieldErrorReport(
        fieldId = "",
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
        messages_En =
          if (reportEn.validators.isEmpty) reportEn.messages else reportEn.messages ++ reportEn.validators.map(_._1),
        messages_Cy =
          if (reportCy.validators.isEmpty) reportCy.messages else reportCy.messages ++ reportCy.validators.map(_._2),
        validators = reportEn.validators
      )
    def makeEnCy(reportEn: List[FieldErrorReport], reportCy: List[FieldErrorReport]): List[EnCyReport] = {
      val unionList = (reportEn ++ reportCy).groupBy(_.fieldId).map(_._2.head).toList

      unionList
        .map { element =>
          val en = reportEn.find(_.fieldId == element.fieldId).getOrElse(FieldErrorReport.makeBlank())
          val cy = reportCy.find(_.fieldId == element.fieldId).getOrElse(FieldErrorReport.makeBlank())
          EnCyReport.make(en, cy)
        }
        .sortBy(_.fieldId)
    }
  }

  def toTableCells(
    enCyReports: List[EnCyReport],
    formTemplateId: FormTemplateId
  ): List[List[(String, String, String, String)]] = {
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

  def errorsJson(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      _ => _ => _ => _ => _ =>
        Redirect(
          routes.TestOnlyErrorMessageController.errorMessages(formTemplateId, maybeAccessCode, true, None, false)
        )
          .pure[Future]
    }

  def errorsHtml(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      _ => _ => _ => _ => _ =>
        Redirect(
          routes.TestOnlyErrorMessageController.errorMessages(formTemplateId, maybeAccessCode, false, None, false)
        )
          .pure[Future]
    }

  def errorsUsageHtml(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      _ => _ => _ => _ => _ =>
        Redirect(
          routes.TestOnlyErrorMessageController.errorMessages(formTemplateId, maybeAccessCode, false, None, true)
        )
          .pure[Future]
    }

  private def fieldErrorReportsF(
    formComponents: List[FormComponent],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    cache: AuthCacheWithForm,
    inputBaseComponentId: Option[String]
  )(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator): Future[List[FieldErrorReport]] =
    formComponents
      .map { formComponent =>
        val formModel = formModelOptics.formModelVisibilityOptics.formModel
        new ComponentsValidator[DataOrigin.Mongo, Future](
          formModelOptics.formModelVisibilityOptics,
          formComponent,
          cache.toCacheData,
          EnvelopeWithMapping.empty,
          lookupRegistry,
          new BooleanExprEval(),
          ComponentChecker.ErrorReportInterpreter,
          true
        ).validate(GetEmailCodeFieldMatcher(formModel)).map {
          case Valid(a)                      => (formComponent, GformError.emptyGformError)
          case Validated.Invalid(gformError) => (formComponent, gformError)
        }
      }
      .sequence
      .map(_.flatMap { case (formComponent, gfromError) =>
        FieldErrorReport.make(formComponent, gfromError, inputBaseComponentId)
      })
}
