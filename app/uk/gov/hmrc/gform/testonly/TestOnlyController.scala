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

import cats.data.EitherT
import cats.data.Validated.Valid
import cats.implicits._
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString
import play.api.i18n.{ I18nSupport, Messages }
import play.api.libs.json.{ JsObject, JsValue, Json }
import play.api.mvc._
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2._
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.crypto.PlainText
import uk.gov.hmrc.gform.auth.models.{ MaterialisedRetrievals, OperationWithForm, OperationWithoutForm }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.gform.{ AcknowledgementPdfService, _ }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ SectionSelectorType, UserSession }
import uk.gov.hmrc.gform.objectStore.Attachments
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.SdesDestination.{ DataStore, DataStoreLegacy, Dms, HmrcIlluminate, InfoArchive }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, SdesDestination }
import uk.gov.hmrc.gform.testonly.snapshot.SnapshotForms._
import uk.gov.hmrc.gform.testonly.snapshot._
import uk.gov.hmrc.gform.views.html.debug.snippets.inputWrapper
import uk.gov.hmrc.gform.views.html.debug.{ toolbox, viewExpressions }
import uk.gov.hmrc.gform.views.html.formatInstant
import uk.gov.hmrc.gform.views.html.hardcoded.pages._
import uk.gov.hmrc.gform.views.html.summary.snippets.bulleted_list
import uk.gov.hmrc.gform.BuildInfo
import uk.gov.hmrc.gform.eval.ExpressionResult
import uk.gov.hmrc.govukfrontend.views.Aliases.{ InsetText, Label, SelectItem, TabItem, TabPanel, Tabs }
import uk.gov.hmrc.govukfrontend.views.html.components.{ GovukErrorMessage, GovukHint, GovukInsetText, GovukLabel, GovukSelect, GovukTable, GovukTabs }
import uk.gov.hmrc.govukfrontend.views.html.helpers.{ GovukFormGroup, GovukHintAndErrorMessage }
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.{ HtmlContent, Text }
import uk.gov.hmrc.govukfrontend.views.viewmodels.select.Select
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.{ HeadCell, Table, TableRow }
import uk.gov.hmrc.http.{ HeaderCarrier, SessionKeys }
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl.idFunctor
import uk.gov.hmrc.play.bootstrap.binders.{ OnlyRelative, RedirectUrl }
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCrypto
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import java.time.Instant
import scala.concurrent.{ ExecutionContext, Future }

class TestOnlyController(
  i18nSupport: I18nSupport,
  proxy: ProxyActions,
  gformConnector: GformConnector,
  lookupRegistry: LookupRegistry,
  auth: AuthenticatedRequestActions,
  servicesConfig: ServicesConfig,
  frontendAppConfig: FrontendAppConfig,
  controllerComponents: MessagesControllerComponents,
  newFormController: NewFormController,
  authLoginApiService: AuthLoginApiService,
  summaryController: SummaryController,
  acknowledgementPdfService: AcknowledgementPdfService,
  sessionCookieCrypto: SessionCookieCrypto,
  englishMessages: Messages
)(implicit ec: ExecutionContext)
    extends FrontendController(controllerComponents: MessagesControllerComponents) {

  def proxyToGform(path: String): Action[Source[ByteString, _]] = proxy(gformBaseUrl)(path)

  def proxyToFileUpload(path: String): Action[Source[ByteString, _]] = proxy(fileUploadBaseUrl)(path)

  def proxyToSave4Later(path: String): Action[Source[ByteString, _]] = proxy(save4Later)(path)

  def whatsInSession(): Action[AnyContent] = Action { implicit request =>
    Ok(Json.toJson(request.session.data))
  }

  def clearSession(): Action[AnyContent] = Action { request =>
    Ok("session cleared").withSession()
  }

  def config() = Action { r =>
    val result: JsValue = Json.parse(ConfigFactory.load().root().render(ConfigRenderOptions.concise()))
    Ok(result)
  }

  private lazy val gformBaseUrl = servicesConfig.baseUrl("gform")
  private lazy val fileUploadBaseUrl = servicesConfig.baseUrl("file-upload")
  private lazy val save4Later = servicesConfig.baseUrl("save4later")

  private def recov[A](f: Future[A])(errorMsg: String)(implicit ec: ExecutionContext): FOpt[A] =
    fromFutureOptA(f.map(Right.apply).recover { case e => Left(UnexpectedState(errorMsg + "\n" + e.getMessage)) })

  def handlebarModel(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import cache._
        import i18nSupport._
        val customerId =
          CustomerIdRecalculation
            .evaluateCustomerId[DataOrigin.Mongo, SectionSelectorType.WithAcknowledgement](
              cache,
              formModelOptics.formModelVisibilityOptics
            )

        withHandlebarPayload {
          for {
            userSession <- fromFutureA(UserSessionBuilder[Future](cache))
            res <-
              fetchHandlebarModel(
                form,
                formTemplate,
                formModelOptics.formModelVisibilityOptics,
                customerId,
                retrievals,
                userSession
              )
          } yield res
        }
    }

  private def fetchHandlebarModel(
    form: Form,
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    customerId: CustomerId,
    retrievals: MaterialisedRetrievals,
    userSession: UserSession
  )(implicit
    l: LangADT,
    m: Messages,
    hc: HeaderCarrier
  ): EitherT[Future, UnexpectedState, Result] = {

    val emailParameters = EmailParametersRecalculated(Map.empty)
    val affinityGroup = AffinityGroupUtil.fromRetrievals(retrievals)

    for {
      structuredFormData <- fromFutureA(
                              StructuredFormDataBuilder[DataOrigin.Mongo, Future](
                                formModelVisibilityOptics,
                                formTemplate.destinations,
                                formTemplate.expressionsOutput,
                                lookupRegistry
                              )
                            )

      submissionData = SubmissionData(
                         PdfContent("htmlForPDF"),
                         None,
                         FrontEndSubmissionVariablesBuilder(
                           retrievals,
                           formTemplate,
                           formModelVisibilityOptics,
                           customerId
                         ),
                         structuredFormData,
                         emailParameters,
                         Attachments.empty,
                         l,
                         None,
                         DestinationEvaluator(formTemplate, formModelVisibilityOptics),
                         userSession
                       )

      httpResponse <- recov(
                        gformConnector
                          .renderHandlebarModel(
                            formTemplate._id,
                            form._id,
                            customerId,
                            submissionData,
                            affinityGroup
                          )
                      )("Error when calling gform service.")

    } yield Ok(httpResponse.body)

  }

  private def formTab(
    formTemplateId: FormTemplateId,
    accessCode: Option[AccessCode],
    isFormBuilderEnabled: Boolean,
    isSpecimen: Boolean
  ): HtmlFormat.Appendable = {
    val returnToSummaryLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "Return to summary page",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.changeStateAndRedirectToCYA(formTemplateId, accessCode)
    )

    val viewFormDataLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View form data",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.getFormData(formTemplateId, accessCode)
    )

    val viewSourceJsonTemplateLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View source json template",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.proxyToGform("gform/formtemplates/" + formTemplateId.value)
    )

    val saveCurrentFormLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "Save current form",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.saveFormPage(formTemplateId, accessCode)
    )

    val restoreFormLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "Restore a form",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.getSnapshots(formTemplateId, accessCode, UserInputs())
    )

    val toggleSpecimen = if (isSpecimen) {
      uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
        "Start non specimen form",
        routes.NewFormController.dashboard(
          FormTemplateId(
            formTemplateId.value.replace("specimen-", "")
          )
        )
      )
    } else {
      uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
        "Start specimen form",
        routes.NewFormController.dashboard(FormTemplateId("specimen-" + formTemplateId.value))
      )
    }

    val toggleFormBuilder = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      if (isFormBuilderEnabled) "Disable form builder" else "Enable form builder",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.toggleFormBuilder(formTemplateId, accessCode)
    )

    val links =
      List(
        returnToSummaryLink,
        viewFormDataLink,
        viewSourceJsonTemplateLink,
        saveCurrentFormLink,
        restoreFormLink,
        toggleSpecimen,
        toggleFormBuilder
      )

    bulleted_list(links)
  }

  private def submittedDataTab(formTemplate: FormTemplate, accessCode: Option[AccessCode], envelopeId: EnvelopeId) = {

    def createDownloadContent(destination: SdesDestination) =
      uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
        s"Download files for ${destination.description}",
        uk.gov.hmrc.gform.testonly.routes.TestOnlyController
          .proxyToGform(s"gform/object-store/${destination.downloadPath}/envelopes/${envelopeId.value}")
      )

    val downloadContents = List(Dms, DataStore, DataStoreLegacy, HmrcIlluminate, InfoArchive).map(createDownloadContent)

    val dataStoreWorkItemLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View data-store-work-item entry",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .proxyToGform("gform/data-store-work-item/envelopeId/" + envelopeId.value)
    )

    val dmsWorkItemLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View dms-work-item entry",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .proxyToGform("gform/dms-work-item/envelopeId/" + envelopeId.value)
    )

    val viewSDESLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View sdes submission",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .proxyToGform("gform/sdes/envelopeId/" + envelopeId.value)
    )

    val links = downloadContents ++ List(dataStoreWorkItemLink, dmsWorkItemLink, viewSDESLink)
    bulleted_list(links)
  }

  private def developmentToolsTab(
    formTemplate: FormTemplate,
    accessCode: Option[AccessCode],
    envelopeId: EnvelopeId
  ) = {

    val envelope = inputWrapper("Envelope ID", envelopeId.value)

    val govukTable = formTemplate.destinations.fold { destinationList =>
      val ids: List[(DestinationId, String, Boolean)] = destinationList.destinations.collect {
        case d: Destination.DataStore         => (d.id, "hmrcIlluminate", d.convertSingleQuotes.getOrElse(false))
        case d: Destination.HandlebarsHttpApi => (d.id, "handlebarsHttpApi", d.convertSingleQuotes.getOrElse(false))
        case d: Destination.HmrcDms           => (d.id, "hmrcDms", d.convertSingleQuotes.getOrElse(false))
      }

      val rows: List[List[TableRow]] = ids.map { case (destinationId, destinationType, convertSingleQuotes) =>
        val processPayloadLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
          destinationId.id,
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .handlebarPayload(formTemplate._id, destinationId, accessCode)
        )
        val embeddedLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
          "Embedded",
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .handlebarPayloadEmbedded(formTemplate._id, destinationId, accessCode)
        )
        val sourceLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
          "Source",
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .handlebarPayloadSource(formTemplate._id, destinationId, accessCode)
        )
        List(
          TableRow(
            content = HtmlContent(processPayloadLink)
          ),
          TableRow(
            content = Text(destinationType)
          ),
          TableRow(
            content = HtmlContent(embeddedLink)
          ),
          TableRow(
            content = HtmlContent(sourceLink)
          ),
          TableRow(
            content = Text(convertSingleQuotes.toString)
          )
        )
      }

      val head =
        Seq(
          HeadCell(
            content = Text("Output json")
          ),
          HeadCell(
            content = Text("Destination type")
          ),
          HeadCell(
            content = Text("Embedded")
          ),
          HeadCell(
            content = Text("Source")
          ),
          HeadCell(
            content = Text("convertSingleQuotes")
          )
        )

      new GovukTable()(
        Table(
          caption = Some("Destinations with handlebar payload"),
          captionClasses = "govuk-table__caption--m",
          rows = rows,
          head = Some(head)
        )
      )
    }(_ => HtmlFormat.empty)

    val viewHandlebarModelLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View handlebar json model",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.handlebarModel(formTemplate._id, accessCode)
    )

    val viewSourceTemplateLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View source json template",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.proxyToGform("gform/formtemplates/" + formTemplate._id.value)
    )

    val viewInternalTemplateLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View internal json template ('convertSingleQuotes' has been applied to 'includeIf' etc.)",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .proxyToGform("gform/formtemplates/" + formTemplate._id.value + "/internal")
    )

    val viewEnvelopeFilesLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View envelope files",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.proxyToGform("gform/envelopes/" + envelopeId.value)
    )

    val viewUploadedFilesLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View the uploaded files from bucket",
      uk.gov.hmrc.gform.testonly.routes.ObjectStoreAdminController.objectStoreContent(envelopeId)
    )

    val viewTranslationLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View translation source",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .proxyToGform("gform/translation/" + formTemplate._id.value + "/debug")
    )

    val viewExpressionsLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View expressions graph",
      uk.gov.hmrc.gform.testonly.routes.DebugController
        .exprs(formTemplate._id)
    )

    val viewFormModelLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View form model",
      uk.gov.hmrc.gform.testonly.routes.DebugController
        .model(formTemplate._id)
    )

    val viewAllExpressionsLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View expressions",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.showExpressions(formTemplate._id, accessCode)
    )

    val links = List(
      viewHandlebarModelLink,
      viewSourceTemplateLink,
      viewInternalTemplateLink,
      viewEnvelopeFilesLink,
      viewUploadedFilesLink,
      viewTranslationLink,
      viewExpressionsLink,
      viewAllExpressionsLink,
      viewFormModelLink
    )

    val bulletedList = bulleted_list(links)

    val tableInset = new GovukInsetText()(
      InsetText(
        content = content.HtmlContent(
          HtmlFormat.fill(
            List(
              Html("Embedded - payload is embedded into json template itself ('convertSingleQuotes' has been applied)"),
              br(),
              Html("Source - original payload from *.hbs file")
            )
          )
        )
      )
    )

    HtmlFormat.fill(List(envelope, br(), bulletedList, br(), govukTable, tableInset))
  }

  def showExpressions(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]): Action[AnyContent] =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, accessCode) {
      implicit request => implicit langADT => cache => _ => formModelOptics =>
        import i18nSupport._

        showExpresionValues(formTemplateId, cache, formModelOptics)
    }

  private def showExpresionValues(
    formTemplateId: FormTemplateId,
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit request: Request[AnyContent], lang: LangADT, hc: HeaderCarrier, message: Messages): Future[Result] = {

    def extractExprMeta(expr: Expr): (String, String, String, String, String) = {
      val evaluated = formModelOptics.formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr)
      val staticTypeData = evaluated.typeInfo.staticTypeData

      val tcType = staticTypeData.textConstraint.fold("")(tc => tc.getClass.getSimpleName.stripSuffix("$"))

      val rounding = staticTypeData.textConstraint
        .collect {
          case Number(_, _, roundingMode, _)         => roundingMode.toString
          case PositiveNumber(_, _, roundingMode, _) => roundingMode.toString
          case Sterling(roundingMode, _)             => roundingMode.toString
        }
        .getOrElse("")

      val fractionalDigits = staticTypeData.textConstraint
        .collect {
          case Number(_, maxFractionalDigits, _, _)         => maxFractionalDigits.toString
          case PositiveNumber(_, maxFractionalDigits, _, _) => maxFractionalDigits.toString
        }
        .getOrElse("")

      val resultType = evaluated.expressionResult match {
        case ExpressionResult.Invalid(explanation)  => s"Invalid: $explanation"
        case ExpressionResult.NumberResult(_)       => "Number Result"
        case ExpressionResult.StringResult(_)       => "String Result"
        case ExpressionResult.DateResult(_)         => "Date Result"
        case ExpressionResult.TaxPeriodResult(_, _) => "Tax Period Result"
        case ExpressionResult.PeriodResult(_)       => "Period Result"
        case ExpressionResult.OptionResult(_)       => "Option Result"
        case ExpressionResult.ListResult(_)         => "List Result"
        case ExpressionResult.AddressResult(_)      => "Address Result"
        case _                                      => ""
      }
      val value = evaluated.stringRepresentation

      (tcType, rounding, fractionalDigits, resultType, value)
    }

    def buildExprRow(
      key: String,
      value: JsValue,
      maybeExpr: Option[Expr],
      maybeErrorMsg: Option[String]
    ): Seq[TableRow] = {
      val exprStr = value
        .asOpt[String]
        .orElse((value \ "value").asOpt[String])
        .getOrElse(Json.prettyPrint(value))

      val (tcType, rounding, fractionalDigits, resultType, valueStr) = maybeExpr
        .map(extractExprMeta)
        .getOrElse(
          maybeErrorMsg.fold(("not found", "not found", "not found", "not found", "not found"))(msg =>
            ("", "", "", "", s"Error evaluating expression: $msg")
          )
        )

      Seq(
        TableRow(HtmlContent(key)),
        TableRow(Text(exprStr)),
        TableRow(Text(tcType)),
        TableRow(Text(rounding)),
        TableRow(Text(fractionalDigits)),
        TableRow(Text(resultType)),
        TableRow(Text(valueStr))
      )
    }

    def buildBooleanExprRow(key: String, value: JsValue, maybeExpr: Option[BooleanExpr]): Seq[TableRow] = {
      val exprStr = value.asOpt[String].getOrElse(Json.prettyPrint(value))
      val resolved = maybeExpr
        .map(be => formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(be).toString)
        .getOrElse("not found")

      Seq(
        TableRow(HtmlContent(key)),
        TableRow(Text(exprStr)),
        TableRow(Text(resolved))
      )
    }

    for {
      exprsFromTemplate <- gformConnector.getExpressions(formTemplateId)
      rawTemplateJson   <- gformConnector.getFormTemplateRaw(formTemplateId)
    } yield rawTemplateJson.asOpt[JsObject].fold[Result](NotFound("Form template not found")) { json =>
      val expressions = (json \ "expressions").validate[JsObject].getOrElse(Json.obj())
      val booleanExpressions = (json \ "booleanExpressions").validate[JsObject].getOrElse(Json.obj())

      val expressionsTable = new GovukTable()(
        Table(
          caption = Some("expressions"),
          captionClasses = "govuk-table__caption--m",
          head = Some(
            Seq("Id", "Expression", "Type", "Round", "Fractional Digits", "Result Type", "Value")
              .map(label => HeadCell(Text(label)))
          ),
          rows = expressions.value.map { case (key, value) =>
            buildExprRow(
              key,
              value,
              exprsFromTemplate.expressions.get(ExpressionId(key)),
              exprsFromTemplate.errorMap.get(ExpressionId(key))
            )
          }.toSeq
        )
      )

      val fieldsTable = new GovukTable()(
        Table(
          caption = Some("fields"),
          captionClasses = "govuk-table__caption--m",
          head = Some(
            Seq("Id", "Value").map(label => HeadCell(Text(label)))
          ),
          rows = cache.form.formData.fields.map(f =>
            Seq(
              TableRow(Text(f.id.toFormComponentId.value)),
              TableRow(Text(f.value))
            )
          )
        )
      )

      val booleanExpressionsTable = new GovukTable()(
        Table(
          caption = Some("booleanExpressions"),
          captionClasses = "govuk-table__caption--m",
          head = Some(
            Seq("Id", "Expression", "Value").map(label => HeadCell(Text(label)))
          ),
          rows = booleanExpressions.value.map { case (key, value) =>
            buildBooleanExprRow(key, value, exprsFromTemplate.booleanExpressions.get(BooleanExprId(key)))
          }.toSeq
        )
      )

      Ok(
        viewExpressions(
          cache.formTemplate,
          expressionsTable,
          booleanExpressionsTable,
          fieldsTable,
          frontendAppConfig
        )
      )
    }
  }

  private def reportsTab(formTemplate: FormTemplate, accessCode: Option[AccessCode]) = {
    val viewFormTemplateDetailsLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View form template details",
      uk.gov.hmrc.gform.testonly.routes.FormTemplateExtractController.extract(formTemplate._id, accessCode)
    )

    val viewErrorReport = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View error report",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyErrorMessageController.errorsHtml(formTemplate._id, accessCode)
    )

    val viewErrorUsageReport = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View error usage report",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyErrorMessageController.errorsUsageHtml(formTemplate._id, accessCode)
    )

    val viewHtmlErrorsReport = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View HTML errors in form",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.validateFormHtml(formTemplate._id)
    )

    val links = List(viewFormTemplateDetailsLink, viewErrorReport, viewErrorUsageReport, viewHtmlErrorsReport)

    bulleted_list(links)
  }

  private def translationsTab(formTemplate: FormTemplate, accessCode: Option[AccessCode]) = {

    val showTranslationQuickLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "Translation tools...",
      uk.gov.hmrc.gform.testonly.routes.TranslationController
        .translationQuick(formTemplate._id, accessCode)
    )

    val showTranslationLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "Translation tools with debugging support... (can be slow for huge forms)",
      uk.gov.hmrc.gform.testonly.routes.TranslationController
        .translationDebug(formTemplate._id, accessCode)
    )

    val screenshotInstructions = HtmlContent(
      """<a class="govuk-link" target="_blank" href="https://github.com/hmrc/gform-capture-extension/blob/main/README.md">View instructions for screen capture extension</a>"""
    )
    val links = List(
      showTranslationQuickLink,
      showTranslationLink,
      screenshotInstructions.value
    )
    bulleted_list(links)
  }

  def handleToolbox(
    formTemplateId: FormTemplateId,
    accessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, accessCode) {
    implicit request => implicit lang => cache => _ => formModelOptics =>
      import i18nSupport._

      val isFormBuilderEnabled = request.cookies.get(CookieNames.formBuilderCookieName).isDefined
      val isSpecimen = cache.formTemplate.isSpecimen

      val govukTabs = new GovukTabs()(
        Tabs(
          items = Seq(
            TabItem(
              id = Some("form"),
              label = "Form",
              panel = TabPanel(
                content = HtmlContent(formTab(formTemplateId, accessCode, isFormBuilderEnabled, isSpecimen))
              )
            ),
            TabItem(
              id = Some("submitted-data"),
              label = "Submitted data",
              panel = TabPanel(
                content = HtmlContent(submittedDataTab(cache.formTemplate, accessCode, cache.form.envelopeId))
              )
            ),
            TabItem(
              id = Some("reports"),
              label = "Reports",
              panel = TabPanel(
                content = HtmlContent(reportsTab(cache.formTemplate, accessCode))
              )
            ),
            TabItem(
              id = Some("development-tools"),
              label = "Development tools",
              panel = TabPanel(
                content = HtmlContent(developmentToolsTab(cache.formTemplate, accessCode, cache.form.envelopeId))
              )
            ),
            TabItem(
              id = Some("translation-tools"),
              label = "Welsh translation",
              panel = TabPanel(
                content = HtmlContent(translationsTab(cache.formTemplate, accessCode))
              )
            )
          )
        )
      )

      Ok(toolbox(cache.formTemplate, cache.form.envelopeId, accessCode, frontendAppConfig, govukTabs)).pure[Future]

  }
  def handlebarPayload(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import cache._
        import i18nSupport._
        val customerId =
          CustomerIdRecalculation
            .evaluateCustomerId[DataOrigin.Mongo, SectionSelectorType.WithAcknowledgement](
              cache,
              formModelOptics.formModelVisibilityOptics
            )

        withHandlebarPayload {
          for {
            userSession <- fromFutureA(UserSessionBuilder[Future](cache))
            res <- fetchHandlebarPayload(
                     form,
                     formTemplate,
                     formModelOptics.formModelVisibilityOptics,
                     customerId,
                     destinationId,
                     retrievals,
                     userSession
                   )
          } yield res
        }
    }

  def handlebarPayloadEmbedded(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      request => lang => cache => _ => formModelOptics =>
        val res: Result = cache.formTemplate.destinations.fold { destinationList =>
          val ids: Option[Option[String]] = destinationList.destinations.collectFirst {
            case d: Destination.DataStore if d.id === destinationId         => d.payload
            case d: Destination.HandlebarsHttpApi if d.id === destinationId => d.payload
            case h: Destination.HmrcDms if h.id === destinationId           => h.payload
          }
          ids.flatten match {
            case None          => BadRequest(s"No payload found on destination $destinationId")
            case Some(payload) => Ok(payload)
          }
        }(_ => BadRequest("No destination with payload found"))

        res.pure[Future]
    }

  def handlebarPayloadSource(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => lang => cache => _ => formModelOptics =>
        gformConnector.handlebarPayloadSource(formTemplateId, destinationId).map(Ok(_))
    }

  private def withHandlebarPayload(
    eitherT: EitherT[Future, UnexpectedState, Result]
  ): Future[Result] =
    eitherT.fold(error => BadRequest(error.error), identity)

  private def fetchHandlebarPayload(
    form: Form,
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    customerId: CustomerId,
    destinationId: DestinationId,
    retrievals: MaterialisedRetrievals,
    userSession: UserSession
  )(implicit
    l: LangADT,
    m: Messages,
    hc: HeaderCarrier
  ): EitherT[Future, UnexpectedState, Result] = {

    val emailParameters = EmailParametersRecalculated(Map.empty)
    val affinityGroup = AffinityGroupUtil.fromRetrievals(retrievals)

    for {
      structuredFormData <- fromFutureA(
                              StructuredFormDataBuilder[DataOrigin.Mongo, Future](
                                formModelVisibilityOptics,
                                formTemplate.destinations,
                                formTemplate.expressionsOutput,
                                lookupRegistry
                              )
                            )

      submissionData = SubmissionData(
                         PdfContent("htmlForPDF"),
                         None,
                         FrontEndSubmissionVariablesBuilder(
                           retrievals,
                           formTemplate,
                           formModelVisibilityOptics,
                           customerId
                         ),
                         structuredFormData,
                         emailParameters,
                         Attachments.empty,
                         l,
                         None,
                         DestinationEvaluator(formTemplate, formModelVisibilityOptics),
                         userSession
                       )

      httpResponse <- recov(
                        gformConnector
                          .renderHandlebarPayload(
                            formTemplate._id,
                            form._id,
                            destinationId,
                            customerId,
                            submissionData,
                            affinityGroup
                          )
                      )("Error when calling gform service.")

    } yield Ok(httpResponse.body)

  }

  private val defaultRetrievals = Retrievals.credentials and
    Retrievals.allEnrolments and
    Retrievals.affinityGroup and
    Retrievals.groupIdentifier and
    Retrievals.nino and
    Retrievals.email and
    Retrievals.confidenceLevel and
    Retrievals.itmpName and
    Retrievals.itmpDateOfBirth and
    Retrievals.itmpAddress and
    Retrievals.credentialRole and
    Retrievals.credentialStrength and
    Retrievals.gatewayInformation and
    Retrievals.agentInformation

  private def fetchAuthData(implicit hc: HeaderCarrier): Future[Option[GovernmentGatewayFormData]] =
    auth
      .authorised(AuthProviders(AuthProvider.GovernmentGateway))
      .retrieve(defaultRetrievals) {
        case maybeCredentials ~
            enrolments ~
            maybeAffinityGroup ~
            maybeGroupIdentifier ~
            maybeNino ~
            maybeEmail ~
            confidenceLevel ~
            itmpName ~
            itmpDateOfBirth ~
            itmpAddress ~
            maybeCredentialRole ~
            credentialStrength ~
            maybeGatewayInformation ~
            agentInformation =>
          val authWizardData = GovernmentGatewayFormData()
            .withCredentials(maybeCredentials)
            .withEnrolments(enrolments)
            .withAffinityGroup(maybeAffinityGroup)
            .withGroupIdentifier(maybeGroupIdentifier)
            .withNino(maybeNino)
            .withEmail(maybeEmail)
            .withConfidenceLevel(confidenceLevel)
            .withItmpData(itmpName, itmpDateOfBirth, itmpAddress)
            .withAgent(agentInformation)
            .withCredentialRole(maybeCredentialRole)
            .withCredentialStrength(credentialStrength)
            .withGatewayToken(maybeGatewayInformation)
          Some(authWizardData).pure[Future]
      }
      .recover(_ => None)

  private def saveSnapshot(
    userData: SaveFormUserData,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  )(implicit
    hc: HeaderCarrier
  ) = {
    val currentFormId = FormId(userData.currentFormId)
    val description = userData.description
    for {
      authWizardData <- fetchAuthData
      saveRequest = SaveRequest(
                      currentFormId,
                      Description(description),
                      GformFrontendVersion(BuildInfo.version),
                      authWizardData,
                      maybeAccessCode
                    )
      snapshotOverview <- gformConnector.saveForm(saveRequest)
    } yield Redirect(
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.snapshotPage(
        formTemplateId,
        snapshotOverview.snapshotId,
        maybeAccessCode
      )
    )
  }

  def saveForm(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    Action.async { implicit request =>
      implicit val hc: HeaderCarrier =
        HeaderCarrierConverter.fromRequestAndSession(request, request.session)

      saveFormUserData
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest("save form errors ${formWithErrors.errorsAsJson}").pure[Future],
          userData => saveSnapshot(userData, formTemplateId, maybeAccessCode)
        )
    }

  def createSnapshot(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => lang => cache => _ => formModelOptics =>
        val userData = SaveFormUserData(
          "Quick save",
          cache.form._id.value
        )
        saveSnapshot(userData, formTemplateId, maybeAccessCode)
    }

  def updateSnapshot(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => implicit lang => cache => _ => formModelOptics =>
      import i18nSupport._
      updateSnapshotUserData
        .bindFromRequest()
        .fold(
          formWithErrors =>
            BadRequest(
              update_snapshot(
                cache.formTemplate,
                maybeAccessCode,
                frontendAppConfig,
                SnapshotId(formWithErrors("snapshotId").value.getOrElse("")),
                Description(formWithErrors("description").value.getOrElse("")),
                formWithErrors("formData").value.getOrElse(""),
                formWithErrors
              )
            ).pure[Future],
          userData => {
            val updateRequest =
              UpdateSnapshotRequest(
                SnapshotId(userData.snapshotId),
                Json.parse(userData.formData).as[JsObject],
                Description(userData.description)
              )
            for {
              snapshotOverview <- gformConnector.updateSnapshot(updateRequest)

            } yield Redirect(
              uk.gov.hmrc.gform.testonly.routes.TestOnlyController.snapshotPage(
                formTemplateId,
                snapshotOverview.snapshotId,
                maybeAccessCode
              )
            )
          }
        )
  }

  def saveFormPage(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => implicit lang => cache => _ => formModelOptics =>
      import i18nSupport._
      Future.successful(
        Ok(save_form_page(cache.formTemplate, maybeAccessCode, frontendAppConfig, cache.form._id.value))
      )
  }

  def snapshotLoadData(formTemplateId: FormTemplateId, snapshotId: SnapshotId, maybeAccessCode: Option[AccessCode]) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.DownloadSummaryPdf
    ) { implicit request: Request[AnyContent] => implicit l => cache => sse => formModelOptics =>
      val currentFormId = FormId(cache.retrievals, formTemplateId, maybeAccessCode)
      import i18nSupport._
      val updateRequest = UpdateFormDataRequest(snapshotId, currentFormId)
      for {
        updatedForm <- gformConnector.loadSnapshotData(updateRequest)
        res <- newFormController.redirectContinue[SectionSelectorType.Normal](
                 cache.toAuthCacheWithoutForm,
                 updatedForm,
                 maybeAccessCode,
                 request
               )
      } yield res
    }

  def snapshot(snapshotId: SnapshotId, useOriginalTemplate: Boolean) =
    controllerComponents.actionBuilder.async { implicit request =>
      for {
        restoredSnapshot <- gformConnector.snapshotOverview(snapshotId)
      } yield {
        val redirectUrl = uk.gov.hmrc.gform.testonly.routes.TestOnlyController
          .snapshotSwitch(snapshotId, useOriginalTemplate)
          .url
        val sessionCookieValue = restoredSnapshot.ggFormData.fold("anonymous")(_ => "hmrc")

        val cookieValue = sessionCookieCrypto.crypto.encrypt(PlainText(sessionCookieValue))

        // This is here to guarantee seamless switching between anonymous and hmrc snapshots!
        // We need to be sure to use correct xxxCookieCryptoFilter in SessionCookieDispatcherFilter
        // for following request.
        // Following request will create new session and it needs to be created with proper
        // xxxCookieCryptoFilter
        Redirect(redirectUrl)
          .withCookies(Cookie(CookieNames.authConfigCookieName, cookieValue.value, secure = true))
      }
    }

  def snapshotSwitch(snapshotId: SnapshotId, useOriginalTemplate: Boolean) =
    controllerComponents.actionBuilder.async { implicit request =>
      for {
        restoredSnapshot <- gformConnector.restoreForm(snapshotId, useOriginalTemplate)
        redirectUrl = uk.gov.hmrc.gform.testonly.routes.TestOnlyController
                        .restoreContinue(restoredSnapshot.formTemplateId, restoredSnapshot.accessCode)
                        .url
        result <- getSessionAndRedirect(restoredSnapshot, redirectUrl)
      } yield result
    }

  def restoreContinue(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => sse => formModelOptics =>
        import i18nSupport._
        newFormController.redirectContinue[SectionSelectorType.Normal](
          cache.toAuthCacheWithoutForm,
          cache.form,
          maybeAccessCode,
          request
        )
    }

  private def getSessionAndRedirect(
    restoredSnapshot: RestoredSnapshot,
    redirectUrl: String
  ): Future[Result] =
    restoredSnapshot.ggFormData match {
      case Some(ggFormData) =>
        authLoginApiService
          .createNewSession(ggFormData.withRedirectionUrl(redirectUrl))
      case None =>
        val userId = restoredSnapshot.formIdData.fold(_.userId)(_.userId).value

        Redirect(redirectUrl)
          .withSession(SessionKeys.sessionId -> userId)
          .pure[Future]
    }

  def updateSnapshotPage(
    formTemplateId: FormTemplateId,
    snapshotId: SnapshotId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => implicit lang => cache => _ => formModelOptics =>
      import i18nSupport._

      for {
        snapshotOverivew <- gformConnector.snapshotOverview(snapshotId)
      } yield Ok(
        update_snapshot(
          cache.formTemplate,
          maybeAccessCode,
          frontendAppConfig,
          snapshotId,
          snapshotOverivew.description,
          snapshotOverivew.formData.map(_.toString()).getOrElse(""),
          updateSnapshotUserData
        )
      )
  }

  def snapshotPage(
    formTemplateId: FormTemplateId,
    snapshotId: SnapshotId,
    maybeAccessCode: Option[AccessCode]
  ) =
    controllerComponents.actionBuilder.async { implicit request =>
      import i18nSupport._
      implicit val lang: LangADT = LangADT.En
      for {
        formTemplate     <- gformConnector.getFormTemplate(formTemplateId)
        snapshotOverivew <- gformConnector.snapshotOverview(snapshotId)
      } yield {
        val versions: Html = Html(
          s"""
             |gform: ${snapshotOverivew.gformVersion.value}</br>
             |gform-frontend: ${snapshotOverivew.gformFrontendVersion.value}
             |""".stripMargin
        )
        val shareUrl =
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .snapshot(snapshotId)
            .path

        val devUrl =
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .snapshot(snapshotId, useOriginalTemplate = true)
            .path

        val loadDataUrl =
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .snapshotLoadData(formTemplateId, snapshotId, maybeAccessCode)
            .path

        Ok(
          snapshot_page(
            formTemplate,
            maybeAccessCode,
            frontendAppConfig,
            snapshotId,
            snapshotOverivew,
            versions,
            Json.prettyPrint(snapshotOverivew.formData.getOrElse(Json.obj())),
            shareUrl,
            devUrl,
            loadDataUrl,
            Json.prettyPrint(snapshotOverivew.ggFormData.map(Json.toJson(_)).getOrElse(Json.obj()))
          )
        )
      }
    }

  private val enableFormBuilderCookie = Cookie(CookieNames.formBuilderCookieName, "enabled", secure = true)
  private val discardFormBuilderCookie = DiscardingCookie(CookieNames.formBuilderCookieName)

  def toggleFormBuilder(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]) =
    controllerComponents.actionBuilder.async { request =>
      val redirect =
        Redirect(uk.gov.hmrc.gform.testonly.routes.TestOnlyController.handleToolbox(formTemplateId, maybeAccessCode))
      val result = request.cookies.get(CookieNames.formBuilderCookieName) match {
        case None    => redirect.withCookies(enableFormBuilderCookie)
        case Some(_) => redirect.discardingCookies(discardFormBuilderCookie)
      }
      Future.successful(result)
    }

  def getSnapshots(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    userInputs: UserInputs
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import i18nSupport._
        val currentUrl = request.uri
        val validatedFrom = for {
          from <- userInputs.from
        } yield DateTimeValidator.validateUserInput("from", from)
        val validatedTo = for {
          to <- userInputs.to
        } yield DateTimeValidator.validateUserInput("to", to)
        val (from, to) = (validatedFrom, validatedTo) match {
          case (Some(Valid(from)), Some(Valid(to))) => (from, to)
          case (Some(Valid(from)), _)               => (from, None)
          case (_, Some(Valid(to)))                 => (None, to)
          case _                                    => (None, None)
        }
        val filter = SnapshotFilter(
          from,
          to,
          userInputs.snapshotIdFilter,
          userInputs.descriptionFilter,
          userInputs.templateIdFilter
        )
        for {
          snapshots <- gformConnector.getSnapshots(filter).map(_.sortBy(_.savedAt)(Ordering[Instant].reverse))
        } yield {
          val html = renderSnapshots(snapshots, formTemplateId, maybeAccessCode, currentUrl)

          val templateItems = SelectItem(
            value = userInputs.templateIdFilter,
            text = userInputs.templateIdFilter.getOrElse("")
          ) :: snapshots.map(_.originalTemplateId.value).distinct.sorted.map { v =>
            SelectItem(
              value = Some(v),
              text = v
            )
          }
          val govukLabel = new GovukLabel()
          val govukHint = new GovukHint()
          val govukErrorMessage = new GovukErrorMessage()
          val govukFormGroup: GovukFormGroup = new GovukFormGroup
          val govukHintAndErrorMessage: GovukHintAndErrorMessage =
            new GovukHintAndErrorMessage(govukHint, govukErrorMessage)
          val govukSelect = new GovukSelect(govukLabel, govukFormGroup, govukHintAndErrorMessage)
          val select = govukSelect(
            Select(
              id = "templateIdFilter",
              name = "templateIdFilter",
              items = templateItems,
              label = Label(
                content = Text("Filter by template id")
              )
            )
          )
          Ok(
            snapshots_page(
              cache.formTemplate,
              maybeAccessCode,
              frontendAppConfig,
              html,
              cache.form._id.value,
              UserInputs.fromDateInput(userInputs, DateTimeValidator.mayBeErrors(validatedFrom)),
              UserInputs.toDateInput(userInputs, DateTimeValidator.mayBeErrors(validatedTo)),
              userInputs.snapshotIdFilter,
              userInputs.descriptionFilter,
              select
            )
          )
        }
    }

  def renderSnapshots(
    snapshots: List[SnapshotOverview],
    formTemplateId: FormTemplateId,
    accessCode: Option[AccessCode],
    currentUrl: String
  ): Html = {

    val tableRows: List[List[TableRow]] = snapshots.map { snapshot =>
      List(
        TableRow(
          content = HtmlContent(Html(snapshot.originalTemplateId.value))
        ),
        TableRow(
          content = HtmlContent(Html(snapshot.description.value)),
          attributes = Map("style" -> "word-break: break-word")
        ),
        TableRow(
          content = Text(formatInstant(snapshot.savedAt))
        ),
        TableRow(
          content = HtmlContent {
            val url = uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .snapshotPage(
                formTemplateId,
                snapshot.snapshotId,
                accessCode
              )
              .url
            s"""<a class=govuk-link href='$url'>${snapshot.snapshotId.value}</a>"""
          }
        ),
        TableRow(
          content = HtmlContent {
            val url = uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .updateSnapshotPage(formTemplateId, snapshot.snapshotId, accessCode)
              .url
            s"""<a class=govuk-link href='$url'>edit</a>"""
          }
        ),
        TableRow(
          content = HtmlContent {
            val url = uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .snapshotDeleteConfirmation(snapshot.snapshotId, RedirectUrl(currentUrl))
              .url
            s"""<a class=govuk-link href='$url'>delete</a>"""
          }
        )
      )
    }

    val header: List[HeadCell] = List(
      HeadCell(
        content = Text("Template id")
      ),
      HeadCell(
        content = Text("Description")
      ),
      HeadCell(
        content = Text("Saved at")
      ),
      HeadCell(
        content = Text("Snapshot id (click to restore)")
      ),
      HeadCell(
        content = Text("")
      ),
      HeadCell(
        content = Text("")
      )
    )

    val table = Table(
      head = Some(header),
      rows = tableRows,
      firstCellIsHeader = true
    )

    new GovukTable()(table)
  }

  def snapshotDeleteConfirmation(snapshotId: SnapshotId, backUrl: RedirectUrl) =
    controllerComponents.actionBuilder.async { implicit request =>
      import i18nSupport._
      implicit val lang: LangADT = LangADT.En
      val actionUrl = uk.gov.hmrc.gform.testonly.routes.TestOnlyController.deleteSnapshot(snapshotId).url
      Ok(
        snapshot_delete_confirmation(
          frontendAppConfig,
          snapshotId,
          actionUrl,
          backUrl.get(OnlyRelative).url
        )
      ).pure[Future]
    }

  def deleteSnapshot(snapshotId: SnapshotId) =
    controllerComponents.actionBuilder.async { implicit request =>
      import i18nSupport._
      implicit val lang: LangADT = LangADT.En
      deleteSnapshotUserData
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest("delete snapshot error: ${formWithErrors.errorsAsJson}").pure[Future],
          userData =>
            gformConnector
              .deleteSnapshot(userData.snapshotId)
              .map { _ =>
                Ok(snapshot_delete_acknowledgement(frontendAppConfig, snapshotId, userData.backUrl))
              }
        )
    }

  def filterSnapshotsPost(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    controllerComponents.actionBuilder.async { implicit request =>
      val answers: Map[String, String] = request.body.asFormUrlEncoded
        .map(_.collect {
          case (field, value :: _) if value.trim.nonEmpty =>
            (field, value.trim)
        })
        .getOrElse(Map.empty[String, String])

      val fromUserInputs = DateTimeUserInput("from", answers)
      val toUserInputs = DateTimeUserInput("to", answers)
      val templateIdFilter =
        if (answers.get("currentTemplateId").contains("on")) Some(formTemplateId.value)
        else answers.get("templateIdFilter")
      val userInputs =
        UserInputs(
          Some(fromUserInputs),
          Some(toUserInputs),
          answers.get("snapshotIdFilter"),
          answers.get("descFilter"),
          templateIdFilter
        )

      Future.successful(
        Redirect(
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController.getSnapshots(
            formTemplateId,
            maybeAccessCode,
            userInputs
          )
        )
      )
    }

  def getFormData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    _ => _ => cache => _ => formModelOptics =>
      Future.successful(
        Ok(Json.toJson(cache.form))
      )
  }

  def generateSummaryContent(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      accessCode,
      OperationWithForm.DownloadSummaryPdf
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      summaryController
        .createPDFContent(cache, formModelOptics)
        .map(content => pdfContent(cache.formTemplate.accessiblePdf, content))
    }

  def generateAcknowledgementContent(
    formTemplateId: FormTemplateId,
    accessCode: Option[AccessCode]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.WithAcknowledgement](
      formTemplateId,
      accessCode,
      OperationWithForm.ViewAcknowledgement
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      acknowledgementPdfService
        .createPDFContent(cache, accessCode, formModelOptics, sendAuditEvent = false)
        .map(content => pdfContent(cache.formTemplate.accessiblePdf, content))
    }

  private def pdfContent(accessiblePdf: Boolean, content: PdfContent) =
    if (accessiblePdf) {
      Ok(content.content).as(XML)
    } else {
      Ok(content.content).as(HTML)
    }
  def changeStateAndRedirectToCYA(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ForceReturnToCYA
    ) { implicit request => _ => cache => _ => _ =>
      for {
        _ <- gformConnector.deleteGeneratedFiles(cache.form.envelopeId)
        res <-
          gformConnector
            .updateUserData(
              FormIdData(cache.retrievals, formTemplateId, maybeAccessCode),
              UserData(
                cache.form.formData,
                InProgress,
                cache.form.visitsIndex,
                cache.form.thirdPartyData,
                cache.form.componentIdToFileId,
                cache.form.taskIdTaskStatus
              )
            )
            .flatMap { _ =>
              Future.successful(
                Redirect(
                  uk.gov.hmrc.gform.gform.routes.SummaryController
                    .summaryById(formTemplateId, maybeAccessCode, None, None, true, None)
                )
              )
            }
      } yield res
    }

  // This endpoint is used by performance test scripts to submit data for the summary page.
  def submitSummary(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    save: Direction,
    maybeCoordinates: Option[Coordinates],
    taskCompleted: Option[Boolean]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.AcceptSummary
    ) { implicit request: Request[AnyContent] => implicit l => cache => implicit sse => formModelOptics =>
      processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics) { _ => _ => _ =>
        save match {
          case Exit =>
            Redirect(
              uk.gov.hmrc.gform.gform.routes.SaveAcknowledgementController
                .saveAndExitFromSummary(cache.formTemplateContext.formTemplate._id, maybeAccessCode, maybeCoordinates)
            ).pure[Future]
          case SummaryContinue =>
            summaryController.handleSummaryContinue(
              cache.form.formTemplateId,
              maybeAccessCode,
              cache,
              formModelOptics,
              cache.form.formData.fingerprint,
              maybeCoordinates,
              taskCompleted
            )
          case _ => BadRequest("Cannot determine action").pure[Future]
        }
      }
    }

  def validateFormHtml(
    formTemplateId: FormTemplateId
  ) =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) { implicit request => _ => _ =>
      for {
        rawTemplateJson    <- gformConnector.getFormTemplateRaw(formTemplateId)
        validationResponse <- gformConnector.validateFormHtml(rawTemplateJson)
      } yield Ok(validationResponse).as("application/json")
    }
}
