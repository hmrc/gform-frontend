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

package uk.gov.hmrc.gform.gformbackend

import cats.data.NonEmptyList
import cats.instances.future._
import play.api.i18n.Messages
import play.api.mvc.Request
import cats.syntax.all._
import play.api.libs.json.Json
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.objectStore.Attachments
import uk.gov.hmrc.gform.gform.{ CustomerId, DestinationEvaluator, FrontEndSubmissionVariablesBuilder, SectionRenderingService, StructuredFormDataBuilder, SummaryPagePurpose, UserSessionBuilder }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ SectionSelector, SectionSelectorType, UserSession }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.pdf.model.PDFCustomRender
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.InstructionPdfFields
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil, BundledFormSubmissionData, LangADT, PdfContent, SourceOrigin, SubmissionData, UserId, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormId, FormIdData, FormModelOptics, FormStatus, QueryParams, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParameter, EmailParameterValue, EmailParametersRecalculated, EmailTemplateVariable, FormPhase, FormTemplate, FormTemplateContext, FormTemplateId, InstructionPDF }
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, SmartStringEvaluatorFactory }
import uk.gov.hmrc.gform.models.optics.DataOrigin.Mongo
import uk.gov.hmrc.gform.pdf.PDFRenderService
import uk.gov.hmrc.gform.pdf.model.{ PDFModel, PDFType }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.retrieval.{ AuthRetrievals, AuthRetrievalsByFormIdData }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

trait GformBackEndAlgebra[F[_]] {
  def getForm(id: FormIdData)(implicit hc: HeaderCarrier): F[Form]

  def getFormTemplate(id: FormTemplateId)(implicit hc: HeaderCarrier): F[FormTemplateContext]

  def createSubmission(
    formId: FormId,
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId,
    customerId: String,
    noOfAttachments: Int
  )(implicit hc: HeaderCarrier): F[Submission]

  def submissionDetails(formIdData: FormIdData, envelopeId: EnvelopeId)(implicit
    hc: HeaderCarrier
  ): F[Option[Submission]]

  def submitWithUpdatedFormStatus[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    formStatus: FormStatus,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    submissionDetails: Option[SubmissionDetails],
    customerId: CustomerId,
    attachments: Attachments,
    formModelOptics: FormModelOptics[D]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier,
    lise: SmartStringEvaluator
  ): F[(HttpResponse, CustomerId)]

  def updateUserData(updatedForm: Form, maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): F[Unit]

  def getFormBundle(rootFormId: FormIdData)(implicit hc: HeaderCarrier): F[NonEmptyList[FormIdData]]

  def submitFormBundle(rootFormId: FormIdData, bundle: NonEmptyList[BundledFormSubmissionData])(implicit
    hc: HeaderCarrier
  ): F[Unit]

  def forceUpdateFormStatus(formId: FormIdData, status: FormStatus)(implicit hc: HeaderCarrier): F[Unit]

  def newForm(formTemplateId: FormTemplateId, retrievals: MaterialisedRetrievals, queryParams: QueryParams)(implicit
    hc: HeaderCarrier
  ): F[FormIdData]
}

class GformBackEndService(
  gformConnector: GformConnector,
  renderer: SectionRenderingService,
  pdfRenderService: PDFRenderService,
  lookupRegistry: LookupRegistry,
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory
)(implicit ec: ExecutionContext)
    extends GformBackEndAlgebra[Future] {

  def getForm(id: FormIdData)(implicit hc: HeaderCarrier): Future[Form] = gformConnector.getForm(id)

  def getFormTemplate(id: FormTemplateId)(implicit hc: HeaderCarrier): Future[FormTemplateContext] =
    gformConnector.getFormTemplateContext(id)

  def getFormBundle(rootFormId: FormIdData)(implicit hc: HeaderCarrier): Future[NonEmptyList[FormIdData]] =
    gformConnector.getFormBundle(rootFormId)

  def createSubmission(
    formId: FormId,
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId,
    customerId: String,
    noOfAttachments: Int
  )(implicit hc: HeaderCarrier): Future[Submission] =
    gformConnector.createSubmission(formId, formTemplateId, envelopeId, customerId, noOfAttachments)

  def submitFormBundle(rootFormId: FormIdData, bundle: NonEmptyList[BundledFormSubmissionData])(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    gformConnector.submitFormBundle(rootFormId, bundle)

  def submissionDetails(formIdData: FormIdData, envelopeId: EnvelopeId)(implicit
    hc: HeaderCarrier
  ): Future[Option[Submission]] =
    gformConnector.submissionDetails(formIdData, envelopeId)

  def submitWithUpdatedFormStatus[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    formStatus: FormStatus,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    submissionDetails: Option[SubmissionDetails],
    customerId: CustomerId,
    attachments: Attachments,
    formModelOptics: FormModelOptics[D]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier,
    lise: SmartStringEvaluator
  ): Future[(HttpResponse, CustomerId)] =
    for {
      _        <- updateUserData(cache.form.copy(status = formStatus), maybeAccessCode)
      response <- handleSubmission(maybeAccessCode, cache, customerId, submissionDetails, attachments, formModelOptics)
    } yield (response, customerId)

  def forceUpdateFormStatus(formId: FormIdData, status: FormStatus)(implicit hc: HeaderCarrier): Future[Unit] =
    gformConnector.forceUpdateFormStatus(formId, status)

  private def handleSubmission[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    customerId: CustomerId,
    submissionDetails: Option[SubmissionDetails],
    attachments: Attachments,
    formModelOptics: FormModelOptics[D]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier,
    lise: SmartStringEvaluator
  ): Future[HttpResponse] = {
    val summarySectionDeclaration = renderer.renderSummarySectionDeclaration(
      cache,
      formModelOptics.asInstanceOf[FormModelOptics[DataOrigin.Mongo]],
      maybeAccessCode,
      cache.formTemplate.summarySection.excludeFieldsFromPDF,
      ValidationResult.empty
    )

    val maybePDFOptions = cache.formTemplate.destinations match {
      case d: DestinationList =>
        d.acknowledgementSection.pdf.map(p => PDFModel.Options(p.tabularFormat, p.includeSignatureBox))
      case _ => None
    }

    val structuredFormData = StructuredFormDataBuilder(
      formModelOptics.formModelVisibilityOptics,
      cache.formTemplate.destinations,
      cache.formTemplate.expressionsOutput,
      lookupRegistry
    )

    for {
      htmlForPDF <-
        pdfRenderService.createPDFContent[D, U, PDFType.Summary](
          s"${messages("summary.acknowledgement.pdf")} - ${cache.formTemplate.formName.value}",
          None,
          cache,
          formModelOptics,
          cache.formTemplate.destinations match {
            case d: DestinationList => d.acknowledgementSection.pdf.map(p => PDFModel.HeaderFooter(p.header, p.footer))
            case _                  => None
          },
          submissionDetails,
          SummaryPagePurpose.ForDms,
          Some(summarySectionDeclaration),
          None,
          maybePDFOptions,
          Some(cache.formTemplate.formName.value)
        )
      htmlForInstructionPDF <-
        dmsDestinationWithIncludeInstructionPdf(cache.formTemplate) match {
          case Some(InstructionPdfFields.Ordered) =>
            createHTMLForInstructionPDF[SectionSelectorType.Normal, D, PDFType.Instruction](
              cache,
              submissionDetails,
              formModelOptics,
              None
            )
          case Some(InstructionPdfFields.All) =>
            createHTMLForInstructionPDF[SectionSelectorType.Normal, D, PDFType.Summary](
              cache,
              submissionDetails,
              formModelOptics,
              Some(cache.formTemplate.formName.value)
            )
          case _ => Future.successful(None)
        }
      maybeEmailAddress <- cache.formTemplate.emailExpr.traverse { expr =>
                             val email = formModelOptics.formModelVisibilityOptics
                               .evalAndApplyTypeInfoFirst(expr)
                               .stringRepresentation
                             Future.successful(email)
                           }
      userSession <- UserSessionBuilder(cache: AuthCacheWithForm)
      _           <- gformConnector.upsertAuthRetrievals(AuthRetrievals.fromCache(cache))
      response <- handleSubmission(
                    cache.retrievals,
                    cache.formTemplate,
                    emailParameter(cache.formTemplate, formModelOptics.formModelVisibilityOptics),
                    maybeAccessCode,
                    customerId,
                    htmlForPDF,
                    htmlForInstructionPDF,
                    structuredFormData,
                    attachments,
                    formModelOptics.formModelVisibilityOptics,
                    maybeEmailAddress,
                    userSession
                  )
    } yield response
  }

  private def createHTMLForInstructionPDF[U <: SectionSelectorType: SectionSelector, D <: DataOrigin, P <: PDFType](
    cache: AuthCacheWithForm,
    submissionDetails: Option[SubmissionDetails],
    formModelOptics: FormModelOptics[D],
    maybeFormName: Option[String]
  )(implicit
    messages: Messages,
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    pdfFunctions: PDFCustomRender[P]
  ): Future[Option[PdfContent]] = {
    val formModelOpticsUpdated = FormModelOptics.mkFormModelOptics[D, SectionSelectorType.Normal](
      formModelOptics.formModelVisibilityOptics.recData.variadicFormData
        .asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
      cache,
      Some(FormPhase(InstructionPDF))
    )

    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorFactory
      .apply(
        formModelOpticsUpdated.formModelVisibilityOptics
          .asInstanceOf[FormModelVisibilityOptics[Mongo]]
      )

    pdfRenderService
      .createPDFContent[D, U, P](
        s"Instructions PDF - ${cache.formTemplate.formName.value}",
        None,
        cache,
        formModelOpticsUpdated,
        cache.formTemplate.destinations match {
          case DestinationList(_, acknowledgementSection, _) =>
            acknowledgementSection.instructionPdf.map(p => PDFModel.HeaderFooter(p.header, p.footer))
          case _ => None
        },
        submissionDetails,
        SummaryPagePurpose.ForDms,
        None,
        maybeFormName = maybeFormName
      )
      .map(Some(_))

  }

  def emailParameter[D <: DataOrigin](
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit messages: Messages): EmailParametersRecalculated =
    formTemplate.emailParameters.fold(EmailParametersRecalculated.empty) { emailParameters =>
      val emailParametersRecalculated: Map[EmailTemplateVariable, EmailParameterValue] = emailParameters
        .map { case EmailParameter(emailTemplateVariable, expr) =>
          val emailParameterValue = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr).stringRepresentation
          EmailTemplateVariable(emailTemplateVariable) -> EmailParameterValue(emailParameterValue)
        }
        .toList
        .toMap
      EmailParametersRecalculated(emailParametersRecalculated)
    }

  def updateUserData(updatedForm: Form, maybeAccessCode: Option[AccessCode])(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    gformConnector
      .updateUserData(
        FormIdData.fromForm(updatedForm, maybeAccessCode),
        UserData(
          updatedForm.formData,
          updatedForm.status,
          updatedForm.visitsIndex,
          updatedForm.thirdPartyData,
          updatedForm.componentIdToFileId,
          updatedForm.taskIdTaskStatus
        )
      )

  private def handleSubmission[D <: DataOrigin](
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    emailParameters: EmailParametersRecalculated,
    maybeAccessCode: Option[AccessCode],
    customerId: CustomerId,
    htmlForPDF: PdfContent,
    htmlForInstructionPDF: Option[PdfContent],
    structuredFormData: StructuredFormValue.ObjectStructure,
    attachments: Attachments,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeEmailAddress: Option[String],
    userSession: UserSession
  )(implicit hc: HeaderCarrier, l: LangADT, m: Messages): Future[HttpResponse] =
    gformConnector.submitForm(
      FormIdData(retrievals, formTemplate._id, maybeAccessCode),
      customerId,
      buildSubmissionData(
        htmlForPDF,
        htmlForInstructionPDF,
        customerId,
        retrievals,
        formTemplate,
        emailParameters,
        structuredFormData,
        attachments,
        formModelVisibilityOptics,
        maybeEmailAddress,
        userSession
      ),
      AffinityGroupUtil.fromRetrievals(retrievals)
    )

  private def buildSubmissionData[D <: DataOrigin](
    htmlForPDF: PdfContent,
    htmlForInstructionPDF: Option[PdfContent],
    customerId: CustomerId,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    emailParameters: EmailParametersRecalculated,
    structuredFormData: StructuredFormValue.ObjectStructure,
    attachments: Attachments,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeEmailAddress: Option[String],
    userSession: UserSession
  )(implicit l: LangADT, m: Messages): SubmissionData =
    SubmissionData(
      htmlForPDF,
      htmlForInstructionPDF,
      FrontEndSubmissionVariablesBuilder(retrievals, formTemplate, formModelVisibilityOptics, customerId),
      structuredFormData,
      emailParameters,
      attachments,
      l,
      maybeEmailAddress,
      DestinationEvaluator(formTemplate, formModelVisibilityOptics),
      userSession
    )

  private def dmsDestinationWithIncludeInstructionPdf(formTemplate: FormTemplate): Option[InstructionPdfFields] =
    formTemplate.destinations match {
      case DestinationList(destinations, _, _) =>
        destinations.collectFirst {
          case HmrcDms(_, _, _, _, _, _, _, _, _, _, _, Some(instructionPdfFields), _, _, _, _, _, _, _, _) =>
            instructionPdfFields
        }

      case _ => None
    }

  override def newForm(formTemplateId: FormTemplateId, retrievals: MaterialisedRetrievals, queryParams: QueryParams)(
    implicit hc: HeaderCarrier
  ): Future[FormIdData] =
    for {
      newFormData <-
        gformConnector
          .newForm(formTemplateId, UserId(retrievals), AffinityGroupUtil.fromRetrievals(retrievals), queryParams)
      _ <- gformConnector.upsertAuthRetrievalsByFormIdData(
             AuthRetrievalsByFormIdData(newFormData, Json.toJson(retrievals))
           )
    } yield newFormData

}
