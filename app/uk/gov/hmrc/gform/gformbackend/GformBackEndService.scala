/*
 * Copyright 2021 HM Revenue & Customs
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

import scala.language.higherKinds
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.fileupload.Attachments
import uk.gov.hmrc.gform.gform.{ CustomerId, FrontEndSubmissionVariablesBuilder, StructuredFormDataBuilder, SummaryPagePurpose }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil, BundledFormSubmissionData, LangADT, PdfHtml, SourceOrigin, SubmissionData, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormId, FormIdData, FormModelOptics, FormStatus, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParameter, EmailParameterValue, EmailParametersRecalculated, EmailTemplateVariable, FormPhase, FormTemplate, FormTemplateId, InstructionPDF }
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, SmartStringEvaluatorFactory }
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.instructions.InstructionsRenderingService
import uk.gov.hmrc.gform.models.optics.DataOrigin.Mongo
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.summary.{ SubmissionDetails, SummaryRenderingService }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

trait GformBackEndAlgebra[F[_]] {
  def getForm(id: FormIdData)(implicit hc: HeaderCarrier): F[Form]

  def getFormTemplate(id: FormTemplateId)(implicit hc: HeaderCarrier): F[FormTemplate]

  def createSubmission(
    formId: FormId,
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId,
    customerId: String,
    noOfAttachments: Int
  )(implicit hc: HeaderCarrier): F[Submission]

  def submissionDetails(formIdData: FormIdData, envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): F[Submission]

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
}

class GformBackEndService(
  gformConnector: GformConnector,
  summaryRenderingService: SummaryRenderingService,
  instructionsRenderingService: InstructionsRenderingService,
  lookupRegistry: LookupRegistry,
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory,
  recalculation: Recalculation[Future, Throwable]
)(implicit ec: ExecutionContext)
    extends GformBackEndAlgebra[Future] {

  def getForm(id: FormIdData)(implicit hc: HeaderCarrier): Future[Form] = gformConnector.getForm(id)

  def getFormTemplate(id: FormTemplateId)(implicit hc: HeaderCarrier): Future[FormTemplate] =
    gformConnector.getFormTemplate(id)

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
  ): Future[Submission] =
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
  ): Future[HttpResponse] =
    for {
      htmlForPDF <-
        summaryRenderingService
          .createHtmlForPdf(maybeAccessCode, cache, submissionDetails, SummaryPagePurpose.ForDms, formModelOptics)
      htmlForInstructionPDF <- if (dmsDestinationWithIncludeInstructionPdf(cache.formTemplate))
                                 createHTMLForInstructionPDF[SectionSelectorType.Normal, D](
                                   maybeAccessCode,
                                   cache,
                                   submissionDetails,
                                   formModelOptics
                                 )
                               else
                                 Future.successful(None)
      structuredFormData <- StructuredFormDataBuilder(
                              formModelOptics.formModelVisibilityOptics,
                              cache.formTemplate.destinations,
                              lookupRegistry
                            )
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
                    formModelOptics.formModelVisibilityOptics
                  )
    } yield response

  private def createHTMLForInstructionPDF[U <: SectionSelectorType: SectionSelector, D <: DataOrigin](
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    submissionDetails: Option[SubmissionDetails],
    formModelOptics: FormModelOptics[D]
  )(implicit request: Request[_], l: LangADT, hc: HeaderCarrier) = {

    val formModelOpticsUpdatedF = FormModelOptics.mkFormModelOptics(
      formModelOptics.formModelVisibilityOptics.recData.variadicFormData
        .asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
      cache,
      recalculation,
      Some(FormPhase(InstructionPDF))
    )

    formModelOpticsUpdatedF.flatMap { formModelOpticsUpdated =>
      implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorFactory
        .apply(
          formModelOpticsUpdated.formModelVisibilityOptics
            .asInstanceOf[FormModelVisibilityOptics[Mongo]],
          cache.retrievals,
          maybeAccessCode,
          cache.form,
          cache.formTemplate
        )
      instructionsRenderingService
        .createInstructionPDFHtml(
          cache,
          submissionDetails,
          formModelOpticsUpdated
        )
        .map(Some(_))
    }
  }

  def emailParameter[D <: DataOrigin](
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): EmailParametersRecalculated =
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

  def updateUserData(updatedForm: Form, maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): Future[Unit] =
    gformConnector
      .updateUserData(
        FormIdData.fromForm(updatedForm, maybeAccessCode),
        UserData(
          updatedForm.formData,
          updatedForm.status,
          updatedForm.visitsIndex,
          updatedForm.thirdPartyData,
          updatedForm.componentIdToFileId
        )
      )

  private def handleSubmission[D <: DataOrigin](
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    emailParameters: EmailParametersRecalculated,
    maybeAccessCode: Option[AccessCode],
    customerId: CustomerId,
    htmlForPDF: PdfHtml,
    htmlForInstructionPDF: Option[PdfHtml],
    structuredFormData: StructuredFormValue.ObjectStructure,
    attachments: Attachments,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit hc: HeaderCarrier): Future[HttpResponse] =
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
        formModelVisibilityOptics
      ),
      AffinityGroupUtil.fromRetrievals(retrievals)
    )

  private def buildSubmissionData[D <: DataOrigin](
    htmlForPDF: PdfHtml,
    htmlForInstructionPDF: Option[PdfHtml],
    customerId: CustomerId,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    emailParameters: EmailParametersRecalculated,
    structuredFormData: StructuredFormValue.ObjectStructure,
    attachments: Attachments,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): SubmissionData =
    SubmissionData(
      htmlForPDF,
      htmlForInstructionPDF,
      FrontEndSubmissionVariablesBuilder(retrievals, formTemplate, formModelVisibilityOptics, customerId),
      structuredFormData,
      emailParameters,
      attachments
    )

  private def dmsDestinationWithIncludeInstructionPdf(formTemplate: FormTemplate): Boolean =
    formTemplate.destinations match {
      case DestinationList(destinations, _, _) =>
        destinations
          .collect { case HmrcDms(_, _, _, _, _, _, _, _, _, _, includeInstructionPdf) =>
            includeInstructionPdf
          }
          .contains(true)
      case _ => false
    }
}
