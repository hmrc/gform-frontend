/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.mvc.Request
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.gform.{ CustomerId, FrontEndSubmissionVariablesBuilder, StructuredFormDataBuilder }
import uk.gov.hmrc.gform.graph.{ CustomerIdRecalculation, EmailParameterRecalculation, Recalculation }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, FormIdData, FormStatus, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParametersRecalculated, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil, BundledFormSubmissionData, LangADT, PdfHtml, SubmissionData }
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.summary.{ SubmissionDetails, SummaryRenderingService }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

trait GformBackEndAlgebra[F[_]] {
  def getForm(id: FormId)(implicit hc: HeaderCarrier): F[Form]

  def getForm(id: FormIdData)(implicit hc: HeaderCarrier): F[Form]

  def getFormTemplate(id: FormTemplateId)(implicit hc: HeaderCarrier): F[FormTemplate]

  def submissionDetails(formIdData: FormIdData)(implicit hc: HeaderCarrier): F[Submission]

  def submitWithUpdatedFormStatus(
    formStatus: FormStatus,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    submissionDetails: Option[SubmissionDetails])(
    implicit request: Request[_],
    l: LangADT,
    hc: HeaderCarrier): F[(HttpResponse, CustomerId)]

  def updateUserData(updatedForm: Form, maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): F[Unit]

  def getFormBundle(rootFormId: FormIdData)(implicit hc: HeaderCarrier): F[NonEmptyList[FormId]]

  def submitFormBundle(rootFormId: FormIdData, bundle: NonEmptyList[BundledFormSubmissionData])(
    implicit hc: HeaderCarrier): F[Unit]

  def forceUpdateFormStatus(formId: FormIdData, status: FormStatus)(implicit hc: HeaderCarrier): F[Unit]
}

class GformBackEndService(
  gformConnector: GformConnector,
  summaryRenderingService: SummaryRenderingService,
  recalculation: Recalculation[Future, Throwable],
  customerIdRecalculation: CustomerIdRecalculation[Future],
  lookupRegistry: LookupRegistry)(implicit ec: ExecutionContext)
    extends GformBackEndAlgebra[Future] {

  def getForm(id: FormId)(implicit hc: HeaderCarrier): Future[Form] = gformConnector.getForm(id)

  def getForm(id: FormIdData)(implicit hc: HeaderCarrier): Future[Form] = gformConnector.getForm(id)

  def getFormTemplate(id: FormTemplateId)(implicit hc: HeaderCarrier): Future[FormTemplate] =
    gformConnector.getFormTemplate(id)

  def getFormBundle(rootFormId: FormIdData)(implicit hc: HeaderCarrier): Future[NonEmptyList[FormId]] =
    gformConnector.getFormBundle(rootFormId)

  def submitFormBundle(rootFormId: FormIdData, bundle: NonEmptyList[BundledFormSubmissionData])(
    implicit hc: HeaderCarrier): Future[Unit] =
    gformConnector.submitFormBundle(rootFormId, bundle)

  def submissionDetails(formIdData: FormIdData)(implicit hc: HeaderCarrier): Future[Submission] =
    gformConnector.submissionDetails(formIdData)

  def submitWithUpdatedFormStatus(
    formStatus: FormStatus,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    submissionDetails: Option[SubmissionDetails])(
    implicit request: Request[_],
    l: LangADT,
    hc: HeaderCarrier): Future[(HttpResponse, CustomerId)] =
    for {
      _          <- updateUserData(cache.form.copy(status = formStatus), maybeAccessCode)
      customerId <- customerIdRecalculation.evaluateCustomerId(cache)
      response   <- handleSubmission(maybeAccessCode, cache, customerId, submissionDetails)
    } yield (response, customerId)

  def forceUpdateFormStatus(formId: FormIdData, status: FormStatus)(implicit hc: HeaderCarrier): Future[Unit] =
    gformConnector.forceUpdateFormStatus(formId, status)

  private def handleSubmission(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    customerId: CustomerId,
    submissionDetails: Option[SubmissionDetails])(
    implicit request: Request[_],
    l: LangADT,
    hc: HeaderCarrier): Future[HttpResponse] =
    for {
      htmlForPDF         <- summaryRenderingService.createHtmlForPdf(maybeAccessCode, cache, submissionDetails)
      emailParameter     <- EmailParameterRecalculation(cache).recalculateEmailParameters(recalculation)
      structuredFormData <- StructuredFormDataBuilder(cache.form, cache.formTemplate, lookupRegistry)
      response <- handleSubmission(
                   cache.retrievals,
                   cache.formTemplate,
                   emailParameter,
                   maybeAccessCode,
                   customerId,
                   htmlForPDF,
                   structuredFormData
                 )
    } yield response

  def updateUserData(updatedForm: Form, maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): Future[Unit] =
    gformConnector
      .updateUserData(
        FormIdData.fromForm(updatedForm, maybeAccessCode),
        UserData(
          updatedForm.formData,
          updatedForm.status,
          updatedForm.visitsIndex,
          updatedForm.thirdPartyData
        )
      )

  private def handleSubmission(
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    emailParameters: EmailParametersRecalculated,
    maybeAccessCode: Option[AccessCode],
    customerId: CustomerId,
    htmlForPDF: PdfHtml,
    structuredFormData: StructuredFormValue.ObjectStructure
  )(implicit hc: HeaderCarrier): Future[HttpResponse] =
    gformConnector.submitForm(
      FormIdData(retrievals, formTemplate._id, maybeAccessCode),
      customerId,
      buildSubmissionData(htmlForPDF, customerId, retrievals, formTemplate, emailParameters, structuredFormData),
      AffinityGroupUtil.fromRetrievals(retrievals)
    )

  private def buildSubmissionData(
    htmlForPDF: PdfHtml,
    customerId: CustomerId,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    emailParameters: EmailParametersRecalculated,
    structuredFormData: StructuredFormValue.ObjectStructure): SubmissionData =
    SubmissionData(
      htmlForPDF,
      FrontEndSubmissionVariablesBuilder(retrievals, formTemplate, customerId),
      structuredFormData,
      emailParameters)
}
