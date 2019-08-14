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

import cats.instances.future._
import play.api.mvc.Request
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.gform.{ CustomerId, FrontEndSubmissionVariablesBuilder, StructuredFormDataBuilder }
import uk.gov.hmrc.gform.graph.{ CustomerIdRecalculation, EmailParameterRecalculation, Recalculation }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, FormStatus, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParametersRecalculated, FormTemplate }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil, LangADT, SubmissionData }
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.summary.{ SubmissionDetails, SummaryRenderingService }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

trait GformBackEndAlgebra[F[_]] {
  def submissionDetails(formId: FormId)(implicit hc: HeaderCarrier): F[Submission]

  def submitWithUpdatedFormStatus(
    formStatus: FormStatus,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    submissionDetails: Option[SubmissionDetails])(
    implicit request: Request[_],
    l: LangADT,
    hc: HeaderCarrier): F[(HttpResponse, CustomerId)]

  def updateUserData(updatedForm: Form, status: FormStatus)(implicit hc: HeaderCarrier): F[Unit]
}

class GformBackEndService(
  gformConnector: GformConnector,
  summaryRenderingService: SummaryRenderingService,
  recalculation: Recalculation[Future, Throwable],
  customerIdRecalculation: CustomerIdRecalculation[Future],
  lookupRegistry: LookupRegistry)(implicit ec: ExecutionContext)
    extends GformBackEndAlgebra[Future] {

  def submissionDetails(formId: FormId)(implicit hc: HeaderCarrier): Future[Submission] =
    gformConnector.submissionDetails(formId)

  def submitWithUpdatedFormStatus(
    formStatus: FormStatus,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    submissionDetails: Option[SubmissionDetails])(
    implicit request: Request[_],
    l: LangADT,
    hc: HeaderCarrier): Future[(HttpResponse, CustomerId)] =
    for {
      _          <- updateUserData(cache.form, formStatus)
      customerId <- customerIdRecalculation.evaluateCustomerId(cache)
      response   <- handleSubmission(maybeAccessCode, cache, customerId, submissionDetails)
    } yield (response, customerId)

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

  def updateUserData(updatedForm: Form, status: FormStatus)(implicit hc: HeaderCarrier): Future[Unit] =
    gformConnector
      .updateUserData(
        updatedForm._id,
        UserData(
          updatedForm.formData,
          status,
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
    htmlForPDF: String,
    structuredFormData: StructuredFormValue.ObjectStructure
  )(implicit hc: HeaderCarrier): Future[HttpResponse] =
    gformConnector.submitForm(
      FormId(retrievals, formTemplate._id, maybeAccessCode),
      customerId,
      buildSubmissionData(htmlForPDF, customerId, retrievals, formTemplate, emailParameters, structuredFormData),
      AffinityGroupUtil.fromRetrievals(retrievals)
    )

  private def buildSubmissionData(
    htmlForPDF: String,
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
