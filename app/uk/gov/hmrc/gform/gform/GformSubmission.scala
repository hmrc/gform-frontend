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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParametersRecalculated, FormTemplate }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil, SubmissionData }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object GformSubmission {

  def handleSubmission(
    config: FrontendAppConfig,
    gformConnector: GformConnector,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    emailParameters: EmailParametersRecalculated,
    maybeAccessCode: Option[AccessCode],
    customerId: CustomerId,
    htmlForPDF: String,
    structuredFormData: StructuredFormValue.ObjectStructure
  )(implicit hc: HeaderCarrier): Future[HttpResponse] =
    gformConnector.submitFormWithPdf(
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
