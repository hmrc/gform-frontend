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

package uk.gov.hmrc.gform.instructions

import play.api.i18n.I18nSupport
import play.api.mvc.Request
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.FileUploadAlgebra
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.views.html.summary.summaryInstructionPdf
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class InstructionsRenderingService(
  i18nSupport: I18nSupport,
  fileUploadAlgebra: FileUploadAlgebra[Future],
  validationService: ValidationService) {

  def createInstructionPDFHtml[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeSubmissionDetails: Option[SubmissionDetails],
    formModelOptics: FormModelOptics[D]
  )(
    implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[PdfHtml] = {
    import i18nSupport._
    for {
      envelope <- fileUploadAlgebra.getEnvelope(cache.form.envelopeId)
      validationResult <- validationService
                           .validateFormModel(cache.toCacheData, envelope, formModelOptics.formModelVisibilityOptics)
    } yield {
      val mayBeInstructionPdf = cache.formTemplate.destinations match {
        case DestinationList(_, acknowledgementSection, _) =>
          acknowledgementSection.instructionPdf
        case _ => None
      }
      PdfHtml(
        summaryInstructionPdf(
          FormModelInstructionSummaryConverter.convert(formModelOptics, cache, envelope, validationResult),
          maybeSubmissionDetails,
          mayBeInstructionPdf,
          cache.formTemplate
        ).toString)
    }
  }
}
