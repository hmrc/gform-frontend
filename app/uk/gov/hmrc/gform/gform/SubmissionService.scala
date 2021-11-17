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

package uk.gov.hmrc.gform.gform

import org.slf4j.LoggerFactory
import play.api.i18n.I18nSupport
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.{ Attachments, EnvelopeWithMapping, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.GformBackEndAlgebra
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, FormModelOptics, Signed }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, IsFileUpload }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class SubmissionService(
  i18nSupport: I18nSupport,
  gformBackEnd: GformBackEndAlgebra[Future],
  nonRepudiationHelpers: NonRepudiationHelpers,
  auditService: AuditService,
  fileUploadService: FileUploadService
)(implicit ec: ExecutionContext) {

  private val logger = LoggerFactory.getLogger(getClass)

  import i18nSupport._

  def submitForm[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    envelope: EnvelopeWithMapping,
    formModelOptics: FormModelOptics[D]
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    l: LangADT,
    lise: SmartStringEvaluator
  ): Future[CustomerId] = {

    val formModelVisibilityOptics = formModelOptics.formModelVisibilityOptics

    val attachments: Attachments = {
      val visibleFc: Set[FormComponent] = formModelVisibilityOptics.allFormComponents.toSet

      val visibleFcIds: Set[FormComponentId] = visibleFc.collect {
        case fc @ IsFileUpload(_) if envelope.contains(fc.modelComponentId) => fc.id
      }

      Attachments(visibleFcIds.toList)
    }

    val variadicFormData: VariadicFormData[SourceOrigin.Current] = formModelOptics.pageOpticsData

    val cacheUpd = cache.copy(form = cache.form.copy(formData = variadicFormData.toFormData))

    val submissionMark = nonRepudiationHelpers.computeHash(nonRepudiationHelpers.formDataToJson(cache.form))

    for {
      _ <- cleanseEnvelope(cache.form.envelopeId, envelope, attachments)
      customerId = CustomerIdRecalculation.evaluateCustomerId(cache, formModelOptics.formModelVisibilityOptics)
      submission <- gformBackEnd.createSubmission(
                      cache.form._id,
                      cache.form.formTemplateId,
                      cache.form.envelopeId,
                      customerId.id,
                      attachments.size
                    )
      result <- gformBackEnd
                  .submitWithUpdatedFormStatus(
                    Signed,
                    cacheUpd,
                    maybeAccessCode,
                    Some(SubmissionDetails(submission, submissionMark)),
                    customerId,
                    attachments,
                    formModelOptics
                  )

    } yield {
      val (_, customerId) = result
      auditSubmissionEvent(cacheUpd, customerId, formModelVisibilityOptics)
      customerId
    }
  }

  private def auditSubmissionEvent[D <: DataOrigin](
    cache: AuthCacheWithForm,
    customerId: CustomerId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit hc: HeaderCarrier): Unit =
    auditService.sendSubmissionEvent(cache.form, formModelVisibilityOptics, cache.retrievals, customerId)

  private def cleanseEnvelope(
    envelopeId: EnvelopeId,
    envelope: EnvelopeWithMapping,
    attachments: Attachments
  )(implicit
    hc: HeaderCarrier
  ): Future[Unit] = {
    val lookup: Set[FileId] =
      attachments.files.flatMap(envelope.mapping.mapping.get).toSet
    val toRemove: List[FileId] = envelope.files
      .filterNot { file =>
        lookup.contains(file.fileId)
      }
      .map(_.fileId)
    logger.warn(s"Removing ${toRemove.size} files from envelopeId $envelopeId.")
    fileUploadService.deleteFiles(envelopeId, toRemove.toSet)
  }

}
