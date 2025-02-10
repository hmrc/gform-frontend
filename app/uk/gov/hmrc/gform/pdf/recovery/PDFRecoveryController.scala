/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.pdf.recovery

import org.slf4j.{ Logger, LoggerFactory }
import play.api.http.HttpEntity
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, ResponseHeader, Result }
import uk.gov.hmrc.auth.core.{ ConfidenceLevel, Enrolments, User }
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, GovernmentGatewayId, OtherRetrievals, Role }
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.gformbackend.GformBackEndService
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.pdf.model.PDFType
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Individual
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroup, AffinityGroupUtil, PdfContent, SourceOrigin, SubmissionRef, VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, FormTemplateId, IsChoice, IsRevealingChoice }
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionId }
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.{ ExecutionContext, Future }

class PDFRecoveryController(
  auth: AuthenticatedRequestActions,
  controllerComponents: MessagesControllerComponents,
  gformBackEndService: GformBackEndService,
  lookupRegistry: LookupRegistry,
  nonRepudiationHelpers: NonRepudiationHelpers,
  i18nSupport: I18nSupport,
  pdfGeneratorService: PdfGeneratorService,
  recalculation: Recalculation[Future, Throwable]
)(implicit ec: ExecutionContext)
    extends FrontendController(controllerComponents: MessagesControllerComponents) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def recoverInstructionPdf(
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId,
    submissionTime: String,
    affinityGroup: String
  ): Action[AnyContent] =
    auth.asyncNoAuth(formTemplateId) { implicit request => implicit l => formTemplate =>
      import i18nSupport._
      logger.info(
        s"Calling recovery Instruction Pdf, formTemplateId: ${formTemplateId.value}, envelopeId: ${envelopeId.value} , affinityGroup: $affinityGroup, submissionTime: $submissionTime"
      )
      val customFormatter = DateTimeFormatter.ofPattern("yyyy-MM-ddHH:mm:ss")
      val localDateTime = LocalDateTime.parse(submissionTime, customFormatter)
      logger.info(s"Parsing submissionTime: $localDateTime")
      val affinity = AffinityGroupUtil.toAffinityGroup(affinityGroup).getOrElse(Individual)
      for {
        form                <- gformBackEndService.getFormByEnvelopeId(formTemplateId, envelopeId)
        formTemplateContext <- gformBackEndService.getFormTemplate(formTemplateId)
        cache = AuthCacheWithForm(
                  createAuthenticatedRetrievals(affinity),
                  form,
                  formTemplateContext,
                  Role.Reviewer,
                  None,
                  lookupRegistry
                )
        submissionMark = nonRepudiationHelpers.computeHash(nonRepudiationHelpers.formDataToJson(cache.form))
        submissionDetails = Some(
                              SubmissionDetails(
                                Submission(
                                  SubmissionId(form._id, envelopeId),
                                  localDateTime,
                                  SubmissionRef(envelopeId),
                                  envelopeId,
                                  DmsMetaData(formTemplateId)
                                ),
                                submissionMark
                              )
                            )
        fcLookup = cache.formModel[SectionSelectorType.WithAcknowledgement].fcLookup
        variadicData = createVariadicFormData(form.formData.toData, fcLookup)
        formModelOptics <- FormModelOptics.mkFormModelOptics[DataOrigin.Browser, Future, SectionSelectorType.Normal](
                             variadicData,
                             cache,
                             recalculation
                           )
        htmlForInstructionPDF <-
          gformBackEndService
            .createHTMLForInstructionPDF[SectionSelectorType.Normal, DataOrigin.Browser, PDFType.Summary](
              None,
              cache,
              submissionDetails,
              formModelOptics,
              Some(cache.formTemplate.formName.value)
            )
        pdfStream <- pdfGeneratorService.generatePDF(htmlForInstructionPDF.getOrElse(PdfContent("")))
      } yield Result(
        header = ResponseHeader(200, Map.empty),
        body = HttpEntity.Streamed(pdfStream, None, Some("application/pdf"))
      )
    }

  private def createVariadicFormData(
    toData: Map[ModelComponentId, String],
    fcLookup: Map[FormComponentId, FormComponent]
  ) =
    VariadicFormData[SourceOrigin.OutOfDate](toData.map { case (modelComponentId, value) =>
      fcLookup.map(l => l._1.modelComponentId.baseComponentId -> l._2).get(modelComponentId.baseComponentId) match {
        case Some(fc) =>
          fc match {
            case IsRevealingChoice(_) | IsChoice(_) =>
              modelComponentId -> VariadicValue.Many(value.split(",").toIndexedSeq)
            case _ => modelComponentId -> VariadicValue.One(value)
          }
        case _ =>
          logger.error(s"Form component not found ${modelComponentId.toFormComponentId.value}")
          throw new RuntimeException(s"Form component not found ${modelComponentId.toFormComponentId.value}")
      }
    })

  private def createAuthenticatedRetrievals(affinityGroup: AffinityGroup) =
    AuthenticatedRetrievals(
      GovernmentGatewayId(""),
      Enrolments(Set.empty),
      affinityGroup,
      "PDFRecoveryId",
      None,
      OtherRetrievals.empty,
      ConfidenceLevel.L250,
      Some(User)
    )

}
