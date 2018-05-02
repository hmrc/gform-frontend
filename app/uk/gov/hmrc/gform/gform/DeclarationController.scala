/*
 * Copyright 2018 HM Revenue & Customs
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

import cats.data.Validated.{ Invalid, Valid }
import org.jsoup.Jsoup
import play.api.i18n.I18nSupport
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.AuthService
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.auth.models.Retrievals.getTaxIdValue
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ formDataMap, get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

class DeclarationController(
    i18nSupport: I18nSupport,
    config: FrontendAppConfig,
    auth: AuthenticatedRequestActions,
    gformConnector: GformConnector,
    auditService: AuditService,
    repeatService: RepeatingComponentService,
    summaryController: SummaryController,
    pdfService: PdfGeneratorService,
    renderer: SectionRenderingService,
    validationService: ValidationService,
    authService: AuthService
) extends FrontendController {

  import i18nSupport._

  def showDeclaration(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    cache.form.status match {
      case Validated => renderer.renderDeclarationSection(cache.form, cache.formTemplate, cache.retrievals, None, Map.empty, None, repeatService.getCache, lang).map(Ok(_))
      case _ => Future.successful(BadRequest)
    }
  }
  //todo try and refactor the two addExtraDataToHTML into one method
  private def addExtraDataToHTML(
    html: String,
    authConfig: AuthConfig,
    submissionReference: Option[TextExpression],
    retrievals: Retrievals,
    data: Map[FormComponentId, Seq[String]]
  ): String = {
    // format: OFF
    val referenceNumber = (authConfig, submissionReference) match {
      case (_,                  Some(textExpression)) => authService.evaluateSubmissionReference(textExpression, retrievals, data)
      case (_: EEITTAuthConfig, None)                 => authService.eeitReferenceNumber(retrievals)
      case (_,                  None)                 => getTaxIdValue(Some("HMRC-OBTDS-ORG"), "EtmpRegistrationNumber", retrievals)
    }
    // format: ON

    val extraData =
      s"""
         |<table class="table--font-reset ">
         |  <thead>
         |    <tr>
         |      <th class="grid-layout__column--1-2"> <h2 class="h2-heading">Submission details</h2> </th>
         |      <th class="text--right"> </th>
         |    </tr>
         |  </thead>
         |  <tbody>
         |    <tr>
         |      <td>Submission reference</td>
         |      <td>${referenceNumber}</td>
         |     </tr>
         |  </tbody>
         |</table>
      """.stripMargin

    val doc = Jsoup.parse(html)
    doc.select("article[class*=content__body]").append(extraData)
    doc.html
  }

  def submitDeclaration(formTemplateId4Ga: FormTemplateId, formId: FormId, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    processResponseDataFromBody(request) { (data: Map[FormComponentId, Seq[String]]) =>

      val validationResultF = validationService.validateComponents(getAllDeclarationFields(cache.formTemplate.declarationSection.fields), data, cache.form.envelopeId, cache.retrievals)

      get(data, FormComponentId("save")) match {
        case "Continue" :: Nil => validationResultF.flatMap {
          case Valid(()) =>
            val updatedForm = updateFormWithDeclaration(cache.form, cache.formTemplate, data)
            val customerId = authService.evaluateSubmissionReference(cache.formTemplate.dmsSubmission.customerId, cache.retrievals, formDataMap(updatedForm.formData))
            for {
              _ <- gformConnector.updateUserData(cache.form._id, UserData(updatedForm.formData, None, Signed))
              //todo perhaps not make these calls at all if the feature flag is false?
              summaryHml <- summaryController.getSummaryHTML(formId, cache, repeatService.getCache, lang)
              cleanHtml = pdfService.sanitiseHtmlForPDF(summaryHml)
              htmlForPDF = addExtraDataToHTML(cleanHtml, cache.formTemplate.authConfig, cache.formTemplate.submissionReference, cache.retrievals, data)
              _ <- if (config.sendPdfWithSubmission) gformConnector.submitFormWithPdf(formId, customerId, htmlForPDF) else { gformConnector.submitForm(formId, customerId) }
            } yield {
              val submissionEventId = auditService.sendSubmissionEvent(cache.form, cache.formTemplate.sections :+ cache.formTemplate.declarationSection, cache.retrievals)
              Redirect(uk.gov.hmrc.gform.gform.routes.AcknowledgementController.showAcknowledgement(formId, formTemplateId4Ga, lang, submissionEventId))
            }
          case validationResult @ Invalid(_) =>
            val errorMap: List[(FormComponent, FormFieldValidationResult)] = getErrorMap(validationResult, data, cache.formTemplate)
            for {
              html <- renderer.renderDeclarationSection(cache.form, cache.formTemplate, cache.retrievals, Some(validationResult), data, Some(errorMap), repeatService.getCache, lang)
            } yield Ok(html)
        }
        case _ =>
          Future.successful(BadRequest("Cannot determine action"))
      }
    }
  }

  private def updateFormWithDeclaration(form: Form, formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]]) = {
    val fieldNames = data.keySet.map(_.value)
    val allDeclarationFields = getAllDeclarationFields(formTemplate.declarationSection.fields)
    val submissibleFormFields = allDeclarationFields.flatMap { fieldValue =>
      fieldNames
        .filter(_.startsWith(fieldValue.id.value))
        .map(name => FormField(FormComponentId(name), data(FormComponentId(name)).head))
    }
    val updatedFields = form.formData.fields ++ submissibleFormFields

    form.copy(formData = form.formData.copy(fields = updatedFields))
  }

  private def getErrorMap(validationResult: ValidatedType, data: Map[FormComponentId, Seq[String]], formTemplate: FormTemplate): List[(FormComponent, FormFieldValidationResult)] = {
    val declarationFields = getAllDeclarationFields(formTemplate.declarationSection.fields)
    validationService.evaluateValidation(validationResult, declarationFields, data, Envelope(Nil))
  }

  private def getAllDeclarationFields(fields: List[FormComponent]): List[FormComponent] = {
    fields.flatMap { fieldValue =>
      fieldValue.`type` match {
        case grp: Group => getAllDeclarationFields(grp.fields)
        case _ => List(fieldValue)
      }
    }
  }

}
