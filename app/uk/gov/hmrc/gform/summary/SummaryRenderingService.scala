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

package uk.gov.hmrc.gform.summary

import java.time.format.DateTimeFormatter

import cats.data.Validated.{ Invalid, Valid }
import cats.instances.future._
import org.jsoup.Jsoup
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.Request
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gform.HtmlSanitiser
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.helpers.Fields.flattenGroups
import uk.gov.hmrc.gform.models.helpers.{ Fields, TaxPeriodHelper }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, Obligations }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormDataRecalculated, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.SubmissionRef
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.views.html.summary.snippets._
import uk.gov.hmrc.gform.views.html.summary.summary
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class SummaryRenderingService(
  i18nSupport: I18nSupport,
  fileUploadService: FileUploadService,
  recalculation: Recalculation[Future, Throwable],
  validationService: ValidationService,
  frontendAppConfig: FrontendAppConfig) {

  def createHtmlForPdf(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    submissionDetails: Option[SubmissionDetails])(
    implicit request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[String] = {
    import i18nSupport._

    // ToDo: Why do we sanitise just the summaryHtml and not the whole thing after adding the extra data?
    for {
      summaryHtml <- getSummaryHTML(cache.form.formTemplateId, maybeAccessCode, cache)
    } yield
      addExtraDataToHTML(
        // ToDo: I'm bothered by this. Why is submitted always true? Why is it not submissionDetails.isDefined?
        // Would it matter if sanitiseHtmlForPDF always did what it does when submitted = true?
        HtmlSanitiser.sanitiseHtmlForPDF(summaryHtml, submitted = true),
        submissionDetails,
        cache
      )
  }

  private def addExtraDataToHTML(html: String, submissionDetails: Option[SubmissionDetails], cache: AuthCacheWithForm)(
    implicit hc: HeaderCarrier,
    messages: Messages,
    curLang: LangADT): String = {
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val formattedTime = submissionDetails.map(sd =>
      s"""${sd.submission.submittedDate.format(dateFormat)} ${sd.submission.submittedDate.format(timeFormat)}""")

    val rows = List(
      formattedTime.map(ft => cya_row(messages("submission.date"), ft)),
      Some(cya_row(messages("submission.reference"), SubmissionRef(cache.form.envelopeId).toString)),
      submissionDetails.map(sd => cya_row(messages("submission.mark"), sd.hashedValue))
    ).flatten

    val extraData = cya_section(messages("submission.details"), HtmlFormat.fill(rows)).toString()

    val formDataAsMap = cache.form.formData.toData

    val declaration: List[(FormComponent, Seq[String])] = for {
      formTemplateDecField <- flattenGroups(cache.formTemplate.declarationSection.fields)
      formData             <- formDataAsMap.get(formTemplateDecField.id)
    } yield (formTemplateDecField, formData)

    val declarationExtraData = cya_section(
      messages("submission.declaration.details"),
      HtmlFormat.fill(declaration.map {
        case (formDecFields, formData) => cya_row(formDecFields.label.value, formData.mkString)
      })
    ).toString()

    val headerHtml = pdf_header(cache.formTemplate).toString()

    val doc = Jsoup.parse(html)
    doc.select("article[class*=content__body]").prepend(headerHtml)
    doc.select("article[class*=content__body]").append(extraData)
    doc.select("article[class*=content__body]").append(declarationExtraData)
    doc.html.replace("£", "&pound;")
  }

  def getSummaryHTML(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode], cache: AuthCacheWithForm)(
    implicit request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Html] = {
    val dataRaw = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

    import i18nSupport._

    for {
      data <- recalculation
               .recalculateFormData(
                 dataRaw,
                 cache.formTemplate,
                 cache.retrievals,
                 cache.form.thirdPartyData,
                 cache.form.envelopeId)
      envelope <- envelopeF
      (v, _)   <- validationService.validateForm(cache, envelope, cache.retrievals)
    } yield
      SummaryRenderingService.renderSummary(
        cache.formTemplate,
        v,
        data,
        maybeAccessCode,
        envelope,
        cache.retrievals,
        frontendAppConfig,
        cache.form.thirdPartyData.obligations
      )

  }
}

object SummaryRenderingService {
  def renderSummary(
    formTemplate: FormTemplate,
    validatedType: ValidatedType[ValidationResult],
    formFields: FormDataRecalculated,
    maybeAccessCode: Option[AccessCode],
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    frontendAppConfig: FrontendAppConfig,
    obligations: Obligations
  )(
    implicit
    request: Request[_],
    messages: Messages,
    l: LangADT): Html = {
    val sfr =
      summaryForRender(validatedType, formFields, maybeAccessCode, formTemplate, envelope, obligations)
    summary(
      formTemplate,
      sfr,
      maybeAccessCode,
      formTemplate.formCategory,
      retrievals.renderSaveAndComeBackLater,
      retrievals.continueLabelKey,
      frontendAppConfig
    )
  }

  def summaryForRender(
    validatedType: ValidatedType[ValidationResult],
    data: FormDataRecalculated,
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    envelope: Envelope,
    obligations: Obligations
  )(implicit messages: Messages, l: LangADT): List[Html] = {

    def renderHtmls(sections: List[Section], fields: List[FormComponent])(implicit l: LangADT): List[Html] = {
      def validate(formComponent: FormComponent): Option[FormFieldValidationResult] = {
        val gformErrors = validatedType match {
          case Invalid(errors) => errors
          case Valid(_)        => Map.empty[FormComponentId, Set[String]]
        }
        Fields.getValidationResult(data, fields, envelope, gformErrors)(formComponent)
      }

      def valueToHtml(
        fieldValue: FormComponent,
        formTemplateId: FormTemplateId,
        maybeAccessCode: Option[AccessCode],
        title: String,
        sectionNumber: SectionNumber,
        sectionTitle4Ga: SectionTitle4Ga): Html = {

        val changeButton = change_button(
          formTemplateId,
          maybeAccessCode,
          title,
          sectionNumber,
          sectionTitle4Ga,
          fieldValue
        )

        def groupToHtml(fieldValue: FormComponent, presentationHint: List[PresentationHint])(
          implicit l: LangADT): Html = {
          val isLabel = fieldValue.shortName.map(ls => ls.value).getOrElse(fieldValue.label.value).nonEmpty

          fieldValue.`type` match {
            case groupField: Group
                if presentationHint.contains(SummariseGroupAsGrid) && groupField.repeatsMax.isDefined =>
              val htmlList: List[Html] = {

                val groups: List[GroupList] =
                  getAllFieldsInGroup(fieldValue, groupField, data).filter(_.hasData(data))

                for {
                  group <- groups
                  value = group.componentList.map(validate)
                } yield group_grid(fieldValue, value, false, changeButton)

              }

              repeating_group(htmlList)
            case groupField: Group
                if presentationHint.contains(SummariseGroupAsGrid) => // TODO unify this case with previous one after new group_grid template is in place
              val fcs: List[FormComponent] =
                getAllFieldsInGroup(fieldValue, groupField, data).filter(_.hasData(data)).flatMap(_.componentList)

              val value = fcs.map(validate).filterNot(_.isEmpty)

              if (value.nonEmpty) {
                group_grid(fieldValue, value, isLabel, changeButton)
              } else Html("")

            case groupField @ Group(_, orientation, _, _, _, _) =>
              val fvs: List[GroupList] =
                getAllFieldsInGroup(fieldValue, groupField, data)

              val htmlList = fvs.flatMap(_.componentList.map { fv =>
                valueToHtml(
                  fv,
                  formTemplateId,
                  maybeAccessCode,
                  title,
                  sectionNumber,
                  sectionTitle4Ga
                )
              })
              group(fieldValue, htmlList, orientation, isLabel)

            case _ =>
              valueToHtml(
                fieldValue,
                formTemplateId,
                maybeAccessCode,
                title,
                sectionNumber,
                sectionTitle4Ga
              )
          }
        }

        fieldValue.`type` match {
          case UkSortCode(_)     => sort_code(fieldValue, validate(fieldValue), changeButton)
          case Date(_, _, _)     => date(fieldValue, validate(fieldValue), changeButton)
          case Address(_)        => address(fieldValue, validate(fieldValue), changeButton)
          case Text(_, _, _, _)  => text(fieldValue, validate(fieldValue), changeButton)
          case TextArea(_, _, _) => textarea(fieldValue, validate(fieldValue), changeButton)

          case Choice(_, options, _, _, _) =>
            val selections = options.toList.zipWithIndex
              .map {
                case (option, index) =>
                  validate(fieldValue)
                    .flatMap(_.getOptionalCurrentValue(fieldValue.id.value + index.toString))
                    .map(_ => option.value)
              }
              .collect { case Some(selection) => selection }

            choice(fieldValue, selections, changeButton)

          case rc @ RevealingChoice(os) =>
            val selections: List[String] = os
              .map(_.choice)
              .zipWithIndex
              .map {
                case (option, index) =>
                  validate(fieldValue)
                    .flatMap(_.getOptionalCurrentValue(fieldValue.id.value + index.toString))
                    .map(_ => option)
              }
              .collect { case Some(selection) => selection.value }

            val hiddenFieldInfo = for {
              field <- RevealingChoice.slice(fieldValue.id)(data.data)(rc)
            } yield valueToHtml(field, formTemplateId, maybeAccessCode, title, sectionNumber, sectionTitle4Ga)

            val listOfHtml = choice(fieldValue, selections, changeButton) :: hiddenFieldInfo
            revealingChoice(listOfHtml)

          case f @ FileUpload()         => file_upload(fieldValue, f, validate(fieldValue), changeButton)
          case InformationMessage(_, _) => Html("")
          case Group(_, _, _, _, _, _)  => groupToHtml(fieldValue, fieldValue.presentationHint.getOrElse(Nil))

          case h @ HmrcTaxPeriod(_, _, _) =>
            val periodId = TaxPeriodHelper.formatTaxPeriodOutput(validate(fieldValue))
            val maybeObligation = obligations.findByPeriodKey(h, periodId)
            hmrc_tax_period(fieldValue, validate(fieldValue), changeButton, maybeObligation)
        }
      }

      def showOnSummary(fieldValue: FormComponent) =
        fieldValue.presentationHint
          .fold(false)(x => x.contains(InvisibleInSummary))

      val sectionsToRender =
        sections.zipWithIndex.collect {
          case (section, index) if data.isVisible(section) => (section, index)
        }

      sectionsToRender
        .flatMap {
          case (section, index) =>
            val sectionTitle4Ga = sectionTitle4GaFactory(sections(index).title.value)
            val begin = begin_section(section.shortName.getOrElse(section.title).value)
            val end = end_section()

            val middle =
              section.fields
                .filterNot(showOnSummary)
                .map(
                  valueToHtml(
                    _,
                    formTemplate._id,
                    maybeAccessCode,
                    section.shortName.getOrElse(section.title).value,
                    SectionNumber(index),
                    sectionTitle4Ga))
            begin +: middle :+ end
        }

    }

    val sections = RepeatingComponentService.getAllSections(formTemplate, data)

    val fields = sections.flatMap(RepeatingComponentService.atomicFields(_, data.data))

    renderHtmls(sections, fields)
  }
}
