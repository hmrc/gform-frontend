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

package uk.gov.hmrc.gform.summary

import cats.data.Validated.{ Invalid, Valid }
import play.api.i18n.Messages
import play.api.mvc.Request
import play.twirl.api.Html
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.ops.FormTemplateIdSyntax
import uk.gov.hmrc.gform.sharedmodel.{ AccessCodeId, UserFormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormDataRecalculated, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.views.html.summary.snippets._
import uk.gov.hmrc.gform.views.html.summary.summary

object SummaryRenderingService {

  def renderSummary(
    formTemplate: FormTemplate,
    validatedType: ValidatedType,
    formFields: FormDataRecalculated,
    userFormTemplateId: UserFormTemplateId,
    maybeAccessCodeId: Option[AccessCodeId],
    envelope: Envelope,
    lang: Option[String],
    frontendAppConfig: FrontendAppConfig
  )(
    implicit
    request: Request[_],
    messages: Messages): Html = {
    val sfr =
      summaryForRender(validatedType, formFields, userFormTemplateId, maybeAccessCodeId, formTemplate, envelope, lang)
    summary(
      formTemplate,
      sfr,
      userFormTemplateId,
      maybeAccessCodeId,
      formTemplate.formCategory.getOrElse(Default),
      lang,
      frontendAppConfig)
  }

  def summaryForRender(
    validatedType: ValidatedType,
    data: FormDataRecalculated,
    userFormTemplateId: UserFormTemplateId,
    maybeAccessCodeId: Option[AccessCodeId],
    formTemplate: FormTemplate,
    envelope: Envelope,
    lang: Option[String]
  ): List[Html] = {

    def renderHtmls(sections: List[Section], fields: List[FormComponent]): List[Html] = {
      def validate(formComponent: FormComponent): Option[FormFieldValidationResult] = {
        val gformErrors = validatedType match {
          case Invalid(errors) => errors
          case Valid(())       => Map.empty[FormComponentId, Set[String]]
        }
        Fields.getValidationResult(data, fields, envelope, gformErrors)(formComponent)
      }

      def valueToHtml(
        fieldValue: FormComponent,
        formTemplateId4Ga: FormTemplateId4Ga,
        userFormTemplateId: UserFormTemplateId,
        maybeAccessCodeId: Option[AccessCodeId],
        title: String,
        sectionNumber: SectionNumber,
        sectionTitle4Ga: SectionTitle4Ga,
        lang: Option[String]): Html = {

        val changeButton = change_button(
          formTemplateId4Ga,
          userFormTemplateId,
          maybeAccessCodeId,
          title,
          sectionNumber,
          sectionTitle4Ga,
          lang,
          fieldValue
        )

        def groupToHtml(fieldValue: FormComponent, presentationHint: List[PresentationHint]): Html = {
          val isLabel = fieldValue.shortName.getOrElse(fieldValue.label).nonEmpty

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

              val value = fcs.map(validate).filterNot(_ == None)

              if (value.nonEmpty) {
                group_grid(fieldValue, value, isLabel, changeButton)
              } else Html("")

            case groupField @ Group(_, orientation, _, _, _, _) =>
              val fvs: List[GroupList] =
                getAllFieldsInGroup(fieldValue, groupField, data)

              val htmlList = fvs.flatMap(_.componentList.map { fv =>
                valueToHtml(
                  fv,
                  formTemplateId4Ga,
                  userFormTemplateId,
                  maybeAccessCodeId,
                  title,
                  sectionNumber,
                  sectionTitle4Ga,
                  lang
                )
              })
              group(fieldValue, htmlList, orientation, isLabel)

            case _ =>
              valueToHtml(
                fieldValue,
                formTemplateId4Ga,
                userFormTemplateId,
                maybeAccessCodeId,
                title,
                sectionNumber,
                sectionTitle4Ga,
                lang
              )
          }
        }

        fieldValue.`type` match {
          case UkSortCode(_)     => sort_code(fieldValue, validate(fieldValue), changeButton)
          case Date(_, _, _)     => date(fieldValue, validate(fieldValue), changeButton)
          case Address(_)        => address(fieldValue, validate(fieldValue), changeButton)
          case Text(_, _, _)     => text(fieldValue, validate(fieldValue), changeButton)
          case TextArea(_, _, _) => textarea(fieldValue, validate(fieldValue), changeButton)
          case Choice(_, options, _, _, _) =>
            val selections = options.toList.zipWithIndex
              .map {
                case (option, index) =>
                  validate(fieldValue)
                    .flatMap(_.getOptionalCurrentValue(fieldValue.id.value + index.toString))
                    .map(_ => option)
              }
              .collect { case Some(selection) => selection }

            choice(fieldValue, selections, changeButton)
          case f @ FileUpload()         => file_upload(fieldValue, f, validate(fieldValue), changeButton)
          case InformationMessage(_, _) => Html("")
          case Group(_, _, _, _, _, _)  => groupToHtml(fieldValue, fieldValue.presentationHint.getOrElse(Nil))
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
            val sectionTitle4Ga = sectionTitle4GaFactory(sections(index).title)
            val begin = begin_section(
              formTemplate._id.to4Ga,
              userFormTemplateId,
              maybeAccessCodeId,
              section.shortName.getOrElse(section.title),
              SectionNumber(index),
              sectionTitle4Ga,
              lang)
            val end = end_section(formTemplate._id, userFormTemplateId, maybeAccessCodeId, section.title, index)

            val middle =
              section.fields
                .filterNot(showOnSummary)
                .map(
                  valueToHtml(
                    _,
                    formTemplate._id.to4Ga,
                    userFormTemplateId,
                    maybeAccessCodeId,
                    section.shortName.getOrElse(section.title),
                    SectionNumber(index),
                    sectionTitle4Ga,
                    lang))
            begin +: middle :+ end
        }

    }

    val sections = RepeatingComponentService.getAllSections(formTemplate, data)

    val fields = sections.flatMap(RepeatingComponentService.atomicFields)

    renderHtmls(sections, fields)
  }
}
