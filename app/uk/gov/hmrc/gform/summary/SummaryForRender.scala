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
import cats.implicits._
import play.api.i18n.Messages
import play.api.mvc.Request
import play.twirl.api.Html
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.ops.FormTemplateIdSyntax
import uk.gov.hmrc.gform.sharedmodel.Visibility
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.views.html.summary.snippets._
import uk.gov.hmrc.gform.views.html.summary.summary

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

object SummaryRenderingService {

  def renderSummary(
    formTemplate: FormTemplate,
    validatedType: ValidatedType,
    formFields: Map[FormComponentId, Seq[String]],
    retrievals: MaterialisedRetrievals,
    formId: FormId,
    repeatService: RepeatingComponentService,
    envelope: Envelope,
    lang: Option[String],
    frontendAppConfig: FrontendAppConfig
  )(
    implicit
    request: Request[_],
    messages: Messages,
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Html] =
    summaryForRender(validatedType, formFields, retrievals, formId, formTemplate, repeatService, envelope, lang)
      .map(s => summary(formTemplate, s, formId, formTemplate.formCategory.getOrElse(Default), lang, frontendAppConfig))

  def summaryForRender(
    validatedType: ValidatedType,
    data: Map[FormComponentId, Seq[String]],
    retrievals: MaterialisedRetrievals,
    formId: FormId,
    formTemplate: FormTemplate,
    repeatService: RepeatingComponentService,
    envelope: Envelope,
    lang: Option[String]
  )(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[List[Html]] = {

    def renderHtmls(sections: List[Section], fields: List[FormComponent]): Future[List[Html]] = {
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
        formId: FormId,
        title: String,
        sectionNumber: SectionNumber,
        sectionTitle4Ga: SectionTitle4Ga,
        lang: Option[String]): Future[Html] = {

        def groupToHtml(fieldValue: FormComponent, presentationHint: List[PresentationHint]): Future[Html] = {
          val isLabel = fieldValue.shortName.getOrElse(fieldValue.label).nonEmpty

          def groupGrid(formComponents: List[FormComponent]) = {
            val value = formComponents
              .filter { y =>
                val x = validate(y)
                x.isDefined
              }
              .map(validate)
            if (value.nonEmpty) {
              group_grid(fieldValue, value, isLabel)
            } else Html("")
          }

          fieldValue.`type` match {
            case groupField: Group
                if presentationHint.contains(SummariseGroupAsGrid) && groupField.repeatsMax.isDefined =>
              val htmlList: Future[List[Html]] =
                repeatService
                  .getAllFieldsInGroupForSummary(fieldValue, groupField)
                  .map(y =>
                    for {
                      group <- y
                      value = group.map(validate)
                    } yield {
                      group_grid(fieldValue, value, false)
                  })
              htmlList.map(y => repeating_group(y))
            case groupField: Group if presentationHint.contains(SummariseGroupAsGrid) =>
              groupGrid(groupField.fields)
                .pure[Future]
            case groupField @ Group(_, orientation, _, _, _, _) =>
              for {
                fvs <- repeatService.getAllFieldsInGroupForSummary(fieldValue, groupField)
                htmlList <- Future.sequence(fvs.flatMap(_.map {
                             case (fv: FormComponent) =>
                               valueToHtml(
                                 fv,
                                 formTemplateId4Ga,
                                 formId,
                                 title,
                                 sectionNumber,
                                 sectionTitle4Ga,
                                 lang
                               )
                           }.toList))
              } yield group(fieldValue, htmlList, orientation, isLabel)
            case _ =>
              valueToHtml(
                fieldValue,
                formTemplateId4Ga,
                formId,
                title,
                sectionNumber,
                sectionTitle4Ga,
                lang
              )
          }
        }

        val changeButton = if (fieldValue.derived) { Html("") } else {
          change_button(
            formTemplateId4Ga,
            formId,
            title,
            sectionNumber,
            sectionTitle4Ga,
            lang,
            fieldValue.id
          )
        }

        fieldValue.`type` match {
          case UkSortCode(_)  => Future.successful(sort_code(fieldValue, validate(fieldValue), changeButton))
          case Date(_, _, _)  => Future.successful(date(fieldValue, validate(fieldValue), changeButton))
          case Address(_)     => Future.successful(address(fieldValue, validate(fieldValue), changeButton))
          case Text(_, _)     => Future.successful(text(fieldValue, validate(fieldValue), changeButton))
          case TextArea(_, _) => Future.successful(textarea(fieldValue, validate(fieldValue), changeButton))
          case Choice(_, options, _, _, _) =>
            val selections = options.toList.zipWithIndex
              .map {
                case (option, index) =>
                  validate(fieldValue)
                    .flatMap(_.getOptionalCurrentValue(fieldValue.id.value + index.toString))
                    .map(_ => option)
              }
              .collect { case Some(selection) => selection }

            Future.successful(choice(fieldValue, selections, changeButton))
          case f @ FileUpload()         => Future.successful(file_upload(fieldValue, f, validate(fieldValue), changeButton))
          case InformationMessage(_, _) => Future.successful(Html(""))
          case Group(_, _, _, _, _, _)  => groupToHtml(fieldValue, fieldValue.presentationHint.getOrElse(Nil))
        }
      }

      def showOnSummary(fieldValue: FormComponent) =
        fieldValue.presentationHint
          .fold(false)(x => x.contains(InvisibleInSummary))

      val snippetsF: Future[List[Html]] = {

        val visibility = Visibility(sections, data, retrievals.affinityGroup)

        val sectionsToRender = sections.zipWithIndex.collect {
          case (section, index) if visibility.isVisible(section) => (section, index)
        }

        Future
          .traverse(sectionsToRender) {
            case (section, index) =>
              val sectionTitle4Ga = sectionTitle4GaFactory(formTemplate.sections(index).title)
              val begin = begin_section(
                formTemplate._id.to4Ga,
                formId,
                section.shortName.getOrElse(section.title),
                SectionNumber(index),
                sectionTitle4Ga,
                lang)
              val end = end_section(formTemplate._id, formId, section.title, index)

              Future
                .sequence(
                  section.fields
                    .filterNot(showOnSummary)
                    .map(
                      valueToHtml(
                        _,
                        formTemplate._id.to4Ga,
                        formId,
                        section.shortName.getOrElse(section.title),
                        SectionNumber(index),
                        sectionTitle4Ga,
                        lang))
                )
                .map(begin +: _ :+ end)
          }
          .map(x => x.flatten) //TODO ask a better way to do this.
      }
      snippetsF
    }

    for {
      sections <- repeatService.getAllSections(formTemplate, data)
      fields   <- Future.traverse(sections)(repeatService.atomicFields).map(_.flatten)
      htmls    <- renderHtmls(sections, fields)
    } yield htmls
  }
}
