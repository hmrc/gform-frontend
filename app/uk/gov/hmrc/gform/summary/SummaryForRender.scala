/*
 * Copyright 2017 HM Revenue & Customs
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
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.models.helpers.Javascript.fieldJavascript
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.views.html.summary.snippets._
import uk.gov.hmrc.gform.views.html.summary.summary
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

case class SummaryForRender(snippets: List[Html], javascripts: Html, totalPage: Int)

object SummaryRenderingService {

  def renderSummary(
    formTemplate: FormTemplate,
    validatedType: ValidatedType,
    formFields: Map[FormComponentId, Seq[String]],
    formId: FormId,
    repeatService: RepeatingComponentService,
    envelope: Envelope,
    lang: Option[String]
  )(implicit
    request: Request[_],
    messages: Messages,
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Html] = {
    summaryForRender(validatedType, formFields, formId, formTemplate, repeatService, envelope, lang)
      .map(s => summary(formTemplate, s, formId, formTemplate.formCategory.getOrElse(Default), lang))
  }

  def summaryForRender(
    validatedType: ValidatedType,
    data: Map[FormComponentId, Seq[String]],
    formId: FormId,
    formTemplate: FormTemplate,
    repeatService: RepeatingComponentService,
    envelope: Envelope,
    lang: Option[String]
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[SummaryForRender] = {

    repeatService.getAllSections(formTemplate, data).flatMap { sections =>
      val fields: List[FormComponent] = sections.flatMap(repeatService.atomicFields)

      def validate(formComponent: FormComponent): Option[FormFieldValidationResult] = {
        val gformErrors = validatedType match {
          case Invalid(errors) => errors
          case Valid(()) => Map.empty[FormComponentId, Set[String]]
        }
        Fields.getValidationResult(data, fields, envelope, gformErrors)(formComponent)
      }

      def valueToHtml(fieldValue: FormComponent): Html = {

        def groupToHtml(fieldValue: FormComponent, presentationHint: List[PresentationHint]): Html = fieldValue.`type` match {
          case groupField: Group if presentationHint contains SummariseGroupAsGrid =>
            val groups = repeatService.getAllFieldsInGroupForSummary(fieldValue, groupField)
            val value = groups.map(x => x.map(validate)).toList
            val htmlList = value.map { va => group_grid(fieldValue, va) }
            group(fieldValue, htmlList, groupField.orientation)
          case groupField @ Group(_, orientation, _, _, _, _) => {
            val fvs = repeatService.getAllFieldsInGroupForSummary(fieldValue, groupField)
            val htmlList = fvs.flatMap(_.map {
              case (fv: FormComponent) => valueToHtml(fv)
            }.toList)
            group(fieldValue, htmlList, orientation)
          }
          case _ => valueToHtml(fieldValue)
        }

        fieldValue.`type` match {
          case UkSortCode(_) => sort_code(fieldValue, validate(fieldValue))
          case Date(_, _, _) => date(fieldValue, validate(fieldValue))
          case Address(_) => address(fieldValue, validate(fieldValue))
          case t @ Text(_, _) => text(fieldValue, t, validate(fieldValue))
          case Choice(_, options, _, _, _) =>
            val selections = options.toList.zipWithIndex.map {
              case (option, index) =>
                validate(fieldValue).flatMap(_.getOptionalCurrentValue(fieldValue.id.value + index.toString)).map(_ => option)
            }.collect { case Some(selection) => selection }

            choice(fieldValue, selections)
          case FileUpload() => {
            text(fieldValue, Text(AnyText, Constant("file")), validate(fieldValue))
          }
          case InformationMessage(_, _) => Html("")
          case Group(_, _, _, _, _, _) => groupToHtml(fieldValue, fieldValue.presentationHint.getOrElse(Nil))
        }
      }

      def showOnSummary(fieldValue: FormComponent) =
        fieldValue.presentationHint
          .fold(false)(x => x.contains(InvisibleInSummary))

      val snippets: List[Html] = {
        val allSections = sections.zipWithIndex
        val sectionsToRender = allSections.filter {
          case (section, idx) => BooleanExpr.isTrue(section.includeIf.getOrElse(IncludeIf(IsTrue)).expr, data)
        }
        sectionsToRender.flatMap {
          case (section, index) =>

            begin_section(formTemplate._id, formId, section.shortName.getOrElse(section.title), section.description, index, sections.size, lang) ::
              section.fields.filterNot(showOnSummary)
              .map {
                valueToHtml(_)
              } ++
              List(end_section(formTemplate._id, formId, section.title, index))
        }
      }
      val cacheMap: Future[CacheMap] = repeatService.getAllRepeatingGroups
      val repeatingGroups: Future[List[List[List[FormComponent]]]] = Future.sequence(sections.flatMap(_.fields).map(fv => (fv.id, fv.`type`)).collect {
        case (fieldId, group: Group) => cacheMap.map(_.getEntry[List[List[FormComponent]]](fieldId.value).getOrElse(Nil))
      })
      fieldJavascript(fields, repeatingGroups)
        .map(javascript =>
          SummaryForRender(snippets, Html(javascript), sections.size))

    }
  }
}
