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

package uk.gov.hmrc.gform.models

import play.api.i18n.Messages
import play.api.mvc.Results.Ok
import play.api.mvc.{ Request, Result }
import play.twirl.api.Html
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.helpers.Fields._
import uk.gov.hmrc.gform.models.helpers.Javascript.fieldJavascript
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext, Future }

case class SummaryForRender(snippets: List[Html], javascripts: Future[String], totalPage: Int)

object SummaryForRender {

  def apply(f: FieldValue => Option[FormFieldValidationResult], data: Map[FieldId, Seq[String]], formId: FormId, formTemplate: FormTemplate, repeatService: RepeatingComponentService, envelope: Envelope, lang: Option[String])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SummaryForRender] = {

    repeatService.getAllSections(formTemplate, data).map { sections =>
      val fields: List[FieldValue] = sections.flatMap(repeatService.atomicFields)

      def valueToHtml(fieldValue: FieldValue): Html = {

        def groupToHtml(fieldValue: FieldValue, presentationHint: List[PresentationHint]): Html = fieldValue.`type` match {
          case group: Group if presentationHint contains SummariseGroupAsGrid =>
            val value = group.fields.map(f(_))
            uk.gov.hmrc.gform.views.html.snippets.summary.group_grid(fieldValue, value)
          case groupField @ Group(_, orientation, _, _, _, _) => {
            val fvs = repeatService.getAllFieldsInGroupForSummary(fieldValue, groupField)
            val htmlList = fvs.map {
              case (fv: FieldValue) => valueToHtml(fv)
            }.toList
            uk.gov.hmrc.gform.views.html.snippets.summary.group(fieldValue, htmlList, orientation)
          }
          case _ => valueToHtml(fieldValue)
        }

        fieldValue.`type` match {
          case UkSortCode(_) => uk.gov.hmrc.gform.views.html.snippets.summary.sort_code(fieldValue, f(fieldValue))
          case Date(_, _, _) => uk.gov.hmrc.gform.views.html.snippets.summary.date(fieldValue, f(fieldValue))
          case Address(_) => uk.gov.hmrc.gform.views.html.snippets.summary.address(fieldValue, f(fieldValue))
          case t @ Text(_, _) =>
            val x = f(fieldValue).map(_.getCurrentValue).getOrElse("")
            x
            uk.gov.hmrc.gform.views.html.snippets.summary.text(fieldValue, t, f(fieldValue))
          case Choice(_, options, _, _, _) =>
            val selections = options.toList.zipWithIndex.map {
              case (option, index) =>
                f(fieldValue).flatMap(_.getOptionalCurrentValue(fieldValue.id.value + index.toString)).map(_ => option)
            }.collect { case Some(selection) => selection }

            uk.gov.hmrc.gform.views.html.snippets.summary.choice(fieldValue, selections)
          case FileUpload() => {
            uk.gov.hmrc.gform.views.html.snippets.summary.text(fieldValue, Text(AnyText, Constant("file")), f(fieldValue))
          }
          case InformationMessage(_, _) => Html("")
          case Group(_, _, _, _, _, _) => groupToHtml(fieldValue, fieldValue.presentationHint.getOrElse(Nil))
        }
      }

      def showOnSummary(fieldValue: FieldValue) =
        fieldValue.presentationHint
          .fold(false)(x => x.contains(InvisibleInSummary))

      val snippets: List[Html] = {
        val allSections = sections.zipWithIndex
        val sectionsToRender = allSections.filter {
          case (section, idx) => BooleanExpr.isTrue(section.includeIf.getOrElse(IncludeIf(IsTrue)).expr, data)
        }
        sectionsToRender.flatMap {
          case (section, index) =>

            uk.gov.hmrc.gform.views.html.snippets.summary.begin_section(formTemplate._id, formId, section.shortName.getOrElse(section.title), section.description, index, sections.size, lang) ::
              section.fields.filterNot(showOnSummary)
              .map {
                valueToHtml(_)
              } ++
              List(uk.gov.hmrc.gform.views.html.snippets.summary.end_section(formTemplate._id, formId, section.title, index))
        }
      }
      val cacheMap: Future[CacheMap] = repeatService.getAllRepeatingGroups
      val repeatingGroups: Future[List[List[List[FieldValue]]]] = Future.sequence(sections.flatMap(_.fields).map(fv => (fv.id, fv.`type`)).collect {
        case (fieldId, group: Group) => cacheMap.map(_.getEntry[List[List[FieldValue]]](fieldId.value).getOrElse(Nil))
      })
      SummaryForRender(snippets, fieldJavascript(fields, repeatingGroups), sections.size)
    }
  }
}

case class Summary(formTemplate: FormTemplate) {
  def summaryForRender(f: FieldValue => Option[FormFieldValidationResult], formFields: Map[FieldId, Seq[String]], formId: FormId, repeatService: RepeatingComponentService, envelope: Envelope, lang: Option[String])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SummaryForRender] =
    SummaryForRender(f, formFields, formId, formTemplate, repeatService, envelope, lang)

  def renderSummary(f: FieldValue => Option[FormFieldValidationResult], formFields: Map[FieldId, Seq[String]], formId: FormId, repeatService: RepeatingComponentService, envelope: Envelope, lang: Option[String])(implicit request: Request[_], messages: Messages, hc: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    summaryForRender(f, formFields, formId, repeatService, envelope, lang).map { summaryForRender =>
      Ok(uk.gov.hmrc.gform.views.html.summary(formTemplate, summaryForRender, formId, formTemplate.formCategory.getOrElse(Default), lang))
    }
  }
}