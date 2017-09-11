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
import play.twirl.api.Html
import play.api.i18n.Messages
import play.api.mvc.Results.Ok
import play.api.mvc.{ Request, Result }
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.models.helpers.Javascript.fieldJavascript
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.views.html.summary.snippets._
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext, Future }

case class SummaryForRender(snippets: List[Html], javascripts: Future[String], totalPage: Int)

object SummaryForRender {

  def apply(validatedType: ValidatedType, data: Map[FieldId, Seq[String]], formId: FormId, formTemplate: FormTemplate, repeatService: RepeatingComponentService, envelope: Envelope, lang: Option[String])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SummaryForRender] = {

    repeatService.getAllSections(formTemplate, data).map { sections =>
      val fields: List[FieldValue] = sections.flatMap(repeatService.atomicFields)

      def validate(fieldValue: FieldValue) = {
        val gformErrors = validatedType match {
          case Invalid(errors) => errors
          case Valid(()) => Map.empty[FieldId, Set[String]]
        }
        Fields.valuesValidate(data, fields, envelope, gformErrors)(fieldValue)
      }

      def valueToHtml(fieldValue: FieldValue): Html = {

        def groupToHtml(fieldValue: FieldValue, presentationHint: List[PresentationHint]): Html = fieldValue.`type` match {
          case group: Group if presentationHint contains SummariseGroupAsGrid =>
            val value = group.fields.map(validate(_))
            group_grid(fieldValue, value)
          case groupField @ Group(_, orientation, _, _, _, _) => {
            val fvs = repeatService.getAllFieldsInGroupForSummary(fieldValue, groupField)
            val htmlList = fvs.map {
              case (fv: FieldValue) => valueToHtml(fv)
            }.toList
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

            begin_section(formTemplate._id, formId, section.shortName.getOrElse(section.title), section.description, index, sections.size, lang) ::
              section.fields.filterNot(showOnSummary)
              .map {
                valueToHtml(_)
              } ++
              List(end_section(formTemplate._id, formId, section.title, index))
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

  def generateHTML(f: FieldValue => Option[FormFieldValidationResult], formId: FormId, formFields: Map[FieldId, Seq[String]], repeatService: RepeatingComponentService, envelope: Envelope, lang: Option[String])(implicit request: Request[_], messages: Messages, hc: HeaderCarrier, ec: ExecutionContext): Future[Html] = {
    summaryForRender(f, formFields, formId, repeatService, envelope, lang).map { summaryForRender =>
      uk.gov.hmrc.gform.views.html.summary(formTemplate, summaryForRender, formId, formTemplate.formCategory.getOrElse(Default), lang)
    }
  }

  def renderSummary(f: FieldValue => Option[FormFieldValidationResult], formFields: Map[FieldId, Seq[String]], formId: FormId, repeatService: RepeatingComponentService, envelope: Envelope, lang: Option[String])(implicit request: Request[_], messages: Messages, hc: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    generateHTML(f, formId, formFields, repeatService, envelope, lang).map(Ok(_))
  }
}