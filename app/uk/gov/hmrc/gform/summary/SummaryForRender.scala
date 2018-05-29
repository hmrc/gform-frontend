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
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.models.helpers.Javascript.fieldJavascript
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, RepeatingGroup }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.views.html.summary.snippets._
import uk.gov.hmrc.gform.views.html.summary.summary
import uk.gov.hmrc.gform.views.html.summaryTextArea
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

case class SummaryForRender(snippets: List[Html], javascripts: Html, totalPage: Int)

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
    ec: ExecutionContext): Future[SummaryForRender] =
    repeatService.getAllSections(formTemplate, data).flatMap { sections =>
      val fields: List[FormComponent] = sections.flatMap(repeatService.atomicFields)

      def validate(formComponent: FormComponent): Option[FormFieldValidationResult] = {
        val gformErrors = validatedType match {
          case Invalid(errors) => errors
          case Valid(())       => Map.empty[FormComponentId, Set[String]]
        }
        Fields.getValidationResult(data, fields, envelope, gformErrors)(formComponent)
      }

      def valueToHtml(fieldValue: FormComponent): Future[Html] = {

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
                fvs      <- repeatService.getAllFieldsInGroupForSummary(fieldValue, groupField)
                htmlList <- Future.sequence(fvs.flatMap(_.map { case (fv: FormComponent) => valueToHtml(fv) }.toList))
              } yield group(fieldValue, htmlList, orientation, isLabel)
            case _ => valueToHtml(fieldValue)
          }
        }

        fieldValue.`type` match {
          case UkSortCode(_) => Future.successful(sort_code(fieldValue, validate(fieldValue)))
          case Date(_, _, _) => Future.successful(date(fieldValue, validate(fieldValue)))
          case Address(_)    => Future.successful(address(fieldValue, validate(fieldValue)))
          case Text(_, _)    => Future.successful(text(fieldValue, validate(fieldValue)))
          case TextArea      => Future.successful(textarea(fieldValue, validate(fieldValue)))
          case Choice(_, options, _, _, _) =>
            val selections = options.toList.zipWithIndex
              .map {
                case (option, index) =>
                  validate(fieldValue)
                    .flatMap(_.getOptionalCurrentValue(fieldValue.id.value + index.toString))
                    .map(_ => option)
              }
              .collect { case Some(selection) => selection }

            Future.successful(choice(fieldValue, selections))
          case f @ FileUpload()         => Future.successful(file_upload(fieldValue, f, validate(fieldValue)))
          case InformationMessage(_, _) => Future.successful(Html(""))
          case Group(_, _, _, _, _, _)  => groupToHtml(fieldValue, fieldValue.presentationHint.getOrElse(Nil))
        }
      }

      def showOnSummary(fieldValue: FormComponent) =
        fieldValue.presentationHint
          .fold(false)(x => x.contains(InvisibleInSummary))

      val snippetsF: Future[List[Html]] = {
        val allSections = sections.zipWithIndex
        val sectionsToRender = allSections.filter {
          case (section, idx) =>
            BooleanExpr.isTrue(section.includeIf.getOrElse(IncludeIf(IsTrue)).expr, data, retrievals)
        }
        Future
          .sequence(sectionsToRender.map {
            case (section, index) =>
              val x = begin_section(
                formTemplate._id,
                formId,
                section.shortName.getOrElse(section.title),
                section.description,
                index,
                sections.size,
                lang)
              Future
                .sequence(
                  section.fields
                    .filterNot(showOnSummary)
                    .map(valueToHtml)
                )
                .map(x => x ++ List(end_section(formTemplate._id, formId, section.title, index)))
                .map(z => x :: z)
          })
          .map(x => x.flatten) //TODO ask a better way to do this.
      }
      val cacheMap: Future[CacheMap] = repeatService.getAllRepeatingGroups
      val repeatingGroups: Future[List[List[List[FormComponent]]]] =
        Future.sequence(sections.flatMap(_.fields).map(fv => (fv.id, fv.`type`)).collect {
          case (fieldId, group: Group) =>
            cacheMap.map(_.getEntry[RepeatingGroup](fieldId.value).map(_.list).getOrElse(Nil))
        })
      fieldJavascript(fields, repeatingGroups)
        .flatMap(javascript => snippetsF.map(snippets => SummaryForRender(snippets, Html(javascript), sections.size)))
    }
}
