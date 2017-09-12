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

import play.api.i18n.Messages
import play.api.mvc.Results.Ok
import play.api.mvc.{ Request, Result }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.views.html.summary._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

case class Summary(formTemplate: FormTemplate) {
  def summaryForRender(validatedType: ValidatedType, formFields: Map[FieldId, Seq[String]], formId: FormId, repeatService: RepeatingComponentService, envelope: Envelope, lang: Option[String])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SummaryForRender] =
    SummaryForRender(validatedType, formFields, formId, formTemplate, repeatService, envelope, lang)

  def renderSummary(validatedType: ValidatedType, formFields: Map[FieldId, Seq[String]], formId: FormId, repeatService: RepeatingComponentService, envelope: Envelope, lang: Option[String])(implicit request: Request[_], messages: Messages, hc: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    summaryForRender(validatedType, formFields, formId, repeatService, envelope, lang).map { summaryForRender =>
      Ok(summary(formTemplate, summaryForRender, formId, formTemplate.formCategory.getOrElse(Default), lang))
    }
  }
}
