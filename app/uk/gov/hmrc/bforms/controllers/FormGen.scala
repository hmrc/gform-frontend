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

package uk.gov.hmrc.bforms.controllers

import javax.inject.{Inject, Singleton}

import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{ Action, AnyContent }
import uk.gov.hmrc.bforms.models.{FieldValue, FormTypeId}
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.bforms.service.RetrieveService

sealed trait FormFieldValidationResult {
  def isOk = this match {
    case FieldOk(_, _) => true
    case _ => false
  }

  def getCurrentValue = this match {
    case FieldOk(_, cv) => cv
    case _ => ""
  }
}

case class RequiredField(fieldValue: FieldValue) extends FormFieldValidationResult
case class WrongFormat(fieldValue: FieldValue) extends FormFieldValidationResult
case class FieldOk(fieldValue: FieldValue, currentValue: String) extends FormFieldValidationResult

@Singleton
class FormGen @Inject()(val messagesApi: MessagesApi)(implicit ec: ExecutionContext)
  extends FrontendController with I18nSupport {


  def form(formTypeId: FormTypeId, version: String) = ActionWithTemplate(formTypeId, version).apply { implicit request =>

    val formTemplate = request.formTemplate

    val fieldValues = RetrieveService.getFields(formTemplate)
    val snippets = fieldValues.map(fieldValue => uk.gov.hmrc.bforms.views.html.field_template_text(fieldValue, None))

    Ok(uk.gov.hmrc.bforms.views.html.form(formTemplate, snippets))
  }


  def validateFieldValue(fieldValue: FieldValue, formValue: Seq[String]): FormFieldValidationResult = {
    formValue.filterNot(_.isEmpty()) match {
      case Nil => RequiredField(fieldValue)
      case value :: Nil => FieldOk(fieldValue, value)
      case value :: rest => FieldOk(fieldValue, value) // we don't support multiple values yet
    }
  }

  def save(formTypeId: FormTypeId, version: String) = ActionWithTemplate(formTypeId, version).apply(parse.urlFormEncoded) { implicit request =>

    val formTemplate = request.formTemplate
    val data = request.body

    val fieldValues = RetrieveService.getFields(formTemplate)

    val validate: Map[FieldValue, Seq[String]] = fieldValues.map(fv => fv -> data.get(fv.id).toList.flatten).toMap

    val validationResults: Map[FieldValue, FormFieldValidationResult] = validate.map {
      case (fieldValue, values) =>
        fieldValue -> validateFieldValue(fieldValue, values)
    }

    val canSave = validationResults.forall(_._2.isOk)

    if (canSave) {
      Ok("Your form was successfuly submitted")
    } else {
      val snippets = fieldValues.map { fieldValue =>
        val validatioResult: Option[FormFieldValidationResult] = validationResults.get(fieldValue)
        uk.gov.hmrc.bforms.views.html.field_template_text(fieldValue, validatioResult)
      }

      Ok(uk.gov.hmrc.bforms.views.html.form(formTemplate, snippets))
    }
  }
}
