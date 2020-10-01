/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.controllers.helpers

import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.show._
import com.softwaremill.quicklens._
import play.api.mvc.Results._
import play.api.mvc.{ AnyContent, Request, Result }
import uk.gov.hmrc.gform.controllers.RequestRelatedData
import uk.gov.hmrc.gform.models.{ DataExpanded, ExpandUtils, FormModel }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormField, FormId, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplate, Group }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

object FormDataHelpers {

  def processResponseDataFromBody(request: Request[AnyContent], formModel: FormModel[DataExpanded])(
    continuation: RequestRelatedData => VariadicFormData[SourceOrigin.OutOfDate] => Future[Result])(
    implicit hc: HeaderCarrier): Future[Result] =
    request.body.asFormUrlEncoded
      .map(_.map { case (a, b) => (a, b.map(_.trim)) }) match {
      case Some(requestData) =>
        val (variadicFormData, requestRelatedData) = buildVariadicFormDataFromBrowserPostData(formModel, requestData)
        continuation(requestRelatedData)(variadicFormData)
      case None =>
        Future.successful(BadRequest("Cannot parse body as FormUrlEncoded"))
    }

  def get(data: Map[FormComponentId, Seq[String]], id: FormComponentId): List[String] =
    data.get(id).toList.flatten

  def anyFormId(data: Map[FormComponentId, Seq[String]]): Option[FormId] =
    data.get(FormComponentId("formId")).flatMap(_.filterNot(_.isEmpty()).headOption).map(FormId.apply)

  def dataEnteredInGroup[S <: SourceOrigin](group: Group, fieldData: VariadicFormData[S]): Boolean =
    group.fields
      .flatMap(_.multiValueId.toModelComponentIds)
      .exists(id => fieldData.get(id).exists(_.exists(!_.isEmpty)))

  def updateFormField(form: Form, updatedFormField: FormField): Form = {
    val updated: Seq[FormField] = form.formData.fields.filterNot(_.id === updatedFormField.id).+:(updatedFormField)
    form.modify(_.formData.fields).setTo(updated)
  }

  private def buildVariadicFormDataFromBrowserPostData(
    formModel: FormModel[DataExpanded],
    requestData: Map[String, Seq[String]]
  ): (VariadicFormData[SourceOrigin.OutOfDate], RequestRelatedData) = {

    val variadicFormComponentIds: Set[ModelComponentId] = formModel.allModelComponentIds
    val multiValueIds: Set[ModelComponentId] = formModel.allMultiSelectionIds

    val xs: List[(Option[(ModelComponentId, VariadicValue)], Option[RequestRelatedData])] = requestData.toList.map {
      case (id, s) =>
        val modelComponentId = ExpandUtils.toModelComponentId(id)

        (variadicFormComponentIds(modelComponentId), multiValueIds(modelComponentId)) match {
          case (true, true) =>
            (
              Some(
                modelComponentId -> VariadicValue.Many(
                  s.toList.mkString(",").split(",").map(_.trim).filterNot(_.isEmpty))),
              None)
          case (true, false) =>
            s.toList match {
              case first :: _ =>
                (Some(modelComponentId -> VariadicValue.One(first)), None)
              case _ =>
                throw new IllegalArgumentException(
                  show"""Got a single value form component ID "$id", with an empty list of values""")
            }
          case (false, _) => (None, Some(RequestRelatedData(Map(id -> s))))
        }
    }

    xs.foldLeft((VariadicFormData.empty[SourceOrigin.OutOfDate], RequestRelatedData.empty)) {
      case ((variadicFormDataAcc, requestRelatedDataAcc), (maybeVar, maybeReq)) =>
        (
          maybeVar.fold(variadicFormDataAcc)(variadicFormDataAcc addValue _),
          maybeReq.fold(requestRelatedDataAcc)(requestRelatedDataAcc + _))
    }
  }
}
