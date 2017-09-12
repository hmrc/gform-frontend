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

package uk.gov.hmrc.gform.controllers.helpers

import play.api.mvc.Results._
import play.api.mvc.{ AnyContent, Request, Result }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FieldId, Group }
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

object FormDataHelpers {

  //TODO: fix the bug:
  //for choice component, in mongo we have '1,2,3' but in request from browser we have List(1,2,2)
  //however we can't split formField.value by comma because other data could have it in it
  def formDataMap(formData: FormData): Map[FieldId, Seq[String]] =
    formData.fields.map(formField => formField.id -> List(formField.value)).toMap

  def processResponseDataFromBody(request: Request[AnyContent])(continuation: Map[FieldId, Seq[String]] => Future[Result])(implicit hc: HeaderCarrier): Future[Result] = {
    request.body.asFormUrlEncoded.map(_.map { case (a, b) => (FieldId(a), b) }) match {
      case Some(data) => continuation(data)
      case None => Future.successful(BadRequest("Cannot parse body as FormUrlEncoded")) // Thank you play-authorised-frontend for forcing me to do this check
    }
  }

  def get(data: Map[FieldId, Seq[String]], id: FieldId): List[String] =
    data.get(id).toList.flatten

  def anyFormId(data: Map[FieldId, Seq[String]]): Option[FormId] =
    data.get(FieldId("formId")).flatMap(_.filterNot(_.isEmpty()).headOption).map(FormId.apply)

  def dataEnteredInGroup(group: Group, fieldData: Map[FieldId, Seq[String]]): Boolean = {

    group.fields.map(_.id).find(id => {
      fieldData.get(id).isDefined && fieldData.get(id).get.find(!_.isEmpty).isDefined
    }).isDefined
  }

}
