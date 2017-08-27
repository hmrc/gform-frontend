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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import play.api.libs.json._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

case class Validators(validatorName: String, errorMessage: String, parameters: Map[String, Seq[FormCtx]]) {

  def getValidator = {
    validatorName match {
      case "hmrcUTRPostcodeCheck" => HMRCUTRPostcodeCheck(parameters("utr").head, parameters("postCode").head, errorMessage)
    }
  }
}

object Validators {

  implicit val format = Json.format[Validators]
}

trait Validator[A] {

  def validate(data: Map[FieldId, Seq[String]])(f: A => Future[Boolean])(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Validated[Map[FieldId, Set[String]], Unit]]
}

case class HMRCUTRPostcodeCheck(utr: FormCtx, postcode: FormCtx, errorMessage: String) extends Validator[(String, String)] {

  val utrFieldId = FieldId(utr.value)
  val postcodeFieldId = FieldId(postcode.value)

  override def validate(data: Map[FieldId, Seq[String]])(f: ((String, String)) => Future[Boolean])(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Validated[Map[FieldId, Set[String]], Unit]] = {
    val dataGetter: FieldId => String = id => data.get(id).toList.flatten.headOption.getOrElse("")
    val utrString = dataGetter(utrFieldId)
    val postCodeString = dataGetter(postcodeFieldId)
    f(utrString -> postCodeString).map(if (_) Valid(()) else Invalid(Map(FieldId("B") -> Set(errorMessage))))
  }
}
