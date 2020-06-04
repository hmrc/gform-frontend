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

package uk.gov.hmrc.gform.sharedmodel

import cats.Applicative
import play.api.libs.json._

sealed trait ServiceCallResponse[+A] extends Product with Serializable

case object NotFound extends ServiceCallResponse[Nothing]
case object CannotRetrieveResponse extends ServiceCallResponse[Nothing]
case class ServiceResponse[A](value: A) extends ServiceCallResponse[A]

object ServiceCallResponse {

  implicit val applicative: Applicative[ServiceCallResponse] = new Applicative[ServiceCallResponse] {
    def ap[A, B](ff: ServiceCallResponse[A => B])(fa: ServiceCallResponse[A]): ServiceCallResponse[B] = (ff, fa) match {
      case (ServiceResponse(f), ServiceResponse(a)) => ServiceResponse(f(a))
      case (CannotRetrieveResponse, _)              => CannotRetrieveResponse
      case (_, CannotRetrieveResponse)              => CannotRetrieveResponse
      case (NotFound, _)                            => NotFound
      case (_, NotFound)                            => NotFound
    }
    def pure[A](a: A): ServiceCallResponse[A] = ServiceResponse(a)
  }

  implicit def format[A: Writes: Reads]: OFormat[ServiceCallResponse[A]] = new OFormat[ServiceCallResponse[A]] {
    override def writes(o: ServiceCallResponse[A]): JsObject =
      o match {
        case NotFound               => Json.obj("det" -> "NotFound")
        case CannotRetrieveResponse => Json.obj("det" -> "CannotRetrieveResponse")
        case ServiceResponse(a)     => Json.obj("det" -> implicitly[Writes[A]].writes(a))
      }

    override def reads(json: JsValue): JsResult[ServiceCallResponse[A]] =
      json \ "det" match {
        case JsDefined(js) =>
          js match {
            case JsString("NotFound")               => JsSuccess(NotFound: ServiceCallResponse[A])
            case JsString("CannotRetrieveResponse") => JsSuccess(CannotRetrieveResponse: ServiceCallResponse[A])
            case jsObject: JsValue                  => implicitly[Reads[A]].reads(jsObject).map(ServiceResponse.apply)
            case _                                  => JsError("Not Supported json: " + Json.prettyPrint(json))
          }

        case JsUndefined() => JsError("Not Supported json: " + Json.prettyPrint(json))
      }
  }
}
