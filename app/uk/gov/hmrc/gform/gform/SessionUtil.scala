/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import org.slf4j.{ Logger, LoggerFactory }
import play.api.libs.json.Json.{ fromJson, parse }
import play.api.libs.json.{ JsError, JsSuccess, Reads }
import play.api.mvc.{ AnyContent, Request }

import scala.util.{ Failure, Success, Try }

object SessionUtil {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def jsonFromSession[T: Reads](request: Request[AnyContent], key: String, default: T): T =
    request.session
      .get(key)
      .map { json =>
        Try(parse(json)) match {
          case Success(jsValue) =>
            fromJson[T](jsValue) match {
              case JsSuccess(value, _) => value
              case JsError(errors) =>
                logger.warn(s"Failed to convert JSON to ${default.getClass} for $key: " + errors)
                default
            }
          case Failure(exception) =>
            logger.warn(s"JSON parse failed for session key $key: ", exception)
            default
        }
      }
      .getOrElse(default)

}
