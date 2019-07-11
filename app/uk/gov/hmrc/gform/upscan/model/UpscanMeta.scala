/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.upscan.model

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

case class UpscanMeta(
  reference: String,
  href: String,
  acl: String,
  key: String,
  policy: String,
  xAmzAlgorithm: String,
  xAmzCredentials: String,
  xAmzData: String,
  xAmzMetaCallBackUrl: String,
  xAmzMetaConsumingService: String,
  xAmzSignature: String
)

object UpscanMeta {
  implicit val upscanMetaReads: Reads[UpscanMeta] = (
    (JsPath \ "reference").read[String] and
      (JsPath \ "uploadRequest" \ "href").read[String] and
      (JsPath \ "uploadRequest" \ "fields" \ "acl").read[String] and
      (JsPath \ "uploadRequest" \ "fields" \ "key").read[String] and
      (JsPath \ "uploadRequest" \ "fields" \ "policy").read[String] and
      (JsPath \ "uploadRequest" \ "fields" \ "x-amz-algorithm").read[String] and
      (JsPath \ "uploadRequest" \ "fields" \ "x-amz-credential").read[String] and
      (JsPath \ "uploadRequest" \ "fields" \ "x-amz-date").read[String] and
      (JsPath \ "uploadRequest" \ "fields" \ "x-amz-meta-callback-url").read[String] and
      (JsPath \ "uploadRequest" \ "fields" \ "x-amz-meta-consuming-service").read[String] and
      (JsPath \ "uploadRequest" \ "fields" \ "x-amz-signature").read[String]
  )(UpscanMeta.apply _)
}
