/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.api

import play.api.libs.json.{ JsPath, Json, OFormat, Reads }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OFormatWithTemplateReadFallback

case class Officers(items: List[Officer])

object Officers {
  implicit val format: OFormat[Officers] = Json.format[Officers]
}

case class Officer(officerRole: String, resignedOn: Option[String] = None)

object Officer {
  val reads: Reads[Officer] = for {
    officerRole <- (JsPath \ "officer_role").read[String]
    resignedOn  <- (JsPath \ "resigned_on").readNullable[String]
  } yield Officer(officerRole, resignedOn)

  implicit val format: OFormat[Officer] = OFormatWithTemplateReadFallback(reads)
}
