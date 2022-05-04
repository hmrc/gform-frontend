/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.config.FileInfoConfig

final case class AllowedFileTypes(fileExtensions: NonEmptyList[String]) {

  val allExtensions: Set[String] = fileExtensions.toList.toSet

  val contentTypes: NonEmptyList[ContentType] = FileInfoConfig.contentTypes(fileExtensions)

  def contains(s: String): Boolean = allExtensions(s)
}

object AllowedFileTypes extends JsonUtils {
  implicit val format: OFormat[AllowedFileTypes] = derived.oformat()
}
