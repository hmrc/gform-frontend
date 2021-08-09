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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json.{ Format, JsString }
import uk.gov.hmrc.gform.models.ids.ModelPageId
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat

case class PageId(id: String) {
  val modelPageId: ModelPageId = ModelPageId.fromPageId(this)

  def withIndex(index: Int) = PageId(index + "_" + id)
}

object PageId {
  implicit val format: Format[PageId] =
    ValueClassFormat.vformat("id", PageId.apply, p => JsString(p.id))
}
