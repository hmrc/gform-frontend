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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json.OFormat
import play.twirl.api.Html
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }

case class Save4LaterInfoText(value: LocalisedString) extends AnyVal {
  def asHtml(implicit l: LangADT): Html = markDownParser(value)
}

object Save4LaterInfoText {

  implicit val format: OFormat[Save4LaterInfoText] = derived.oformat

}
