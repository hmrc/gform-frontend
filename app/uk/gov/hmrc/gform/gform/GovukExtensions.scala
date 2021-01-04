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

import org.jsoup.Jsoup
import org.jsoup.nodes.{ Document, Element }
import play.twirl.api.Html

object GovukExtensions {

  def insertUnit(html: Html)(unit: String): Html =
    toHtml(insertUnit(html, unit))

  def insertUnit(html: Html, unit: String): Document = {
    val fragment: String = html.body
    val doc: Document = Jsoup.parseBodyFragment(fragment)
    val div: Element = doc.select("div").first();
    div.append(s"""<span class="gform-unit">$unit</span>""");
    doc
  }

  def toHtml(doc: Document): Html = Html(doc.body().html())

}
