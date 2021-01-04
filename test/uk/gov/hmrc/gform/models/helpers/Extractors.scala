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

package uk.gov.hmrc.gform.models.helpers

import play.twirl.api.Html

object Extractors {
  val NameExtractor = "name=\"(.*)\"".r.unanchored

  def extractNames(html: List[Html]): List[String] = html.flatMap { html =>
    html.body match {
      case NameExtractor(name) => List(name)
      case otherwise           => Nil
    }
  }

  val testStringValueR = "Test!([^!]*)!Test".r.unanchored

  def extractAllTestStringValues(html: List[Html]): List[String] =
    for {
      h <- html
      m <- testStringValueR.findAllIn(h.body).matchData
    } yield m.group(1)

  val hrefExtractor = """href="([^"]*)"""".r.unanchored

  def extractAllHrefs(html: List[Html]): List[String] =
    for {
      h <- html
      m <- hrefExtractor.findAllIn(h.body).matchData
    } yield m.group(1)

  val dateR = "(\\d{2})\\W+(\\w+)\\W+(\\d{4})".r.unanchored

  def extractDates(html: List[Html]): List[(String, String, String)] =
    for {
      h <- html
      m <- dateR.findFirstMatchIn(h.body)
    } yield (m.group(1), m.group(2), m.group(3))

}
