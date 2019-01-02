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

package uk.gov.hmrc.gform.views

import play.twirl.api.{ Html, HtmlFormat }

package object html {

  // No lang support yet
  def localisation(str: String): String =
    str.split('|') match {
      case Array()      => ""
      case Array(s, _*) => s.trim
    }

  def summaryTextArea(str: String): Html = {
    val replaceBy = "<br/>"
    val escaped = HtmlFormat.escape(str).body

    // https://stackoverflow.com/a/14217315/2522894
    val replaced = List("\r\n", "\r", "\n").foldLeft(escaped) {
      case (acc, seq) => acc.replaceAll(seq, replaceBy)
    }

    Html(replaced)
  }
}
