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

package uk.gov.hmrc.gform.views.summary.pdf

import org.apache.commons.text.StringEscapeUtils
import play.twirl.api.{ Html, XmlFormat }
import uk.gov.hmrc.gform.views.xml.summary.pdf.simpleField

object PdfHelper {
  def renderHtml(value: String): XmlFormat.Appendable = {
    val newLineDelimiters = List("<br>", "\n\n")
    val maybeDelimiter = newLineDelimiters.find(value.contains)

    val lines = maybeDelimiter match {
      case Some(delimiter) => value.split(delimiter)
      case None            => return XmlFormat.raw(StringEscapeUtils.unescapeXml(value))
    }

    simpleField(lines.map(StringEscapeUtils.unescapeXml).map(Html(_)).toList)
  }

}
