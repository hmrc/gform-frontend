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

package uk.gov.hmrc.gform.gform
import org.jsoup.Jsoup
import org.jsoup.nodes.{ Document, Node }
import play.twirl.api.Html
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService

object HtmlSanitiser {
  def sanitiseHtmlForPDF(html: Html, submitted: Boolean): String = {
    val doc: Document = Jsoup.parse(html.body)
    removeComments(doc)
    doc.getElementsByTag("link").remove()
    doc.getElementsByTag("meta").remove()
    doc.getElementsByTag("script").remove()
    doc.getElementsByTag("a").remove()
    doc.getElementsByTag("header").remove()
    doc.getElementsByClass("service-info").remove()
    doc.getElementsByClass("footer-wrapper").remove()
    doc.getElementById("global-cookie-message").remove()
    doc.getElementsByClass("print-hidden").remove()
    doc.getElementsByClass("report-error").remove()
    doc.getElementById("global-app-error").remove()
    doc
      .getElementsByTag("head")
      .append(s"<style>${PdfGeneratorService.css}</style>")
    if (submitted) {
      doc.getElementsByClass("cya-intro").remove()
    }
    doc.html.replace("£", "&pound;")
  }

  private def removeComments(node: Node): Unit = {
    var i = 0
    while (i < node.childNodeSize()) {
      val child = node.childNode(i)
      if (child.nodeName.equals("#comment")) {
        child.remove()
      } else {
        removeComments(child)
        i += 1
      }
    }
  }
}
