/*
 * Copyright 2020 HM Revenue & Customs
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
    val formName = doc.select(".govuk-header__link--service-name").first().text()
    val hmrcLogoText = doc.select(".govuk-body-s").first().text()
    val headerName = doc.select(".govuk-heading-l").first().text()
    val message = doc.select(".govuk-body").first().text()
    doc.prepend(s"<p>$message</p>")
    doc.prepend(s"<h2>$headerName</h2>")
    doc.prepend(s"<h2>$formName</h2>")
    doc.prepend(s"<p>$hmrcLogoText</p>")
    removeComments(doc)
    doc.getElementsByTag("link").remove()
    doc.getElementsByTag("meta").remove()
    doc.getElementsByTag("script").remove()
    doc.getElementsByTag("a").remove()
    doc.getElementsByTag("header").remove()
    doc.getElementsByTag("button").remove()
    doc.getElementsByClass("govuk-heading-l").remove()
    doc.getElementsByClass("govuk-phase-banner").remove()
    doc.getElementsByClass("hmrc-language-select").remove()
    doc.getElementsByClass("govuk-body-s").remove()
    doc.getElementsByClass("govuk-body").remove()
    doc.getElementsByClass("govuk-footer").remove()
    doc
      .getElementsByTag("head")
      .append(s"<style>${PdfGeneratorService.css}</style>")
    if (submitted) {
      doc.getElementsByClass("govuk-heading-l").remove()
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
