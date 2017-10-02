/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.summarypdf

import akka.stream.scaladsl.Source
import akka.util.ByteString
import org.jsoup.Jsoup
import org.jsoup.nodes.Node
import play.mvc.Http.{ HeaderNames, MimeTypes }
import play.twirl.api.Html
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

class PdfGeneratorService(pdfGeneratorConnector: PdfGeneratorConnector) {

  def generatePDF(html: String)(implicit hc: HeaderCarrier): Future[Source[ByteString, _]] = {
    val headers = Seq((HeaderNames.CONTENT_TYPE, MimeTypes.FORM))
    val body = Map("html" -> Seq(html))
    pdfGeneratorConnector.generatePDF(body, headers)
  }

  def sanitiseHtmlForPDF(html: Html): String = {
    val doc = Jsoup.parse(html.body)
    removeComments(doc)
    doc.getElementsByTag("link").remove()
    doc.getElementsByTag("meta").remove()
    doc.getElementsByTag("script").remove()
    doc.getElementsByTag("a").remove()
    doc.getElementsByClass("footer-wrapper").remove()
    doc.getElementById("global-cookie-message").remove()
    doc.getElementsByClass("print-hidden").remove()
    doc.getElementsByTag("head").append(s"<style>${PdfGeneratorService.css}</style>")

    doc.html
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

object PdfGeneratorService {
  // TODO TDC: Delete application.min.css from source code and only send HTML once the pdf-service is caching CSS
  lazy val css: String = {
    val is = getClass.getResourceAsStream("/reduced-application.min.css")
    scala.io.Source.fromInputStream(is).getLines.mkString
  }

}