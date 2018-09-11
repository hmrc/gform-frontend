/*
 * Copyright 2018 HM Revenue & Customs
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
import org.jsoup.nodes.{ Document, Node }
import play.mvc.Http.{ HeaderNames, MimeTypes }
import play.twirl.api.Html

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

class PdfGeneratorService(pdfGeneratorConnector: PdfGeneratorConnector) {

  def generatePDF(html: String)(implicit hc: HeaderCarrier): Future[Source[ByteString, _]] = {
    val headers = Seq((HeaderNames.CONTENT_TYPE, MimeTypes.FORM))
    val body = Map("html" -> Seq(html))
    pdfGeneratorConnector.generatePDF(body, headers)
  }

  def sanitiseHtmlForPDF(html: Html, submitted: Boolean = false): String = {
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
      doc.getElementsByClass("unsubmitted").remove()
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

object PdfGeneratorService {
  // TODO TDC: Delete application.min.css from source code and only send HTML once the pdf-service is caching CSS
  lazy val old_css: String = {
    val is = getClass.getResourceAsStream("/reduced-application.min.css")
    scala.io.Source.fromInputStream(is).getLines.mkString
  }

  val css: String =
    """|
       |body{font-family:Arial,sans-serif;font-size: 19px;}
       |dl{border-bottom: 1px solid #bfc1c3;}
       |dt{font-weight: bold;}
       |dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;}
       |.pdf-only{display:block;}
    """.stripMargin
}
