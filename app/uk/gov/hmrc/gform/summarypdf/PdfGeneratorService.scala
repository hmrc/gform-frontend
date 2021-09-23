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

package uk.gov.hmrc.gform.summarypdf

import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.openhtmltopdf.pdfboxout.PdfRendererBuilder
import play.mvc.Http.{ HeaderNames, MimeTypes }
import uk.gov.hmrc.gform.sharedmodel.PdfHtml

import java.io.ByteArrayOutputStream
import scala.concurrent.{ ExecutionContext, Future }

class PdfGeneratorService(
  pdfGeneratorConnector: PdfGeneratorConnector
) {

  def generatePDF(html: PdfHtml): Future[Source[ByteString, _]] = {
    val headers = Seq((HeaderNames.CONTENT_TYPE, MimeTypes.FORM))
    val body = Map("html" -> Seq(html))
    pdfGeneratorConnector.generatePDF(body, headers)
  }

  def generatePDFLocal(pdfHtml: PdfHtml)(implicit ec: ExecutionContext): Future[Array[Byte]] = Future {
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val builder = new PdfRendererBuilder()
    builder.useFastMode()
    builder.withHtmlContent(pdfHtml.html, null)
    builder.toStream(byteArrayOutputStream)
    builder.run()
    byteArrayOutputStream.toByteArray
  }
}

object PdfGeneratorService {
  val css: String =
    """|body{font-family:Arial,sans-serif;font-size: 19px;}
       |dl{border-bottom: 1px solid #bfc1c3;}
       |dt{font-weight: bold;}
       |dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;}
    """.stripMargin
}
