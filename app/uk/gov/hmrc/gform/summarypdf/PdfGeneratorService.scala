/*
 * Copyright 2023 HM Revenue & Customs
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

import org.apache.pekko.stream.scaladsl.{ Source, StreamConverters }
import org.apache.pekko.util.ByteString
import com.openhtmltopdf.pdfboxout.PdfRendererBuilder
import org.jsoup.Jsoup
import org.jsoup.helper.W3CDom
import play.api.Environment
import uk.gov.hmrc.gform.sharedmodel.PdfContent

import java.io.{ ByteArrayOutputStream, OutputStream }
import scala.concurrent.{ ExecutionContext, Future }

class PdfGeneratorService(environment: Environment) {

  def generatePDF(pdfContent: PdfContent)(implicit ec: ExecutionContext): Future[Source[ByteString, Unit]] = Future {
    StreamConverters.asOutputStream().mapMaterializedValue { os =>
      Future {
        build(pdfContent.content, os)
      }
      ()
    }
  }

  def generateByteArrayPDF(pdfContent: PdfContent)(implicit ec: ExecutionContext): Future[ByteArrayOutputStream] =
    Future {
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      build(pdfContent.content, baos)
      baos
    }

  private def build(content: String, os: OutputStream): Unit =
    try {
      val w3cDom = new W3CDom().fromJsoup(Jsoup.parse(content))
      val builder = new PdfRendererBuilder()
      builder.useFastMode()
      builder.usePdfUaAccessibility(true)
      builder.usePdfAConformance(PdfRendererBuilder.PdfAConformance.PDFA_3_U)
      builder.useFont(() => environment.classLoader.getResourceAsStream("arial.ttf"), "Arial")
      builder.withW3cDocument(w3cDom, null) //https://github.com/danfickle/openhtmltopdf/issues/341
      builder.toStream(os)
      builder.run()
    } finally os.close()
}

object PdfGeneratorService {
  val css: String =
    """|body{font-family:Arial,sans-serif;font-size: 16px;margin:50px;}
       |dl{border-bottom: 1px solid #bfc1c3;}
       |dt{font-weight: bold;}
       |dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:15px;}
    """.stripMargin
}
