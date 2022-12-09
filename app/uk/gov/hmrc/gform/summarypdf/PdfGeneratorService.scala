/*
 * Copyright 2022 HM Revenue & Customs
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

import akka.stream.scaladsl.{ Source, StreamConverters }
import akka.util.ByteString
import com.openhtmltopdf.pdfboxout.PdfRendererBuilder
import play.api.Environment
import uk.gov.hmrc.gform.sharedmodel.PdfHtml

import scala.concurrent.{ ExecutionContext, Future }

class PdfGeneratorService(environment: Environment) {

  def generatePDF(pdfHtml: PdfHtml)(implicit ec: ExecutionContext): Future[Source[ByteString, Unit]] = Future {
    StreamConverters.asOutputStream().mapMaterializedValue { os =>
      Future {
        val builder = new PdfRendererBuilder()
        builder.usePdfUaAccessbility(true)
        builder.usePdfAConformance(PdfRendererBuilder.PdfAConformance.PDFA_3_U)
        builder.useFont(() => environment.classLoader.getResourceAsStream("arial.ttf"), "Arial")
        builder.useFastMode()
        builder.withHtmlContent(pdfHtml.html.replaceAllLiterally("<br>", "<br/>"), null)
        builder.toStream(os)
        builder.run()
      }
      ()
    }
  }
}

object PdfGeneratorService {
  val css: String =
    """|body{font-family:Arial,sans-serif;font-size: 16px;margin:50px;}
       |dl{border-bottom: 1px solid #bfc1c3;}
       |dt{font-weight: bold;}
       |dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:15px;}
    """.stripMargin
}
