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

package uk.gov.hmrc.gform.summarypdf

import akka.stream.scaladsl.Source
import akka.util.ByteString
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.Request
import play.mvc.Http.{ HeaderNames, MimeTypes }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.gform.{ HtmlSanitiser, SummaryPagePurpose }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, PdfHtml }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.views.html.summary.snippets.pdf_header

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class PdfGeneratorService(
  i18nSupport: I18nSupport,
  pdfGeneratorConnector: PdfGeneratorConnector,
  summaryRenderingService: SummaryRenderingService) {

  import i18nSupport._

  def generatePDF(html: PdfHtml)(implicit hc: HeaderCarrier): Future[Source[ByteString, _]] = {
    val headers = Seq((HeaderNames.CONTENT_TYPE, MimeTypes.FORM))
    val body = Map("html" -> Seq(html))
    pdfGeneratorConnector.generatePDF(body, headers)
  }

  def generateSummaryPDF(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(
    implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[Source[ByteString, _]] =
    for {
      summaryHtml <- summaryRenderingService
                      .getSummaryHTML(maybeAccessCode, cache, SummaryPagePurpose.ForUser, formModelOptics)
      htmlForPDF = HtmlSanitiser
        .sanitiseHtmlForPDF(summaryHtml, document => HtmlSanitiser.summaryPagePdf(document, cache.formTemplate))
      pdfStream <- generatePDF(PdfHtml(htmlForPDF))
    } yield pdfStream

}

object PdfGeneratorService {
  val css: String =
    """|body{font-family:Arial,sans-serif;font-size: 19px;}
       |dl{border-bottom: 1px solid #bfc1c3;}
       |dt{font-weight: bold;}
       |dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;}
    """.stripMargin
}
