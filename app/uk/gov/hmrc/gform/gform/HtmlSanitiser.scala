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
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluationSyntax, SmartStringEvaluator }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService

object HtmlSanitiser {
  def sanitiseHtmlForPDF(html: Html, modify: Document => Unit): String = {
    val doc: Document = Jsoup.parse(html.body)
    removeComments(doc)

    val body = doc.select(".govuk-template__body").first()
    val maybeLogo = Option(doc.select(".hmrc-organisation-logo").first())
    val form = doc.getElementsByTag("form").first()

    form.getElementsByClass("govuk-body").remove()
    form.getElementsByTag("input").remove()
    form.clearAttributes()

    doc.select(".hmrc-banner").remove()
    doc.getElementsByTag("link").remove()
    doc.getElementsByTag("meta").remove()
    doc.getElementsByTag("noscript").remove()
    doc.getElementsByTag("script").remove()
    doc.getElementsByTag("a").remove()
    doc.getElementsByTag("header").remove()
    doc.getElementsByTag("button").remove()
    doc.getElementsByClass("govuk-phase-banner").remove()
    doc.getElementsByClass("hmrc-language-select").remove()
    doc.getElementsByClass("govuk-footer").remove()
    doc
      .getElementsByTag("head")
      .append(s"<style>${PdfGeneratorService.css}</style>")

    doc.select(".govuk-width-container").remove()

    maybeLogo.foreach(logo => body.append(logo.toString))
    body.append(form.toString)

    modify(doc) // doc is mutable data structure
    doc.html.replace("£", "&pound;")
  }

  def summaryPagePdf(doc: Document, formTemplate: FormTemplate)(
    implicit l: LangADT,
    sse: SmartStringEvaluator): Unit = {
    val headerHtml = markDownParser(formTemplate.summarySection.header).toString

    val form = doc.getElementsByTag("form")
    form.prepend(
      h1(formTemplate.formName.value) +
        h1(formTemplate.summarySection.title.value) +
        headerHtml)
  }

  def acknowledgementPdf(doc: Document, extraData: String, declarationExtraData: String, formTemplate: FormTemplate)(
    implicit l: LangADT,
    sse: SmartStringEvaluator): Unit = {

    val pdf = formTemplate.destinations match {
      case d: DestinationList => d.acknowledgementSection.pdf.map(p => (p.header, p.footer))
      case _                  => None
    }

    val headerHtml = pdf.flatMap(_._1.map(markDownParser(_).toString))
    val footerHtml = pdf.flatMap(_._2.map(markDownParser(_).toString))

    val form = doc.getElementsByTag("form").first()
    headerHtml.map(header => form.prepend(header))
    form.prepend(h1(formTemplate.formName.value))
    form.append(extraData + declarationExtraData)
    footerHtml.map(footer => form.append(footer))
  }

  def printSectionPdf(doc: Document, headerHtml: String, footerHtml: String): Unit = {
    val form = doc.getElementsByTag("form").first()
    form.prepend(headerHtml)
    form.append(footerHtml)
  }

  private def h1(content: String): String = s"""<h1 class="govuk-heading-l">$content</h1>"""

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
