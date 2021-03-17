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

package uk.gov.hmrc.gform.summary

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.PdfHtml
import uk.gov.hmrc.gform.summary.HtmlSupport._

import scala.collection.JavaConverters._

trait HtmlSupport {

  implicit class PdfHtmlOps(pdfHtml: PdfHtml) {
    val document = Jsoup.parse(pdfHtml.html)

    val title = document.title()

    def summaryElements: List[SummaryElement] =
      buildSummaryLists(document.select("h2,dl").asScala)

    def buildSummaryLists(summaryElms: Seq[Element]): List[SummaryElement] =
      summaryElms.map { e =>
        e.tagName() match {
          case "h2" => buildHeaderElement(e)
          case "dl" => buildSummaryListElement(e)
        }
      }.toList

    def buildSummaryListElement(element: Element): SummaryListElement = {
      val rows = element.select("div").asScala
      SummaryListElement(rows.flatMap { row =>
        for {
          key   <- row.select("dt").asScala.headOption.map(_.text())
          value <- row.select("dd").asScala.headOption.map(_.text())
        } yield SummaryListRow(key, value)
      }.toList)
    }
  }

  implicit class HtmlOps(html: Html) {

    val document = Jsoup.parse(html.body)

    val title = document.title()

    def summaryElements: List[SummaryElement] =
      buildSummaryLists(
        document.select("h2[class='govuk-heading-m'],dl[class='govuk-summary-list govuk-!-margin-bottom-5']").asScala
      )

    def buildSummaryLists(summaryElms: Seq[Element]): List[SummaryElement] =
      summaryElms.map { e =>
        e.tagName() match {
          case "h2" => buildHeaderElement(e)
          case "dl" => buildSummaryListElement(e)
        }
      }.toList

    def buildSummaryListElement(element: Element): SummaryListElement = {
      val rows = element.getElementsByClass("govuk-summary-list__row").asScala
      SummaryListElement(rows.flatMap { row =>
        for {
          key   <- row.getElementsByClass("govuk-summary-list__key").asScala.headOption.map(_.text())
          value <- row.getElementsByClass("govuk-summary-list__value").asScala.headOption.map(_.text())
        } yield SummaryListRow(key, value)
      }.toList)
    }
  }

  def buildHeaderElement(element: Element): HeaderElement =
    HeaderElement(element.text())
}

object HtmlSupport {

  trait SummaryElement
  case class HeaderElement(value: String) extends SummaryElement
  case class SummaryListElement(rows: List[SummaryListRow]) extends SummaryElement
  case class SummaryListRow(key: String, value: String)
}
