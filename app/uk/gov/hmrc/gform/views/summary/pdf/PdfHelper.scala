/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.views.summary.pdf

import org.apache.commons.text.StringEscapeUtils
import org.jsoup.Jsoup
import org.jsoup.nodes.{ Element, TextNode }
import play.twirl.api.XmlFormat

import scala.jdk.CollectionConverters.CollectionHasAsScala

object PdfHelper {
  def renderHtml(value: String): XmlFormat.Appendable = {
    val newLineDelimiters = List("<br>", "\n\n")
    val maybeDelimiter = newLineDelimiters.find(value.contains)

    val lines: Array[String] = maybeDelimiter match {
      case Some(delimiter) => value.split(delimiter)
      case None            => Array(value)
    }

    XmlFormat.fill(lines.toList.map(l => XmlFormat.raw(StringEscapeUtils.escapeXml11(insertZeroWidthSpace(l)))))
  }

  def sanitiseHtml(input: String): String =
    insertZeroWidthSpace(StringEscapeUtils.unescapeHtml4(input))

  //The text is a block has no spaces, so FOP finds no place where it can break it.
  //Possible solutions: insert some zero-width spaces &#x200B; in the text
  def insertZeroWidthSpace(input: String): String =
    input.map(c => s"$c\u200B").mkString

  def convertFOP(html: String): String = {
    val document = Jsoup.parse(html)
    val element = document.body()
    convertElement(element)
  }

  private def convertElement(element: Element): String =
    element.tagName() match {
      case "div" =>
        s"<fo:block role='Div'>${convertChildren(element)}</fo:block>"
      case "p" =>
        s"<fo:block role='P' padding-before='0.2cm'>${convertChildren(element)}</fo:block>"
      case "b" | "strong" =>
        s"<fo:inline font-weight='bold'>${convertChildren(element)}</fo:inline>"
      case "i" | "em" =>
        s"<fo:inline font-style='italic'>${convertChildren(element)}</fo:inline>"
      case "span" =>
        s"<fo:inline>${convertChildren(element)}</fo:inline>"
      case "br" =>
        "<fo:block>&#x00A0;</fo:block>"
      case "h1" =>
        s"<fo:block role='H1' font-size='22px' line-height='22pt' font-weight='bold' padding-after='0.4cm' padding-before='0.4cm'>${convertChildren(element)}</fo:block>"
      case "h2" =>
        s"<fo:block role='H2' font-size='16pt' line-height='16pt' font-weight='bold' padding-after='0.4cm' padding-before='0.4cm'>${convertChildren(element)}</fo:block>"
      case "h3" =>
        s"<fo:block role='H3' font-size='14pt' line-height='14pt' font-weight='bold' padding-after='0.4cm' padding-before='0.4cm'>${convertChildren(element)}</fo:block>"
      case "ul" =>
        s"<fo:list-block role='L' padding-before='0.4cm' padding-after='0.4cm'>${element
          .children()
          .asScala
          .map { li =>
            s"<fo:list-item role='LI' padding-after='0.2cm'><fo:list-item-label end-indent='label-end()'><fo:block>•</fo:block></fo:list-item-label><fo:list-item-body start-indent='body-start()'><fo:block>${liConvertChildren(li)}</fo:block></fo:list-item-body></fo:list-item>"
          }
          .mkString("\n")}</fo:list-block>"
      case "ol" =>
        s"<fo:list-block role='L' padding-before='0.4cm' padding-after='0.4cm'>${element
          .children()
          .asScala
          .zipWithIndex
          .map { case (li, idx) =>
            s"<fo:list-item role='LI' padding-after='0.2cm'><fo:list-item-label end-indent='label-end()'><fo:block>${idx + 1}.</fo:block></fo:list-item-label><fo:list-item-body start-indent='body-start()'><fo:block>${liConvertChildren(li)}</fo:block></fo:list-item-body></fo:list-item>"
          }
          .mkString("\n")}</fo:list-block>"
      case _ =>
        convertChildren(element)
    }

  private def convertChildren(element: Element): String =
    element
      .childNodes()
      .asScala
      .map {
        case textNode: TextNode => textNode.text()
        case elem: Element      => convertElement(elem)
        case _                  => ""
      }
      .mkString

  private def liConvertChildren(element: Element): String =
    element
      .childNodes()
      .asScala
      .map {
        case textNode: TextNode => textNode.text()
        case elem: Element =>
          elem.tagName() match {
            case "p" => liConvertChildren(elem)
            case _   => convertElement(elem)
          }
        case _ => ""
      }
      .mkString

}
