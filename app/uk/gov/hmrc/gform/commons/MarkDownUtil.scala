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

package uk.gov.hmrc.gform.commons

import cats.implicits._
import org.intellij.markdown.flavours.gfm.GFMFlavourDescriptor
import org.intellij.markdown.html.HtmlGenerator
import org.intellij.markdown.html.entities.EntityConverter
import org.intellij.markdown.parser.MarkdownParser
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.twirl.api.Html
import uk.gov.hmrc.gform.controllers.helpers.InvisibleCharsHelper.replaceInvisibleChars

import scala.jdk.CollectionConverters._
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }

object MarkDownUtil {

  private val markdownControlCharacters =
    List("\\", "/", "`", "*", "_", "{", "}", "[", "]", "(", ")", "#", "+", "-", ".", "!")

  private def addTargetToLinks(doc: Document) = {
    val links = doc.getElementsByAttribute("href")
    links.asScala.foreach { element =>
      if (!element.hasAttr("target")) {
        element.attr("target", "_blank")
      }
      if (!element.hasAttr("class")) {
        element.attr("class", "govuk-link")
      }
    }
    doc.getElementsByAttributeValueStarting("href", "/submissions/new-form/").removeAttr("target")
    doc.getElementsByAttributeValueStarting("href", "/submissions/form/").removeAttr("target")
    doc.getElementsByAttributeValueStarting("href", "/submissions/acknowledgement/pdf/").addClass("print-link")
    doc.getElementsByAttributeValueStarting("href", "/submissions/redirect?url=").attr("target", "_self")
    doc
  }

  private def addDefaultClassesToLists(doc: Document) = {
    val listTypes = Seq("ul", "ol")

    listTypes.foreach { elementType =>
      val elements = doc.select(elementType)
      val defaultClass = s"govuk-list govuk-list--${if (elementType == "ul") "bullet" else "number"}"
      elements.asScala.foreach { element =>
        val existingClass = element.attr("class")
        val updatedClass =
          if (existingClass.isEmpty) defaultClass else existingClass
        element.attr("class", updatedClass)
      }
    }
    doc
  }

  private def addDefaultClassesToHeaders(doc: Document) = {
    val headings = Seq("h1" -> "govuk-heading-l", "h2" -> "govuk-heading-m", "h3" -> "govuk-heading-s")

    headings.foreach { case (tag, className) =>
      val element = doc.select(tag)
      val existingClass = element.attr("class")
      if (existingClass.isEmpty)
        element.attr("class", s"$className")
    }
    doc
  }

  private def enhanceHtml(html: String): String = {
    val doc: Document = Jsoup.parse(html)
    addTargetToLinks(doc)
    addDefaultClassesToLists(doc)
    addDefaultClassesToHeaders(doc)
    doc.body().html()
  }

  private def replaceHashesForAcknowledgementSection(input: String): String =
    input
      .split("\n")
      .map { line =>
        val pattern = "(@####|@###|@##|@#)".r
        val index = pattern.findFirstIn(line).map(line.indexOf).getOrElse(-1)

        if (index =!= -1) {
          val matchedLength = pattern.findFirstIn(line).getOrElse("").length
          val text = line.substring(index + matchedLength).replaceAll("#*$", "")

          val classIndex = matchedLength - 1
          classIndex match {
            case 1 => s"""<h1 class="govuk-heading-xl">$text</h1>"""
            case 2 => s"""<h2 class="govuk-heading-l">$text</h2>"""
            case 3 => s"""<h3 class="govuk-heading-m">$text</h3>"""
            case 4 => s"""<h4 class="govuk-heading-s">$text</h4>"""
            case _ => s"""$text"""
          }
        } else {
          line
        }
      }
      .mkString("\n")

  def markDownParser(ls: LocalisedString)(implicit l: LangADT): Html = markDownParser(ls.value)

  def markDownParser(ls: SmartString)(implicit sse: SmartStringEvaluator): Html = {
    val markDownText = removeSpacesForStrong(ls.valueForMarkdown())
    markDownParser(markDownText)
  }

  def markDownParser(markDownText0: String): Html = {
    val normalizedMarkdownText = markDownText0.trim.replaceAll(" +", " ")
    val markDownText = replaceHashesForAcknowledgementSection(normalizedMarkdownText)
    val flavour = new GFMFlavourDescriptor
    val parsedTree = new MarkdownParser(flavour).buildMarkdownTreeFromString(markDownText)
    val html = new HtmlGenerator(markDownText, parsedTree, flavour, false).generateHtml
    Html(unescapeMarkdownHtml(enhanceHtml(html)))
  }

  def escapeMarkdown(s: String): String = {
    val replacedEntities = EntityConverter.INSTANCE.replaceEntities(s.replace("\n", ""), true, false)
    markdownControlCharacters.foldLeft(replacedEntities) { case (escaped, specialChar) =>
      escaped.replace(specialChar, "\\" + specialChar)
    }
  }

  def unescapeMarkdownHtml(html: String): String = {
    val unescapeHtml = markdownControlCharacters.foldLeft(html) { case (acc, specialChar) =>
      acc.replace("\\" + specialChar, specialChar)
    }
    replaceInvisibleChars(unescapeHtml)
  }

  def removeSpacesForStrong(s: String): String =
    // removing spaces after double asterisk (**) for bold <strong> markdown
    s.replaceAll("(\\*\\*+)\\s*(.*?)\\s*(\\*\\*+)", "$1$2$3")
}
