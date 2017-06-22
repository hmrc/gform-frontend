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

package uk.gov.hmrc.gform.models

import org.jsoup.Jsoup
import org.scalatest.{FlatSpec, Matchers}
import uk.gov.hmrc.gform.models.components.{FieldId, FieldValue, InformationMessage, StandardInfo}
import uk.gov.hmrc.gform.models.form.FormTypeId

import scala.concurrent.duration._
import org.mockito.Matchers._

import scala.concurrent.Await


class PageForRenderSpec extends FlatSpec with Matchers {

  val markdown =
    """
      |You are now seeing markdown.
      |
      |# This is an H1
      |## This is an H2
      |### This is an H3
      |
      |Link:
      |
      |[This is a link](https://avatars1.githubusercontent.com/u/5755339?v=3&s=200)
      |
      |An image: ![Alt Text](/template/assets/images/gov.uk_logotype_crown.png)
      |
      |Now some code:
      |```
      |This is some code
      |Second line of code
      |Third line of code
      |```
      |
      |Ordered list
      |
      |1. One
      |2. Two
      |3. Three
      |
      |Unordered list
      |
      |* element one
      |* element two
      |* element three
      |
      || Table         | Col           | name  |
      || ------------- |:-------------:| -----:|
      || col 3 is      | right-aligned | $1600 |
      || col 2 is      | centered      |   $12 |
      || aa a a a a aa | bbbbbbbbb     |    $1 |
      |
      """.stripMargin

  val infoFieldValue = FieldValue(
    id = FieldId("testInfoField"),
    `type`= InformationMessage(StandardInfo, markdown),
    label = "This is the field label",
    helpText = None,
    mandatory = true,
    editable = false,
    submissible = false
  )

  val dmsSubmission = DmsSubmission("Dunno", "pure class", "pure business")
  val section = Section("About you", None, List(infoFieldValue))

  val formTemplate = FormTemplate(
    formTypeId = FormTypeId(""),
    formName = "AAA000",
    version = "0.0.0",
    description = "YEAHH man!",
    characterSet = "UTF-8",
    dmsSubmission = dmsSubmission,
    submitSuccessUrl = "success-url",
    submitErrorUrl = "error-url",
    sections = List(section)
  )


  "PageForRender for info field" should "return the HMTL representation of provided markdown" in {
    val pageToRenderF = PageForRender(
      curr = 8,
      fieldData = Map.empty[FieldId, Seq[String]],
      formTemplate = formTemplate,
      section = section,
      f = None)(any(),any())

    val pageToRender = Await.result(pageToRenderF, 10 seconds)
    val doc = Jsoup.parse(pageToRender.snippets.head.toString)

    doc.getElementsByTag("H1").size() shouldBe 1
    doc.getElementsByTag("H2").size() shouldBe 1
    doc.getElementsByTag("H3").size() shouldBe 1
    doc.getElementsByTag("A").size() shouldBe 1
    doc.getElementsByTag("IMG").size() shouldBe 1
    doc.getElementsByTag("CODE").size() shouldBe 1
    doc.getElementsByTag("PRE").size() shouldBe 1
    doc.getElementsByTag("OL").size() shouldBe 1
    doc.getElementsByTag("UL").size() shouldBe 1

    val table = doc.getElementsByTag("TABLE")
    table.size() shouldBe 1
    table.first.getElementsByTag("TR").size shouldBe 4
    table.first.getElementsByTag("TD").size shouldBe 9
  }
}
