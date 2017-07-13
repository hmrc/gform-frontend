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
import org.scalatest.mockito.MockitoSugar.mock
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.connectors.SessionCacheConnector
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.model.{ FormTemplate, FormTypeId, Version }
import uk.gov.hmrc.gform.models.components.{ FieldId, FieldValue, InformationMessage, StandardInfo, _ }
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable.List
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._

class PageForRenderSpec extends Spec {

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
    `type` = InformationMessage(StandardInfo, markdown),
    label = "This is the field label",
    helpText = None,
    shortName = None,
    mandatory = true,
    editable = false,
    submissible = false
  )

  val dmsSubmission = DmsSubmission("Dunno", "pure class", "pure business")
  val section = Section("About you", None, None, List(infoFieldValue))

  val formTemplate = FormTemplate(
    formTypeId = FormTypeId(""),
    formName = "AAA000",
    version = Version("0.0.0"),
    description = "YEAHH man!",
    characterSet = "UTF-8",
    dmsSubmission = dmsSubmission,
    submitSuccessUrl = "success-url",
    submitErrorUrl = "error-url",
    sections = List(section)
  )

  val mockRepeatService = mock[RepeatingComponentService]
  val envelope = Envelope(files = Nil)
  implicit val hc = HeaderCarrier()
  implicit val mockAuthContext = mock[AuthContext]

  "PageForRender for info field" should "return the HMTL representation of provided markdown" in {
    val pageToRenderF = PageForRender(
      curr = 8,
      fieldData = Map.empty[FieldId, Seq[String]],
      formTemplate = formTemplate,
      section = section,
      f = None,
      mockRepeatService,
      envelope
    )

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

  //  GROUPS

  val grpTextField = FieldValue(
    id = FieldId("INNER_TEXT_FIELD"),
    `type` = Text(Constant("CONSTANT_TEXT"), false),
    label = "INNER_TEXT_LABEL",
    shortName = None,
    helpText = None,
    mandatory = true,
    editable = true,
    submissible = true
  )
  val groupFields = List(grpTextField)

  val group = Group(
    fields = groupFields,
    orientation = Vertical,
    repeatsMax = Some(3),
    repeatsMin = Some(1),
    repeatLabel = Some("REPEAT_LABEL"),
    repeatAddAnotherText = Some("TEXT_IN_ADD_BUTTON")
  )

  val groupFieldValue = FieldValue(
    id = FieldId("GROUP_ID"),
    `type` = group,
    label = "LABEL_GROUP_FIELD",
    helpText = None,
    shortName = None,
    mandatory = true,
    editable = false,
    submissible = false
  )

  val grpSection = section.copy(fields = List(groupFieldValue))

  val mockSession = mock[SessionCacheConnector]

  "PageForRender for group field" should "return HTML with dynamic groups and an add-group button (repeating groups)" in {

    val testGrpRepSrvc = new RepeatingComponentService(mockSession) {
      override def getAllFieldsInGroup(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
        multipleCopiesOf(grpTextField, 2)
      }

      override def getRepeatingGroupsForRendering(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
        Future.successful((List(
          multipleCopiesOf(grpTextField, 1),
          multipleCopiesOf(grpTextField, 1)
        ), false))
      }
    }

    val pageToRenderF = PageForRender(
      curr = 0,
      fieldData = Map.empty[FieldId, Seq[String]],
      formTemplate = formTemplate.copy(sections = List(grpSection)),
      section = grpSection,
      f = None,
      testGrpRepSrvc,
      envelope
    )

    val pageToRender = Await.result(pageToRenderF, 10 seconds)
    val doc = Jsoup.parse(pageToRender.snippets.head.toString)

    doc.getElementsByAttributeValue("value", "AddGroup-GROUP_ID").size shouldBe 1 withClue "no limit reached, add button shown"
    doc.getElementsByAttributeValue("value", "CONSTANT_TEXT").size shouldBe 2 withClue "two repeat elements"
    doc.getElementsContainingOwnText("REPEAT_LABEL").size shouldBe 2 withClue "repeat label only for second element"
  }

  it should "hide add-group button when limit has been reached (repeating groups)" in {
    val testGrpRepSrvc = new RepeatingComponentService(mockSession) {
      override def getAllFieldsInGroup(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
        multipleCopiesOf(grpTextField, 2)
      }

      override def getRepeatingGroupsForRendering(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
        Future.successful((List(
          multipleCopiesOf(grpTextField, 1),
          multipleCopiesOf(grpTextField, 1)
        ), true))
      }
    }

    val pageToRenderF = PageForRender(
      curr = 0,
      fieldData = Map.empty[FieldId, Seq[String]],
      formTemplate = formTemplate.copy(sections = List(grpSection)),
      section = grpSection,
      f = None,
      testGrpRepSrvc,
      envelope
    )

    val pageToRender = Await.result(pageToRenderF, 10 seconds)
    val doc = Jsoup.parse(pageToRender.snippets.head.toString)

    doc.getElementsByAttributeValue("value", "AddGroup-GROUP_ID").size shouldBe 0 // no add label when limit reached
    doc.getElementsByAttributeValue("value", "CONSTANT_TEXT").size shouldBe 2
  }

  private def multipleCopiesOf(fieldValue: FieldValue, count: Int): List[FieldValue] = {
    (1 to count).map { i =>
      fieldValue.copy(id = FieldId(s"${i}_${fieldValue.id.value}"))
    }.toList
  }

}
