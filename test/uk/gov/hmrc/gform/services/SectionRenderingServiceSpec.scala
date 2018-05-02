/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.services

import org.jsoup.Jsoup
import org.jsoup.nodes.{ Document, Element }
import org.jsoup.select.Elements
import org.scalatest.mockito.MockitoSugar.mock
import play.api.libs.json.JsValue
import play.api.test.FakeRequest
import uk.gov.hmrc.gform.SpecWithFakeApp
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gform.{ PrepopService, SectionRenderingService }
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.collection.JavaConverters
import scala.collection.immutable.List
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class SectionRenderingServiceSpec extends SpecWithFakeApp {

  implicit val request = {
    val fakeRequest = FakeRequest()
    fakeRequest.copyFakeRequest(tags = fakeRequest.tags + ("CSRF_TOKEN_NAME" -> "csrfToken") + ("CSRF_TOKEN" -> "o'ight mate?"))
  }
  implicit val messages = mock[play.api.i18n.Messages]
  val retrievals = mock[Retrievals]

  val mockPrepopService = new PrepopService(null, null, null) {
    override def prepopData(expr: Expr, formTemplate: FormTemplate, retrievals: Retrievals, data: Map[FormComponentId, Seq[String]], repeatCache: Future[Option[CacheMap]], section: BaseSection, scale: Option[Int])(implicit hc: HeaderCarrier): Future[String] =
      Future.successful("")
  }

  val mockRepeatService = new RepeatingComponentService(null, null) {

    override def getAllSections(formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]], cache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[List[Section]] = {
      Future.successful(allSections)
    }

    override def getCache(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CacheMap]] = Future.successful(None)

    override def getAllRepeatingGroups(cache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[CacheMap] =
      Future.successful(CacheMap("EMPTY", Map.empty[String, JsValue]))

    override def atomicFields(repeatCache: Future[Option[CacheMap]])(section: BaseSection)(implicit hc: HeaderCarrier, ec: ExecutionContext): List[FormComponent] = {
      section.fields
    }

    override def getRepeatingGroupsForRendering(topFieldValue: FormComponent, groupField: Group, cache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ec: ExecutionContext) = {
      Future.successful((List(groupField.fields), false))
    }
  }

  val testService = new SectionRenderingService(mockRepeatService, mockPrepopService, frontendAppConfig)

  "SectionRenderingService" should "generate first page" in {
    val generatedHtml = testService
      .renderSection(
        form,
        SectionNumber.firstSection,
        Map.empty,
        formTemplate,
        None,
        Envelope(Nil),
        envelopeId,
        None,
        allSections,
        mockRepeatService.getCache,
        0,
        Nil,
        retrievals,
        None
      ).futureValue

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))

    hiddenFieldNames should be(List("csrfToken", "nameOfBusiness", "startDate-day", "startDate-month", "startDate-year", "iptRegNum", "save"))
    visibleFields should be(List("firstName", "surname"))
  }
  "SectionRenderingService" should "set a field to hidden if is onlyShowOnSummary is set to true" in {
    val generatedHtml = testService
      .renderSection(
        form,
        SectionNumber.firstSection,
        Map.empty,
        formTemplate,
        None,
        Envelope(Nil),
        envelopeId,
        None,
        allSections.map(sc => sc.copy(fields = sc.fields.map(f => f.copy(onlyShowOnSummary = true)))),
        mockRepeatService.getCache,
        0,
        Nil,
        retrievals,
        None
      ).futureValue

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))

    hiddenFieldNames should be(List("csrfToken", "firstName", "surname", "nameOfBusiness", "startDate-day", "startDate-month", "startDate-year", "iptRegNum", "save"))
    visibleFields should be(List())
  }
  "SectionRenderingService" should "add in progress indicator if it is defined" in {
    val generatedHtml = testService
      .renderSection(
        form,
        SectionNumber.firstSection,
        Map.empty,
        formTemplate,
        None,
        Envelope(Nil),
        envelopeId,
        None,
        List(allSections.head.copy(progressIndicator = Some("Progress Indicator"))),
        mockRepeatService.getCache,
        0,
        Nil,
        retrievals,
        None
      ).futureValue

    val doc: Document = Jsoup.parse(generatedHtml.body)
    val progressIndicator = doc.getElementById("progress-indicator")
    progressIndicator.toString should be("<span id=\"progress-indicator\" class=\"form-hint\">Progress Indicator</span>")
  }

  it should "generate second page" in {
    val generatedHtml = testService
      .renderSection(
        form,
        SectionNumber(1),
        Map.empty,
        formTemplate,
        None,
        Envelope(Nil),
        envelopeId,
        None,
        allSections,
        mockRepeatService.getCache,
        0,
        Nil,
        retrievals,
        None
      ).futureValue

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))

    hiddenFieldNames should be(List("csrfToken", "firstName", "surname", "facePhoto", "save"))
    visibleFields should be(List("nameOfBusiness", "startDate-day", "startDate-month", "startDate-year", "iptRegNum"))
  }

  it should "return the HMTL representation of provided markdown" in new ExampleData {

    override def `fieldValue - firstName` = {
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

      FormComponent(
        id = FormComponentId("testInfoField"),
        `type` = InformationMessage(StandardInfo, markdown),
        label = "This is the field label",
        helpText = None,
        shortName = None,
        validIf = None,
        mandatory = true,
        editable = false,
        submissible = false,
        derived = true,
        errorMessage = None
      )
    }

    override def `section - about you` = super.`section - about you`.copy(fields = List(`fieldValue - firstName`))

    override def allSections = List(
      `section - about you`
    )

    val generatedHtml = testService
      .renderSection(
        form,
        SectionNumber(0),
        Map.empty,
        formTemplate,
        None,
        Envelope(Nil),
        envelopeId,
        None,
        allSections,
        mockRepeatService.getCache,
        0,
        Nil,
        retrievals,
        None
      ).futureValue

    val doc = Jsoup.parse(generatedHtml.body)

    val infoFieldDiv = doc.getElementsByClass("subsection").first

    infoFieldDiv.getElementsByTag("H1").last.text shouldBe "This is an H1"
    infoFieldDiv.getElementsByTag("H2").first.text shouldBe "This is an H2"
    infoFieldDiv.getElementsByTag("H3").first.text shouldBe "This is an H3"
    infoFieldDiv.getElementsByTag("A").size() shouldBe 1
    infoFieldDiv.getElementsByTag("IMG").size() shouldBe 1
    infoFieldDiv.getElementsByTag("CODE").size() shouldBe 1
    infoFieldDiv.getElementsByTag("PRE").size() shouldBe 1
    infoFieldDiv.getElementsByTag("OL").size() shouldBe 1
    infoFieldDiv.getElementsByTag("UL").size() shouldBe 1

    val table = infoFieldDiv.getElementsByTag("TABLE")
    table.size() shouldBe 1
    table.first.getElementsByTag("TR").size shouldBe 4
    table.first.getElementsByTag("TD").size shouldBe 9
  }

  it should "return HTML with dynamic groups and an add-group button (repeating groups)" in new ExampleData {

    override def `group - type` = Group(
      fields = List(`fieldValue - firstName`),
      orientation = Horizontal,
      repeatsMax = Some(3),
      repeatsMin = Some(1),
      repeatLabel = Some("REPEAT_LABEL"),
      repeatAddAnotherText = Some("repeatAddAnotherText")
    )

    override def allSections = List(
      `section - group`
    )

    val generatedHtml = testService
      .renderSection(
        form,
        SectionNumber(0),
        Map.empty,
        formTemplate,
        None,
        Envelope(Nil),
        envelopeId,
        None,
        allSections,
        mockRepeatService.getCache,
        0,
        Nil,
        retrievals,
        None
      ).futureValue

    val doc = Jsoup.parse(generatedHtml.body)

    val addButtonValue = "AddGroup-" + `fieldValue - group`.id.value
    val fieldName = `fieldValue - firstName`.id.value
    doc.getElementsByAttributeValue("value", addButtonValue).size shouldBe 1 withClue "no limit reached, add button shown"
    doc.getElementsByAttributeValue("name", fieldName).size shouldBe 1 withClue "One repeat element"
    doc.getElementsContainingOwnText("REPEAT_LABEL").size shouldBe 1
  }

  it should "hide add-group button when limit has been reached (repeating groups)" in new ExampleData {

    val mock2RepeatService = new RepeatingComponentService(null, null) {

      override def getAllSections(formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]], cache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[List[Section]] = {
        Future.successful(allSections)
      }

      override def getAllRepeatingGroups(cache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[CacheMap] =
        Future.successful(CacheMap("EMPTY", Map.empty[String, JsValue]))

      override def atomicFields(repeatCache: Future[Option[CacheMap]])(section: BaseSection)(implicit hc: HeaderCarrier, ec: ExecutionContext): List[FormComponent] = {
        section.fields
      }

      override def getRepeatingGroupsForRendering(topFieldValue: FormComponent, groupField: Group, cache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ec: ExecutionContext) = {
        Future.successful((List(groupField.fields, groupField.fields), true))
      }
    }

    val thisTestService = new SectionRenderingService(mock2RepeatService, mockPrepopService, frontendAppConfig)

    override def `group - type` = Group(
      fields = List(`fieldValue - firstName`),
      orientation = Horizontal,
      repeatsMax = Some(2),
      repeatsMin = Some(1),
      repeatLabel = Some("REPEAT_LABEL"),
      repeatAddAnotherText = Some("repeatAddAnotherText")
    )

    override def allSections = List(
      `section - group`
    )

    val generatedHtml = thisTestService
      .renderSection(
        form,
        SectionNumber(0),
        Map.empty,
        formTemplate,
        None,
        Envelope(Nil),
        envelopeId,
        None,
        allSections,
        mockRepeatService.getCache,
        0,
        Nil,
        retrievals,
        None
      ).futureValue

    val doc = Jsoup.parse(generatedHtml.body)

    val addButtonValue = "AddGroup-" + `fieldValue - group`.id.value
    val fieldName = `fieldValue - firstName`.id.value
    doc.getElementsByAttributeValue("value", addButtonValue).size shouldBe 0
    doc.getElementsByAttributeValue("name", fieldName).size shouldBe 2
    doc.getElementsContainingOwnText("REPEAT_LABEL").size shouldBe 2
  }

  it should "generate declaration page" in {
    val generatedHtml = testService
      .renderDeclarationSection(
        form,
        formTemplate,
        retrievals,
        None,
        Map.empty,
        None,
        mockRepeatService.getCache,
        None
      ).futureValue

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
    val buttons = toList(doc.getElementsByTag("BUTTON")).map(_.childNode(0).outerHtml())

    hiddenFieldNames should be(List("csrfToken", "save"))
    visibleFields should be(List())
    buttons should be(List(("Accept and submit")))
  }

  it should "generate declaration page with submit claim button" in {
    val generatedHtml = testService
      .renderDeclarationSection(
        form,
        formTemplate.copy(formCategory = Some(HMRCClaimForm)),
        retrievals,
        None,
        Map.empty,
        None,
        mockRepeatService.getCache,
        None
      ).futureValue

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
    val buttons = toList(doc.getElementsByTag("BUTTON")).map(_.childNode(0).outerHtml())

    hiddenFieldNames should be(List("csrfToken", "save"))
    visibleFields should be(List())
    buttons should be(List(("Accept and submit claim")))
  }

  it should "generate declaration page with submit return button" in {
    val generatedHtml = testService
      .renderDeclarationSection(
        form,
        formTemplate.copy(formCategory = Some(HMRCReturnForm)),
        retrievals,
        None,
        Map.empty,
        None,
        mockRepeatService.getCache,
        None
      ).futureValue

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
    val buttons = toList(doc.getElementsByTag("BUTTON")).map(_.childNode(0).outerHtml())

    hiddenFieldNames should be(List("csrfToken", "save"))
    visibleFields should be(List())
    buttons should be(List(("Accept and submit return")))
  }

  private def toList(elements: Elements) = JavaConverters.asScalaIteratorConverter(elements.iterator).asScala.toList
}
