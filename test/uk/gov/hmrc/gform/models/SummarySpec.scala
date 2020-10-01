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

package uk.gov.hmrc.gform.models

/* import cats.data.NonEmptyList
 * import cats.syntax.validated._
 * import org.jsoup.Jsoup
 * import org.scalatest.mockito.MockitoSugar.mock
 * import play.api.i18n.{ Lang, Messages }
 * import play.api.mvc.Call
 * import uk.gov.hmrc.gform.Helpers.toSmartString
 * import uk.gov.hmrc.gform.Spec
 * import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
 * import uk.gov.hmrc.gform.gform.routes
 * import uk.gov.hmrc.gform.graph.RecData
 * import uk.gov.hmrc.gform.models.helpers.Extractors._
 * import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, NotChecked, VariadicFormData }
 * import uk.gov.hmrc.gform.sharedmodel.form.{ FormDataRecalculated, ValidationResult }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate._
 * import uk.gov.hmrc.gform.sharedmodel.graph.IncludeIfGN
 * import uk.gov.hmrc.gform.summary.SummaryRenderingService
 * import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
 * import uk.gov.hmrc.gform.views.ViewHelpersAlgebra
 *
 * import scala.collection.immutable.List
 * import uk.gov.hmrc.http.HeaderCarrier
 *
 * class SummarySpec extends Spec with FormModelSupport {
 *
 *   private implicit val langADT = LangADT.En
 *   private implicit val messages = new Messages {
 *     override def lang: Lang = Lang.defaultLang
 *
 *     override def apply(key: String, args: Any*): String =
 *       if (key == "date.November") "November"
 *       else key + "_value"
 *
 *     override def apply(keys: Seq[String], args: Any*): String = keys.mkString("_")
 *
 *     override def translate(key: String, args: Seq[Any]): Option[String] = Some(apply(key))
 *
 *     override def isDefinedAt(key: String): Boolean = true
 *   }
 *
 *   private implicit val viewHelpers: ViewHelpersAlgebra = null
 *
 *   trait Test extends ExampleData {
 *
 *     def section0 =
 *       Section.NonRepeatingPage(
 *         Page(
 *           toSmartString("Your details"),
 *           None,
 *           None,
 *           None,
 *           None,
 *           None,
 *           List(
 *             FormComponent(
 *               `fieldId - iptRegNum`,
 *               Text(BasicText, Value),
 *               toSmartString("Insurance Premium Tax (IPT) number"),
 *               None,
 *               None,
 *               None,
 *               true,
 *               true,
 *               true,
 *               false,
 *               false,
 *               None
 *             )),
 *           None,
 *           None
 *         ))
 *     def section1 =
 *       Section.NonRepeatingPage(
 *         Page(
 *           toSmartString("About you"),
 *           None,
 *           None,
 *           None,
 *           None,
 *           None,
 *           List(
 *             FormComponent(
 *               `fieldId - firstName`,
 *               Text(BasicText, Value),
 *               toSmartString("First Name"),
 *               None,
 *               None,
 *               None,
 *               true,
 *               true,
 *               true,
 *               false,
 *               false,
 *               None)),
 *           None,
 *           None
 *         ))
 *     def section2: Section =
 *       Section.NonRepeatingPage(
 *         Page(
 *           toSmartString("Business details"),
 *           None,
 *           None,
 *           None,
 *           None,
 *           None,
 *           List(
 *             FormComponent(
 *               `fieldId - businessName`,
 *               Text(BasicText, Value),
 *               toSmartString("Name of business"),
 *               None,
 *               None,
 *               None,
 *               true,
 *               true,
 *               true,
 *               false,
 *               false,
 *               None)),
 *           None,
 *           None
 *         ))
 *
 *     override def `formField - iptRegNum` = super.`formField - iptRegNum`.copy(value = "Test!Your details!Test")
 *     override def `formField - firstName` = super.`formField - firstName`.copy(value = "Test!About you!Test")
 *     override def `formField - businessName` =
 *       super.`formField - businessName`.copy(value = "Test!Business details!Test")
 *
 *     override def formTemplate = super.formTemplate.copy(sections = List(section0, section1, section2))
 *
 *     val retrievals: MaterialisedRetrievals = mock[MaterialisedRetrievals]
 *
 *     def fieldValues = formTemplate.sections.flatMap(_.fields)
 *     def f: ValidatedType[ValidatorsResult] = ValidatorsResult.empty.valid
 *     implicit val hc = HeaderCarrier()
 *
 *   }
 *
 *   "Summary" should "display the summary sections" in new Test {
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate, formDataRecalculated)
 *     val render = SummaryRenderingService
 *       .summaryForRender(f, formDataRecalculated, Some(accessCode), formTemplate, formModel, envelope, NotChecked)
 *     render.size should be(9)
 *     extractAllTestStringValues(render) should be(List("Your details", "About you", "Business details"))
 *   }
 *
 *   it should "display links to page sections" in new Test {
 *     override def formTemplate = super.formTemplate.copy(sections = List(section0, section1))
 *
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *
 *     val testStringValues = extractAllHrefs(render)
 *
 *     def callUrlEscaped(call: Call) = call.url.replaceAll("&", "&amp;")
 *
 *     val expectedResult = List(
 *       callUrlEscaped(
 *         routes.FormController
 *           .form(
 *             formTemplate._id,
 *             Some(accessCode),
 *             SectionNumber(0),
 *             SectionTitle4Ga("Your-details"),
 *             SuppressErrors.Yes,
 *             FastForward.Yes)),
 *       callUrlEscaped(
 *         routes.FormController
 *           .form(
 *             formTemplate._id,
 *             Some(accessCode),
 *             SectionNumber(1),
 *             SectionTitle4Ga("About-you"),
 *             SuppressErrors.Yes,
 *             FastForward.Yes))
 *     )
 *
 *     testStringValues(0) should startWith(expectedResult(0))
 *     testStringValues(1) should startWith(expectedResult(1))
 *   }
 *
 *   it should "display values for each field type with a submissible field, " in new Test {
 *
 *     val section = Section.NonRepeatingPage(
 *       Page(
 *         toSmartString("Personal details"),
 *         None,
 *         None,
 *         None,
 *         None,
 *         None,
 *         List(
 *           FormComponent(
 *             FormComponentId("Surname"),
 *             Text(BasicText, Value),
 *             toSmartString("Surname"),
 *             None,
 *             None,
 *             None,
 *             true,
 *             true,
 *             true,
 *             false,
 *             false,
 *             None),
 *           FormComponent(
 *             FormComponentId("Info"),
 *             Text(BasicText, Value),
 *             toSmartString("Info"),
 *             None,
 *             None,
 *             None,
 *             true,
 *             true,
 *             submissible = false,
 *             false,
 *             false,
 *             None,
 *             presentationHint = Some(List(InvisibleInSummary))
 *           ),
 *           FormComponent(
 *             FormComponentId("BirthDate"),
 *             Date(AnyDate, Offset(0), None),
 *             toSmartString("Birth date"),
 *             None,
 *             None,
 *             None,
 *             true,
 *             true,
 *             true,
 *             false,
 *             false,
 *             None
 *           ),
 *           FormComponent(
 *             FormComponentId("HomeAddress"),
 *             Address(international = false),
 *             toSmartString("Home address"),
 *             None,
 *             None,
 *             None,
 *             true,
 *             true,
 *             true,
 *             false,
 *             false,
 *             None
 *           )
 *         ),
 *         None,
 *         None
 *       ))
 *
 *     override def formTemplate = super.formTemplate.copy(sections = List(section))
 *
 *     override val formDataRecalculated = FormDataRecalculated.empty.copy(
 *       recData = RecData.fromData(VariadicFormData.ones(
 *         FormComponentId("Surname")              -> "Test!Saxe-Coburg-Gotha!Test",
 *         FormComponentId("Info")                 -> "Test!Royal!Test",
 *         FormComponentId("BirthDate-day")        -> "19",
 *         FormComponentId("BirthDate-month")      -> "11",
 *         FormComponentId("BirthDate-year")       -> "1841",
 *         FormComponentId("HomeAddress-street1")  -> "Test!Street!Test",
 *         FormComponentId("HomeAddress-street2")  -> "Test!Second Street!Test",
 *         FormComponentId("HomeAddress-street3")  -> "Test!Third Street!Test",
 *         FormComponentId("HomeAddress-street4")  -> "Test!Town!Test",
 *         FormComponentId("HomeAddress-postcode") -> "Test!PO32 6JX!Test",
 *         FormComponentId("HomeAddress-country")  -> "Test!UK!Test"
 *       )))
 *
 *     override def fieldValues = formTemplate.sections.flatMap(_.fields)
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate, formDataRecalculated)
 *     val render = SummaryRenderingService
 *       .summaryForRender(f, formDataRecalculated, Some(accessCode), formTemplate, formModel, envelope, NotChecked)
 *     val testStringValues = extractAllTestStringValues(render)
 *     testStringValues should be(
 *       List("Saxe-Coburg-Gotha", "Street", "Second Street", "Third Street", "Town", "PO32 6JX", "UK"))
 *     extractDates(render) should be(List(("19", "November", "1841")))
 *   }
 *
 *   it should "display the title when shortName is not present in the section" in new Test {
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *
 *     val doc = Jsoup.parse(render.head.toString())
 *     doc.getElementsByTag("H2").text().toLowerCase should include("your details")
 *   }
 *
 *   it should "display the shortName as section title if present" in new Test {
 *     val shortName = "THIS_IS_A_VERY_VERY_VERY_SHORT_NAME"
 *     val section = section0.updateShortName(Some(toSmartString(shortName)))
 *     override val formTemplate = super.formTemplate.copy(sections = List(section))
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *
 *     val doc = Jsoup.parse(render.head.toString())
 *     doc.getElementsByTag("H2").text().toUpperCase should include(shortName)
 *   }
 *
 *   it should "display shortName instead of label for Address field" in new Test {
 *     val shortName = "JUST_A_VERY_SHORT_NAME"
 *     val addressField = FormComponent(
 *       id = FormComponentId("anId"),
 *       `type` = Address(false),
 *       label = toSmartString("label"),
 *       shortName = Some(toSmartString(shortName)),
 *       helpText = None,
 *       validIf = None,
 *       mandatory = true,
 *       editable = true,
 *       submissible = true,
 *       derived = true,
 *       errorMessage = None
 *     )
 *
 *     val section =
 *       section0.updateFields(List(addressField)).updateShortName(Some(toSmartString("Address section")))
 *     override val formTemplate = super.formTemplate.copy(sections = List(section))
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *     val doc = Jsoup.parse(render.mkString)
 *     doc.getElementsByTag("DT").text().equals(shortName) shouldBe true
 *   }
 *
 *   it should "display label when shortName not provided for Address field" in new Test {
 *     val label = "JUST_A_VERY_LONG_LABEL"
 *     val addressField = FormComponent(
 *       id = FormComponentId("anId"),
 *       `type` = Address(false),
 *       label = toSmartString(label),
 *       shortName = None,
 *       helpText = None,
 *       validIf = None,
 *       mandatory = true,
 *       editable = true,
 *       submissible = true,
 *       derived = true,
 *       errorMessage = None
 *     )
 *     val section =
 *       section0.updateFields(List(addressField)).updateShortName(Some(toSmartString("Address section")))
 *     override val formTemplate = super.formTemplate.copy(sections = List(section))
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *     val doc = Jsoup.parse(render.mkString)
 *     doc.getElementsByTag("DT").text().equals(label) shouldBe true
 *   }
 *
 *   it should "display shortName instead of label for Text field" in new Test {
 *     val shortName = "JUST_A_VERY_SHORT_NAME"
 *     val addressField = FormComponent(
 *       id = FormComponentId("anId"),
 *       `type` = Text(BasicText, Constant("DA")),
 *       label = toSmartString("label"),
 *       shortName = Some(toSmartString(shortName)),
 *       helpText = None,
 *       validIf = None,
 *       mandatory = true,
 *       editable = true,
 *       submissible = true,
 *       derived = true,
 *       errorMessage = None
 *     )
 *
 *     val section =
 *       section0.updateFields(List(addressField)).updateShortName(Some(toSmartString("A section")))
 *     override val formTemplate = super.formTemplate.copy(sections = List(section))
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *     val doc = Jsoup.parse(render.mkString)
 *     doc.getElementsByTag("DT").first().text().equals(shortName) shouldBe true
 *   }
 *
 *   it should "display label if shortName not provided for Text field" in new Test {
 *     val label = "THIS_IS_A_LABEL"
 *     val addressField = FormComponent(
 *       id = FormComponentId("anId"),
 *       `type` = Text(BasicText, Constant("DA")),
 *       label = toSmartString(label),
 *       shortName = None,
 *       helpText = None,
 *       validIf = None,
 *       mandatory = true,
 *       editable = true,
 *       submissible = true,
 *       derived = true,
 *       errorMessage = None
 *     )
 *
 *     val section =
 *       section0.updateFields(List(addressField)).updateShortName(Some(toSmartString("A section")))
 *     override val formTemplate = super.formTemplate.copy(sections = List(section))
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *     val doc = Jsoup.parse(render.mkString)
 *     doc.getElementsByTag("DT").first().text().equals(label) shouldBe true
 *   }
 *
 *   it should "display shortName instead of label for Choice field" in new Test {
 *     val shortName = "JUST_A_VERY_SHORT_NAME"
 *     val addressField = FormComponent(
 *       id = FormComponentId("anId"),
 *       `type` = Choice(Radio, NonEmptyList.of(toSmartString("u")), Vertical, List(), None),
 *       label = toSmartString("label"),
 *       shortName = Some(toSmartString(shortName)),
 *       helpText = None,
 *       validIf = None,
 *       mandatory = true,
 *       editable = true,
 *       submissible = true,
 *       derived = true,
 *       errorMessage = None
 *     )
 *
 *     val section =
 *       section0.updateFields(List(addressField)).updateShortName(Some(toSmartString("A section")))
 *     override val formTemplate = super.formTemplate.copy(sections = List(section))
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *     val doc = Jsoup.parse(render.mkString)
 *     doc.getElementsByTag("DT").first().text().equals(shortName) shouldBe true
 *   }
 *
 *   it should "display label if shortName not provided for Choice field" in new Test {
 *     val label = "THIS_IS_A_LABEL"
 *     val addressField = FormComponent(
 *       id = FormComponentId("anId"),
 *       `type` = Choice(Radio, NonEmptyList.of(toSmartString("u")), Vertical, List(), None),
 *       label = toSmartString(label),
 *       shortName = None,
 *       helpText = None,
 *       validIf = None,
 *       mandatory = true,
 *       editable = true,
 *       submissible = true,
 *       derived = true,
 *       errorMessage = None
 *     )
 *
 *     val section =
 *       section0.updateFields(List(addressField)).updateShortName(Some(toSmartString("A section")))
 *     override val formTemplate = super.formTemplate.copy(sections = List(section))
 *     //    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, fieldValues, envelope)
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *     val doc = Jsoup.parse(render.mkString)
 *     doc.getElementsByTag("DT").first().text().equals(label) shouldBe true
 *   }
 *
 *   it should "display shortName instead of label for Date field" in new Test {
 *     val shortName = "JUST_A_VERY_SHORT_NAME"
 *     val addressField = FormComponent(
 *       id = FormComponentId("anId"),
 *       `type` = Date(AnyDate, Offset(0), None),
 *       label = toSmartString("label"),
 *       shortName = Some(toSmartString(shortName)),
 *       helpText = None,
 *       validIf = None,
 *       mandatory = true,
 *       editable = true,
 *       submissible = true,
 *       derived = true,
 *       errorMessage = None
 *     )
 *
 *     val section =
 *       section0.updateFields(List(addressField)).updateShortName(Some(toSmartString("A section")))
 *     override val formTemplate = super.formTemplate.copy(sections = List(section))
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *     val doc = Jsoup.parse(render.mkString)
 *     doc.getElementsByTag("DT").first().text().equals(shortName) shouldBe true
 *   }
 *
 *   it should "display label if shortName not provided for Date field" in new Test {
 *     val label = "THIS_IS_A_LABEL"
 *     val addressField = FormComponent(
 *       id = FormComponentId("anId"),
 *       `type` = Date(AnyDate, Offset(0), None),
 *       label = toSmartString(label),
 *       shortName = None,
 *       helpText = None,
 *       validIf = None,
 *       mandatory = true,
 *       editable = true,
 *       submissible = true,
 *       derived = true,
 *       errorMessage = None
 *     )
 *
 *     val section =
 *       section0.updateFields(List(addressField)).updateShortName(Some(toSmartString("A section")))
 *     override val formTemplate = super.formTemplate.copy(sections = List(section))
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *     val doc = Jsoup.parse(render.mkString)
 *     doc.getElementsByTag("DT").first().text().equals(label) shouldBe true
 *   }
 *
 *   it should "not render sections with includeIf expressions that evaluate to false" in new Test {
 *
 *     val includeIf = IncludeIf(Equals(FormCtx("firstName"), Constant("Pete")))
 *     override val section2 = super.section2.updateIncludeIf(Some(includeIf))
 *     override val formTemplate = super.formTemplate.copy(
 *       sections = List(section2)
 *     )
 *     val data1 = FormDataRecalculated.empty.copy(
 *       recData = RecData.fromData(VariadicFormData.ones(FormComponentId("firstName") -> "Pete")))
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate, data1)
 *     val renderWithDataMatching = SummaryRenderingService.summaryForRender(
 *       f,
 *       data1,
 *       Some(accessCode),
 *       formTemplate,
 *       formModel,
 *       envelope,
 *       NotChecked
 *     )
 *     renderWithDataMatching.size shouldBe 3
 *     val data2 = FormDataRecalculated(
 *       Set(IncludeIfGN(FormComponentId("includeId_X"), includeIf)),
 *       RecData.fromData(VariadicFormData.ones(FormComponentId("firstName") -> "*Not*Pete")))
 *     val formModel2: FormModel[FullyExpanded] = mkFormModel(formTemplate, data2)
 *     val renderWithDataMismatch = SummaryRenderingService.summaryForRender(
 *       f,
 *       data2,
 *       Some(accessCode),
 *       formTemplate,
 *       formModel2,
 *       envelope,
 *       NotChecked
 *     )
 *     renderWithDataMismatch.size shouldBe 0
 *   }
 *
 *   it should "display Group Labels (or Group Short Names if specified)" in new Test {
 *
 *     val groupFieldValue = FormComponent(
 *       FormComponentId("gid"),
 *       Group(
 *         List(),
 *         Horizontal
 *       ),
 *       toSmartString("Test!group-label!Test"),
 *       None,
 *       None,
 *       None,
 *       true,
 *       true,
 *       true,
 *       true,
 *       false,
 *       None
 *     )
 *     override def section0 =
 *       Section.NonRepeatingPage(Page(toSmartString(""), None, None, None, None, None, List(groupFieldValue), None, None))
 *     override def formTemplate = super.formTemplate.copy(sections = List(section0))
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate)
 *     val render0 =
 *       SummaryRenderingService
 *         .summaryForRender(
 *           f,
 *           FormDataRecalculated.empty,
 *           Some(accessCode),
 *           formTemplate,
 *           formModel,
 *           envelope,
 *           NotChecked)
 *     extractAllTestStringValues(render0) should be(List("group-label"))
 *     val formTemplateWGroupWithShortname = formTemplate.copy(
 *       sections = List(
 *         Section.NonRepeatingPage(
 *           Page(
 *             toSmartString(""),
 *             None,
 *             None,
 *             None,
 *             None,
 *             None,
 *             List(groupFieldValue.copy(shortName = Some(toSmartString("Test!group-shortname!Test")))),
 *             None,
 *             None
 *           )))
 *     )
 *
 *     val filedValues1 = formTemplate.sections.flatMap(_.fields)
 *     val formModel2: FormModel[FullyExpanded] = mkFormModel(formTemplateWGroupWithShortname)
 *     val render1 = SummaryRenderingService
 *       .summaryForRender(
 *         f,
 *         FormDataRecalculated.empty,
 *         Some(accessCode),
 *         formTemplateWGroupWithShortname,
 *         formModel2,
 *         envelope,
 *         NotChecked)
 *     extractAllTestStringValues(render1) should be(List("group-shortname"))
 *   }
 *
 *   "The Change hrefs" should "link to the correct page" in new Test {
 *     val includeIf = IncludeIf(Equals(FormCtx("firstName"), Constant("Pete")))
 *
 *     override val formTemplate = super.formTemplate.copy(
 *       sections = List(section0, section1.updateIncludeIf(Some(includeIf)), section2)
 *     )
 *
 *     val data1 = FormDataRecalculated(
 *       Set(IncludeIfGN(FormComponentId("includeId_X"), includeIf)),
 *       RecData.fromData(VariadicFormData.ones(FormComponentId("firstName") -> "*Not*Pete")))
 *
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(formTemplate, data1)
 *
 *     val summaryForRender = SummaryRenderingService.summaryForRender(
 *       f,
 *       data1,
 *       Some(accessCode),
 *       formTemplate,
 *       formModel,
 *       envelope,
 *       NotChecked
 *     )
 *
 *     {
 *       val htmlAheadOfSection0 = summaryForRender(1)
 *       val doc = Jsoup.parse(htmlAheadOfSection0.toString)
 *       val urlOfHrefToSection0 = doc.select("a:contains(Change)").get(0).attributes().get("href")
 *       val targetUrl = uk.gov.hmrc.gform.gform.routes.FormController
 *         .form(
 *           formTemplate._id,
 *           Some(accessCode),
 *           SectionNumber(0),
 *           SectionTitle4Ga("Your-details"),
 *           SuppressErrors.Yes,
 *           FastForward.Yes)
 *         .url
 *       urlOfHrefToSection0 shouldBe targetUrl
 *     }
 *     {
 *       val htmlAheadOfSection2 = summaryForRender(4)
 *       val doc = Jsoup.parse(htmlAheadOfSection2.toString)
 *       val urlOfHrefToSection2 = doc.select("a:contains(Change)").get(0).attributes().get("href")
 *       val targetUrl = uk.gov.hmrc.gform.gform.routes.FormController
 *         .form(
 *           formTemplate._id,
 *           Some(accessCode),
 *           SectionNumber(2),
 *           SectionTitle4Ga("Business-details"),
 *           SuppressErrors.Yes,
 *           FastForward.Yes)
 *         .url
 *       urlOfHrefToSection2 shouldBe targetUrl
 *     }
 *   }
 * } */
