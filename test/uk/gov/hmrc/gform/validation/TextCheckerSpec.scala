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

package uk.gov.hmrc.gform.validation

import cats.implicits._
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.Environment
import play.api.http.HttpConfiguration
import play.api.i18n._
import uk.gov.hmrc.gform.Helpers.mkDataOutOfDate
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.LookupLoader
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.mkFormTemplate
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.mkSection
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.FormModelSupport
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.ids.IndexedComponentId
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormatExprGen
import uk.gov.hmrc.gform.lookup._
import ComponentChecker._
import org.scalacheck.Gen

import scala.collection.mutable

class TextCheckerSpec
    extends AnyFlatSpecLike with ScalaCheckDrivenPropertyChecks with Matchers with IdiomaticMockito
    with FormModelSupport {

  val lookupLoader = new LookupLoader("target/scala-2.13/resource_managed/main/conf/index")

  val formTemplate: FormTemplate = mock[FormTemplate]
  val envlopeId: EnvelopeId = mock[EnvelopeId]
  val environment = Environment.simple()
  val configuration =
    Configuration.from(Map("play.i18n.langs" -> Seq("en", "cy"))).withFallback(Configuration.load(environment))
  val langs = new DefaultLangsProvider(configuration).get
  val httpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)
  val messagesApi: MessagesApi =
    new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get
  implicit val messages: Messages = messagesApi.preferred(Seq(Lang("en")))
  implicit val l: LangADT = LangADT.En

  implicit val smartStringEvaluator: SmartStringEvaluator = new SmartStringEvaluator {
    override def apply(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
    override def evalEnglish(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
  }

  private val numberWithPlus = FormatExprGen.telephoneNumberGen(FormatExprGen.International)
  private val numberWithoutPlus = FormatExprGen.telephoneNumberGen(FormatExprGen.UK)
  private val telephoneConstraint = Text(TelephoneNumber, Value)
  private val testFormComponent = FormComponent(
    FormComponentId("testFormComponent"),
    telephoneConstraint,
    toSmartString("formComponentLabel"),
    false,
    None,
    None,
    None,
    None,
    Mandatory.TRUE,
    true,
    false,
    true,
    false,
    None
  )

  "validatePhoneNumber" should "return invalid when character count is less than 7 and contains a special character" in {
    val lessThan7WithPlus = numberWithPlus.map(string => string.substring(0, 6))
    forAll(lessThan7WithPlus) { phoneNumber =>
      val result = TextChecker.validatePhoneNumber(testFormComponent, phoneNumber)
      result.foldMap(ShortCircuitInterpreter) shouldBe
        Map(
          purePure("testFormComponent") -> Set(
            "Enter a phone number, like 01632 960 001, 07700 900 982 or +44 0770 090 0175"
          )
        ).asLeft
    }
  }

  it should "return invalid when a string contains a '$' symbol" in {
    forAll(numberWithPlus) { phoneNumber =>
      val result = TextChecker.validatePhoneNumber(
        testFormComponent,
        phoneNumber + "$"
      )
      result.foldMap(ShortCircuitInterpreter) shouldBe Left(
        Map(
          purePure("testFormComponent") -> Set(
            "Enter a phone number, like 01632 960 001, 07700 900 982 or +44 0770 090 0175"
          )
        )
      )
    }
  }

  it should "return valid when character count is between 7-25" in {
    val program7 = TextChecker.validatePhoneNumber(testFormComponent, "1234567")
    program7.foldMap(ShortCircuitInterpreter) shouldBe ().asRight.asRight
    val program25 = TextChecker.validatePhoneNumber(testFormComponent, List.fill(25)('1').mkString)
    program25.foldMap(ShortCircuitInterpreter) shouldBe ().asRight.asRight
  }

  it should "return invalid when character count is less than 7" in {
    val invalidNumber = numberWithoutPlus.map(string => string.substring(0, 6))
    forAll(invalidNumber) { phoneNumber =>
      val result = TextChecker.validatePhoneNumber(testFormComponent, phoneNumber)
      result.foldMap(ShortCircuitInterpreter) shouldBe Left(
        Map(
          purePure("testFormComponent") -> Set(
            "Enter a phone number, like 01632 960 001, 07700 900 982 or +44 0770 090 0175"
          )
        )
      )
    }
  }

  it should "return invalid when character count is greater than 25" in {
    val invalidNumber = Gen.const("1234567890 1234567890 12345")
    forAll(invalidNumber) { phoneNumber =>
      val result = TextChecker.validatePhoneNumber(testFormComponent, phoneNumber)
      result.foldMap(ShortCircuitInterpreter) shouldBe Left(
        Map(
          purePure("testFormComponent") -> Set(
            "Enter a phone number, like 01632 960 001, 07700 900 982 or +44 0770 090 0175"
          )
        )
      )
    }
  }

  private val shortTextComponent = FormComponent(
    FormComponentId("testFormComponent"),
    Text(ShortText(3, 5), Value),
    toSmartString("formComponentLabel"),
    false,
    None,
    None,
    None,
    None,
    Mandatory.TRUE,
    true,
    false,
    true,
    false,
    None
  )

  "validateShortText" should "return invalid if character count is too big" in {
    val shortTextTooLong = "abcdefghij"
    val result = TextChecker.validateShortTextConstraint(shortTextComponent, shortTextTooLong, 3, 5)
    result.foldMap(ShortCircuitInterpreter) shouldBe Left(
      Map(
        purePure("testFormComponent") -> Set(
          "formComponentLabel must be 5 characters or less"
        )
      )
    )
  }

  it should "return invalid if character count is too small" in {
    val shortTextTooShort = "a"
    val result = TextChecker.validateShortTextConstraint(shortTextComponent, shortTextTooShort, 3, 5)
    result.foldMap(ShortCircuitInterpreter) shouldBe Left(
      Map(
        purePure("testFormComponent") -> Set(
          "formComponentLabel must be 3 characters or more"
        )
      )
    )
  }

  it should "return valid if the character count is within range" in {
    val shortTextWithinRange = "abcd"
    val result = TextChecker.validateShortTextConstraint(shortTextComponent, shortTextWithinRange, 3, 5)
    result.foldMap(ShortCircuitInterpreter) shouldBe ().asRight.asRight
  }

  it should "return invalid if incorrect character are entered" in {
    val shortTextIncorrectChars = "a[]*"
    val result =
      TextChecker.validateShortTextConstraint(shortTextComponent, shortTextIncorrectChars, 3, 5)
    result.foldMap(ShortCircuitInterpreter) shouldBe Left(
      Map(
        purePure("testFormComponent") -> Set(
          "formComponentLabel must only include letters, numbers, spaces, hyphens, ampersands and apostrophes"
        )
      )
    )
  }

  private val textComponent = FormComponent(
    FormComponentId("testFormComponent"),
    TextArea(ShortText.default, Value, dataThreshold = None),
    toSmartString("formComponentLabel"),
    false,
    None,
    None,
    None,
    None,
    Mandatory.TRUE,
    true,
    false,
    true,
    false,
    None
  )

  "textValidationWithConstraints" should "return valid if there are all valid characters" in {
    val textWithAllValidCharacters =
      "This would make my 80 percent of your average trading profits for 3 months £730.60."
    val result = TextChecker.textValidationWithConstraints(textComponent, textWithAllValidCharacters, 3, 100)
    result.foldMap(ShortCircuitInterpreter) shouldBe ().asRight.asRight
  }

  it should "return invalid if there are some invalid characters" in {
    val textWithInvalidCharacters =
      "This would ^ make my 80 percent of your ^ average | trading profits for  | 3 months £730.60."

    val result = TextChecker.textValidationWithConstraints(textComponent, textWithInvalidCharacters, 3, 100)
    result.foldMap(ShortCircuitInterpreter) shouldBe Left(
      Map(
        purePure("testFormComponent") -> Set(
          "formComponentLabel cannot include the characters ^ |"
        )
      )
    )

  }

  "validateText" should "validate when FormComponent constraint is WholeSterling(true)" in {
    val constraint = WholeSterling(true, RoundingMode.defaultRoundingMode)
    val fc = textComponent.copy(`type` = Text(constraint, Value))
    val table = TableDrivenPropertyChecks.Table(
      ("input", "expected"),
      ("1", ().asRight.asRight),
      (
        "-1",
        Left(Map(textComponent.id.modelComponentId -> Set("Amount must be 0 or more")))
      ),
      (
        "1.1",
        Left(Map(textComponent.id.modelComponentId -> Set("Amount must not include pence")))
      ),
      (
        "-1.1",
        Left(Map(textComponent.id.modelComponentId -> Set("Amount must not include pence")))
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (inputData, expected) =>
      val formModelOptics = mkFormModelOptics(
        mkFormTemplate(mkSection(textComponent.copy(`type` = Text(constraint, Value)))),
        mkDataOutOfDate(textComponent.id.value -> inputData)
      )
      val result = TextChecker.validateText(fc, constraint, formTemplate, envelopeId)(
        formModelOptics.formModelVisibilityOptics,
        new LookupRegistry(Map.empty)
      )
      result.foldMap(ShortCircuitInterpreter) shouldBe expected
    }
  }

  it should "validate when FormComponent constraint is UkVrn" in {
    val constraint = UkVrn
    val fc = textComponent.copy(`type` = Text(constraint, Value))
    val table = TableDrivenPropertyChecks.Table(
      ("input", "expected"),
      ("107 5563 20", ().asRight.asRight),
      ("XI 107556375", ().asRight.asRight),
      ("GB107556375", ().asRight.asRight),
      (
        "107 5563 201",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter a VAT registration number in the correct format")))
      ),
      (
        "XI10755637",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter a VAT registration number in the correct format")))
      ),
      (
        "GL107556375",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter a VAT registration number in the correct format")))
      ),
      (
        "107 5563 21",
        Left(
          Map(
            textComponent.id.modelComponentId -> Set(
              "The VAT registration number you entered does not exist. Enter a real VAT number"
            )
          )
        )
      ),
      (
        "XI 107556376",
        Left(
          Map(
            textComponent.id.modelComponentId -> Set(
              "The VAT registration number you entered does not exist. Enter a real VAT number"
            )
          )
        )
      ),
      (
        "GB107556376",
        Left(
          Map(
            textComponent.id.modelComponentId -> Set(
              "The VAT registration number you entered does not exist. Enter a real VAT number"
            )
          )
        )
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (inputData, expected) =>
      val formModelOptics = mkFormModelOptics(
        mkFormTemplate(mkSection(textComponent.copy(`type` = Text(constraint, Value)))),
        mkDataOutOfDate(textComponent.id.value -> inputData)
      )
      val result = TextChecker.validateText(fc, constraint, formTemplate, envelopeId)(
        formModelOptics.formModelVisibilityOptics,
        new LookupRegistry(Map.empty)
      )
      result.foldMap(ShortCircuitInterpreter) shouldBe expected
    }
  }

  it should "validate when FormComponent constraint is YearFormat" in {
    val constraint = YearFormat
    val fc = textComponent.copy(`type` = Text(constraint, Value))
    val table = TableDrivenPropertyChecks.Table(
      ("input", "expected"),
      ("1900", ().asRight.asRight),
      ("2099", ().asRight.asRight),
      ("2020", ().asRight.asRight),
      (
        "1899",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter a year in the correct format")))
      ),
      (
        "2101",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter a year in the correct format")))
      ),
      (
        "",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter a year")))
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (inputData, expected) =>
      val formModelOptics = mkFormModelOptics(
        mkFormTemplate(mkSection(textComponent.copy(`type` = Text(constraint, Value)))),
        mkDataOutOfDate(textComponent.id.value -> inputData)
      )
      val result = TextChecker.validateText(fc, constraint, formTemplate, envelopeId)(
        formModelOptics.formModelVisibilityOptics,
        new LookupRegistry(Map.empty)
      )
      result.foldMap(ShortCircuitInterpreter) shouldBe expected
    }
  }

  it should "validate when FormComponent constraint is TimeFormat" in {
    val constraint = TimeFormat
    val fc = textComponent.copy(`type` = Text(constraint, Value))
    val table = TableDrivenPropertyChecks.Table(
      ("input", "expected"),
      ("00", ().asRight.asRight),
      ("12am", ().asRight.asRight),
      ("12AM", ().asRight.asRight),
      ("12pm", ().asRight.asRight),
      ("12PM", ().asRight.asRight),
      ("00:59", ().asRight.asRight),
      ("00.59", ().asRight.asRight),
      ("00:59am", ().asRight.asRight),
      ("0059AM", ().asRight.asRight),
      ("12.59am", ().asRight.asRight),
      ("12.59pm", ().asRight.asRight),
      ("12:59pm", ().asRight.asRight),
      ("1259pm", ().asRight.asRight),
      ("2359", ().asRight.asRight),
      ("23:59", ().asRight.asRight),
      ("23.59", ().asRight.asRight),
      ("2359pm", ().asRight.asRight),
      ("2020", ().asRight.asRight),
      ("05.15", ().asRight.asRight),
      ("0515", ().asRight.asRight),
      ("05 15", ().asRight.asRight),
      ("5:am", ().asRight.asRight),
      ("5.am", ().asRight.asRight),
      ("515am", ().asRight.asRight),
      ("515.am", ().asRight.asRight),
      ("5 15", ().asRight.asRight),
      ("5:15", ().asRight.asRight),
      ("515", ().asRight.asRight),
      ("0515pm", ().asRight.asRight),
      ("05:15am", ().asRight.asRight),
      ("05:15a.m", ().asRight.asRight),
      ("05:15p.m.", ().asRight.asRight),
      ("05.15am", ().asRight.asRight),
      ("5:00am	", ().asRight.asRight),
      ("13", ().asRight.asRight),
      ("00", ().asRight.asRight),
      ("05", ().asRight.asRight),
      ("5.", ().asRight.asRight),
      ("3", ().asRight.asRight),
      (
        "1899",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter a time in the correct format")))
      ),
      (
        "foo",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter a time in the correct format")))
      ),
      (
        "",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter a time")))
      ),
      (
        "12",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter 12am for midnight or 12pm for midday")))
      ),
      (
        "12.00",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter 12:00am for midnight or 12:00pm for midday")))
      ),
      (
        "12:30",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter 12:30am or 12:30pm")))
      ),
      (
        "12:49",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter 12:49am or 12:49pm")))
      ),
      (
        "0",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter a time in the correct format")))
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (inputData, expected) =>
      val formModelOptics = mkFormModelOptics(
        mkFormTemplate(mkSection(textComponent.copy(`type` = Text(constraint, Value)))),
        mkDataOutOfDate(textComponent.id.value -> inputData)
      )
      val result = TextChecker.validateText(fc, constraint, formTemplate, envelopeId)(
        formModelOptics.formModelVisibilityOptics,
        new LookupRegistry(Map.empty)
      )
      result.foldMap(ShortCircuitInterpreter) shouldBe expected
    }
  }

  it should "validate when FormComponent constraint is TimeFormat (Welsh)" in {
    val constraint = TimeFormat
    val fc = textComponent.copy(`type` = Text(constraint, Value))
    val table = TableDrivenPropertyChecks.Table(
      ("input", "expected"),
      ("00", ().asRight.asRight),
      ("12yb", ().asRight.asRight),
      ("12YB", ().asRight.asRight),
      ("12yp", ().asRight.asRight),
      ("12YP", ().asRight.asRight),
      ("00:59", ().asRight.asRight),
      ("00.59", ().asRight.asRight),
      ("00:59yb", ().asRight.asRight),
      ("0059YB", ().asRight.asRight),
      ("12.59yb", ().asRight.asRight),
      ("12.59yp", ().asRight.asRight),
      ("12:59yp", ().asRight.asRight),
      ("1259yp", ().asRight.asRight),
      ("2359", ().asRight.asRight),
      ("23:59", ().asRight.asRight),
      ("23.59", ().asRight.asRight),
      ("2359yp", ().asRight.asRight),
      ("2020", ().asRight.asRight),
      ("05.15", ().asRight.asRight),
      ("0515", ().asRight.asRight),
      ("05 15", ().asRight.asRight),
      ("5:yb", ().asRight.asRight),
      ("5.yb", ().asRight.asRight),
      ("515yb", ().asRight.asRight),
      ("515.yb", ().asRight.asRight),
      ("5 15", ().asRight.asRight),
      ("5:15", ().asRight.asRight),
      ("515", ().asRight.asRight),
      ("0515yp", ().asRight.asRight),
      ("05:15yb", ().asRight.asRight),
      ("05:15y.b", ().asRight.asRight),
      ("05:15y.p.", ().asRight.asRight),
      ("05.15yb", ().asRight.asRight),
      ("5:00yb	", ().asRight.asRight),
      ("13", ().asRight.asRight),
      ("00", ().asRight.asRight),
      ("05", ().asRight.asRight),
      ("5.", ().asRight.asRight),
      ("3", ().asRight.asRight),
      (
        "1899",
        Left(Map(textComponent.id.modelComponentId -> Set("Nodwch amser yn y fformat cywir")))
      ),
      (
        "foo",
        Left(Map(textComponent.id.modelComponentId -> Set("Nodwch amser yn y fformat cywir")))
      ),
      (
        "",
        Left(Map(textComponent.id.modelComponentId -> Set("Nodwch amser")))
      ),
      (
        "12",
        Left(
          Map(textComponent.id.modelComponentId -> Set("Nodwch 12yb ar gyfer hanner nos neu 12yp ar gyfer hanner dydd"))
        )
      ),
      (
        "12.00",
        Left(
          Map(
            textComponent.id.modelComponentId -> Set(
              "Nodwch 12:00yb ar gyfer hanner nos neu 12:00yp ar gyfer hanner dydd"
            )
          )
        )
      ),
      (
        "12:30",
        Left(Map(textComponent.id.modelComponentId -> Set("Nodwch 12:30yb neu 12:30yp")))
      ),
      (
        "12:49",
        Left(Map(textComponent.id.modelComponentId -> Set("Nodwch 12:49yb neu 12:49yp")))
      ),
      (
        "0",
        Left(Map(textComponent.id.modelComponentId -> Set("Nodwch amser yn y fformat cywir")))
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (inputData, expected) =>
      val formModelOptics = mkFormModelOptics(
        mkFormTemplate(mkSection(textComponent.copy(`type` = Text(constraint, Value)))),
        mkDataOutOfDate(textComponent.id.value -> inputData)
      )
      val result = TextChecker.validateText(fc, constraint, formTemplate, envelopeId)(
        formModelOptics.formModelVisibilityOptics,
        new LookupRegistry(Map.empty)
      )(messagesApi.preferred(Seq(Lang("cy"))), LangADT.Cy, smartStringEvaluator)
      result.foldMap(ShortCircuitInterpreter) shouldBe expected
    }
  }

  it should "validate when FormComponent constraint is lookup(country)" in new WithLookupData {
    val table = TableDrivenPropertyChecks.Table(
      ("input", "expected", "label", "shortName", "errorShortName"),
      (
        "",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter Country"))),
        toSmartString("Residence Country"),
        Some(toSmartString("Country")),
        None
      ),
      (
        "England",
        Left(
          Map(
            textComponent.id.modelComponentId -> mutable.LinkedHashSet(
              """No match for "England". Select a country from the list"""
            )
          )
        ),
        toSmartString("Residence Country"),
        Some(toSmartString("Country")),
        None
      ),
      (
        "",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter Residence Country"))),
        toSmartString("Residence Country"),
        None,
        None
      ),
      (
        "",
        Left(Map(textComponent.id.modelComponentId -> Set("Enter country of residence"))),
        toSmartString("Residence Country"),
        None,
        Some(toSmartString("country of residence"))
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (inputData, expected, label, shortName, errorShortName) =>
      val fc = textComponent
        .copy(`type` = Text(constraint, Value))
        .copy(errorShortName = errorShortName)
        .copy(shortName = shortName)
        .copy(label = label)
      val formModelOptics = mkFormModelOptics(
        mkFormTemplate(mkSection(textComponent.copy(`type` = Text(constraint, Value)))),
        mkDataOutOfDate(textComponent.id.value -> inputData)
      )

      val result = TextChecker.validateText(fc, constraint, formTemplate, envelopeId)(
        formModelOptics.formModelVisibilityOptics,
        new LookupRegistry(
          Map(
            Register.Country -> AjaxLookup(
              countryLookupOptions,
              lookupLoader.mkIndexSearcher("country"),
              ShowAll.Enabled
            )
          )
        )
      )
      result.foldMap(ShortCircuitInterpreter) shouldBe expected
    }
  }

  it should "validate when FormComponent constraint is lookup(port)" in new WithLookupDataPort {
    val fc = textComponent
      .copy(`type` = Text(constraint, Value))
      .copy(errorShortName = None)
      .copy(shortName = None)
      .copy(label = toSmartString("Port"))
    val formModelOptics = mkFormModelOptics(
      mkFormTemplate(mkSection(textComponent.copy(`type` = Text(constraint, Value)))),
      mkDataOutOfDate(textComponent.id.value -> "Not a port")
    )
    val result = TextChecker.validateText(fc, constraint, formTemplate, envelopeId)(
      formModelOptics.formModelVisibilityOptics,
      new LookupRegistry(
        Map(
          Register.Port -> AjaxLookup(
            portLookupOptions,
            lookupLoader.mkIndexSearcher("port"),
            ShowAll.Enabled
          )
        )
      )
    )
    result.foldMap(ShortCircuitInterpreter) shouldBe Left(
      Map(
        textComponent.id.modelComponentId ->
          mutable.LinkedHashSet("""No match for "Not a port". Select a value from the list.""")
      )
    )
  }

  private def purePure(fieldId: String) =
    ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId(fieldId)))

  trait WithLookupData {

    val countryLookupSelectionCriteria: Option[List[SelectionCriteria]] = None
    val constraint = Lookup(Register.Country, countryLookupSelectionCriteria)
    lazy val countryLookupOptions: LocalisedLookupOptions = LocalisedLookupOptions(
      Map(
        LangADT.En -> LookupOptions(
          Map(
            LookupLabel("United Kingdom") -> CountryLookupInfo(
              LookupId("GB"),
              0,
              LookupPriority(1),
              LookupPriority(1),
              LookupRegion("1"),
              LookupInGibraltarEuEeaEfta("1"),
              Map()
            ),
            LookupLabel("United States") -> CountryLookupInfo(
              LookupId("US"),
              1,
              LookupPriority(1),
              LookupPriority(1),
              LookupRegion("2"),
              LookupInGibraltarEuEeaEfta("1"),
              Map()
            )
          )
        )
      )
    )
  }

  trait WithLookupDataPort {

    val portLookupSelectionCriteria: Option[List[SelectionCriteria]] = None
    val constraint = Lookup(Register.Port, portLookupSelectionCriteria)
    lazy val portLookupOptions: LocalisedLookupOptions = LocalisedLookupOptions(
      Map(
        LangADT.En -> LookupOptions(
          Map(
            LookupLabel("LHR") -> PortLookupInfo(
              LookupId("LHR"),
              0,
              LookupPriority(1),
              LookupRegion("1"),
              LookupPortType("Airport"),
              LookupCountryCode("GB"),
              LookupPortCode("ZZ11ZZ")
            )
          )
        )
      )
    )
  }
}
