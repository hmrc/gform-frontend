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

import cats.data.Validated.Invalid
import cats.instances.future._
import cats.scalatest.EitherMatchers
import cats.scalatest.ValidatedValues._
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.Configuration
import play.api.Environment
import play.api.http.HttpConfiguration
import play.api.i18n._
import uk.gov.hmrc.gform.GraphSpec
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.FormModelSupport
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.VariadicFormDataSupport
import uk.gov.hmrc.gform.models.ids.IndexedComponentId
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OverseasAddress
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class OverseasAddressCheckerSpec
    extends AnyFlatSpecLike with Matchers with EitherMatchers with ScalaFutures with GraphSpec
    with VariadicFormDataSupport with FormModelSupport with IdiomaticMockito {

  val environment = Environment.simple()
  val configuration =
    Configuration.from(Map("play.i18n.langs" -> Seq("en", "cy"))).withFallback(Configuration.load(environment))
  val langs = new DefaultLangsProvider(configuration).get
  val httpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)
  val messagesApi: MessagesApi =
    new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get
  implicit val messages: Messages = messagesApi.preferred(Seq(Lang("en")))
  implicit val l: LangADT = LangADT.En

  override val retrievals = mock[MaterialisedRetrievals]

  implicit class FormComponentOps(formComponent: FormComponent) {
    def withErrorFields(
      errorShortName: Option[String],
      errorShortNameStart: Option[String],
      errorExample: Option[String]
    ): FormComponent =
      formComponent.copy(
        errorShortName = errorShortName.map(toSmartString),
        errorShortNameStart = errorShortNameStart.map(toSmartString),
        errorExample = errorExample.map(toSmartString)
      )
  }

  val baseAddress = OverseasAddress(
    mandatoryFields = List(),
    optionalFields = List(),
    countryLookup = false,
    value = None,
    countryDisplayed = true,
    selectionCriteria = None
  )
  val baseFormComponent =
    FormComponent(
      FormComponentId("x"),
      baseAddress,
      toSmartString("l"),
      false,
      None,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None
    )

  private val lookupRegistry = new LookupRegistry(Map.empty)

  implicit val smartStringEvaluator: SmartStringEvaluator = new SmartStringEvaluator {
    override def apply(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
    override def evalEnglish(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
  }

  private def componentsValidator(
    formTemplate: FormTemplate,
    formComponent: FormComponent,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ) = {

    val fmb = mkFormModelFromSections(formTemplate.formKind.allSections.sections.map(_.section))

    val fmvo = fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

    val cacheData = new CacheData(
      EnvelopeId(""),
      ThirdPartyData.empty,
      formTemplate
    )

    mkComponentsValidator(fmvo, formComponent, cacheData)
  }

  private def mkComponentsValidator(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    formComponent: FormComponent,
    cacheData: CacheData
  ): ComponentsValidator[DataOrigin.Mongo, Future] =
    new ComponentsValidator(
      formModelVisibilityOptics,
      formComponent,
      cacheData,
      EnvelopeWithMapping.empty,
      lookupRegistry,
      booleanExprEval,
      ComponentChecker.NonShortCircuitInterpreter,
      true
    )

  "OverseasAddress validation" should "accept line1, line2, line3, city,  postcode and country" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue

    result.value should be(())
  }

  it should "return invalid when line1 is not entered, AC1" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line1")) -> Set(
            "Enter address line 1"
          )
        )
      )
    )
  }

  it should "return invalid when line1 is not entered, AC2" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line1")) -> Set(
            "Enter business address line 1"
          )
        )
      )
    )
  }

  it should "return invalid when line2 is not entered, AC3" in {
    val speccedAddress = baseAddress.copy(mandatoryFields = List(OverseasAddress.Configurable.Mandatory.Line2))
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line2")) -> Set(
            "Enter address line 2"
          )
        )
      )
    )
  }

  it should "return invalid when line2 is not entered, AC4" in {
    val speccedAddress = baseAddress.copy(mandatoryFields = List(OverseasAddress.Configurable.Mandatory.Line2))
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line2")) -> Set(
            "Enter address line 2"
          )
        )
      )
    )
  }

  it should "return invalid when line3 is not entered, AC5" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result.value should be(())
  }

  it should "return invalid when line3 is not entered, AC6" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S2",
      "x-line2"    -> "S2",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result.value should be(())
  }

  it should "return invalid when town and city is not entered, AC7" in {
    val speccedAddress = baseAddress.copy(optionalFields = List())
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("city")) -> Set(
            "Enter town or city"
          )
        )
      )
    )
  }

  it should "return invalid when town and city is not entered, AC8" in {
    val speccedAddress = baseAddress.copy(optionalFields = List())
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S2",
      "x-line2"    -> "S2",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("city")) -> Set(
            "Enter business address town or city"
          )
        )
      )
    )
  }

  it should "return invalid when postcode is not entered, AC9" in {
    val speccedAddress = baseAddress.copy(mandatoryFields = List(OverseasAddress.Configurable.Mandatory.Postcode))
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"   -> "S1",
      "x-line2"   -> "S2",
      "x-city"    -> "City",
      "x-country" -> "Slovakia"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("postcode")) -> Set(
            "Enter postcode"
          )
        )
      )
    )
  }

  it should "return invalid when postcode is not entered, AC10" in {
    val speccedAddress = baseAddress.copy(mandatoryFields = List(OverseasAddress.Configurable.Mandatory.Postcode))
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"   -> "S2",
      "x-line2"   -> "S2",
      "x-city"    -> "City",
      "x-country" -> "Slovakia"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("postcode")) -> Set(
            "Enter business address postcode"
          )
        )
      )
    )
  }

  it should "return invalid when country is not entered, AC11" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1" -> "S1",
      "x-line2" -> "S2",
      "x-city"  -> "City"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> Set(
            "Enter a country"
          )
        )
      )
    )
  }

  it should "return invalid when country is not entered, AC12" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1" -> "S2",
      "x-line2" -> "S2",
      "x-city"  -> "City"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> Set(
            "Enter a country"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 1, AC13" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line1")) -> Set(
            "Address line 1 must be 35 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 1, AC14" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line1")) -> Set(
            "Business address line 1 must be 35 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 2, AC15" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line2")) -> Set(
            "Address line 2 must be 35 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 2, AC16" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line2")) -> Set(
            "Business address line 2 must be 35 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 3, AC17" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line3")) -> Set(
            "Address line 3 must be 35 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 3, AC18" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line3")) -> Set(
            "Business address line 3 must be 35 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 28 chars in town and city, AC19" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> List.fill(ValidationValues.overseasCity + 1)("a").mkString,
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("city")) -> Set(
            "Town or city must be 27 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in town and city, AC20" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> List.fill(ValidationValues.overseasCity + 1)("a").mkString,
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("city")) -> Set(
            "Business address town or city must be 27 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 9 chars in postcode, AC21" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-country"  -> "Slovakia",
      "x-city"     -> "City",
      "x-postcode" -> List.fill(ValidationValues.postcodeLimit + 1)("a").mkString
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("postcode")) -> Set(
            "Postcode must be 8 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 9 chars in postcode, AC22" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-country"  -> "Slovakia",
      "x-city"     -> "City",
      "x-postcode" -> List.fill(ValidationValues.postcodeLimit + 1)("a").mkString
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("postcode")) -> Set(
            "Business address postcode must be 8 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 51 chars in country, AC23" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-country"  -> List.fill(ValidationValues.countryLimit + 1)("a").mkString,
      "x-city"     -> "City",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> Set(
            "Country must be 50 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter 51 chars in country, AC24" in {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-country"  -> List.fill(ValidationValues.countryLimit + 1)("a").mkString,
      "x-city"     -> "City",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> Set(
            "Business address country must be 50 characters or less"
          )
        )
      )
    )
  }

  it should "return invalid when enter invalid country name, AC25" in {
    val speccedAddress = baseAddress.copy(countryLookup = true)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-country"  -> "Espana",
      "x-city"     -> "City",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> mutable
            .LinkedHashSet(
              """No match for "Espana". Select a country from the list"""
            )
        )
      )
    )
  }

  it should "return invalid when line1 is not entered, Welsh AC26/AC1" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line1")) -> Set(
            "Nodwch linell 1"
          )
        )
      )
    )
  }

  it should "return invalid when line1 is not entered, Welsh AC26/AC2" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line1")) -> Set(
            "Nodwch linell 1"
          )
        )
      )
    )
  }

  it should "return invalid when line2 is not entered, Welsh AC27/AC3" in new WithWelsh {
    val speccedAddress = baseAddress.copy(mandatoryFields = List(OverseasAddress.Configurable.Mandatory.Line2))
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line2")) -> Set(
            "Nodwch linell 2"
          )
        )
      )
    )
  }

  it should "return invalid when line2 is not entered, AC27/AC4" in new WithWelsh {
    val speccedAddress = baseAddress.copy(mandatoryFields = List(OverseasAddress.Configurable.Mandatory.Line2))
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line2")) -> Set(
            "Nodwch linell 2"
          )
        )
      )
    )
  }

  it should "not return invalid when line3 is not entered, AC28/AC5" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result.value should be(())
  }

  it should "not return invalid when line3 is not entered, Welsh AC28/AC6" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S2",
      "x-line2"    -> "S2",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result.value should be(())
  }

  it should "return invalid when town and city is not entered, Welsh AC29/AC7" in new WithWelsh {
    val speccedAddress = baseAddress.copy(optionalFields = List())
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("city")) -> Set(
            "Nodwch dref neu ddinas"
          )
        )
      )
    )
  }

  it should "return invalid when town and city is not entered, Welsh AC29/AC8" in new WithWelsh {
    val speccedAddress = baseAddress.copy(optionalFields = List())
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S2",
      "x-line2"    -> "S2",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("city")) -> Set(
            "Nodwch dref neu ddinas"
          )
        )
      )
    )
  }

  it should "return invalid when postcode is not entered, Welsh AC30/AC9" in new WithWelsh {
    val speccedAddress = baseAddress.copy(mandatoryFields = List(OverseasAddress.Configurable.Mandatory.Postcode))
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"   -> "S1",
      "x-line2"   -> "S2",
      "x-city"    -> "City",
      "x-country" -> "Slovakia"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("postcode")) -> Set(
            "Nodwch y cod post"
          )
        )
      )
    )
  }

  it should "return invalid when postcode is not entered, Welsh AC30/AC10" in new WithWelsh {
    val speccedAddress = baseAddress.copy(mandatoryFields = List(OverseasAddress.Configurable.Mandatory.Postcode))
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"   -> "S2",
      "x-line2"   -> "S2",
      "x-city"    -> "City",
      "x-country" -> "Slovakia"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("postcode")) -> Set(
            "Nodwch y cod post"
          )
        )
      )
    )
  }

  it should "return invalid when country is not entered, Welsh AC31/AC11" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1" -> "S1",
      "x-line2" -> "S2",
      "x-city"  -> "City"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> Set(
            "Nodwch wlad"
          )
        )
      )
    )
  }

  it should "return invalid when country is not entered, Welsh AC31/AC12" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1" -> "S2",
      "x-line2" -> "S2",
      "x-city"  -> "City"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> Set(
            "Nodwch wlad"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 1, Welsh AC32/AC13" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line1")) -> Set(
            "Mae’n rhaid i linell 1 fod yn 35 o gymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 1, Welsh AC32/AC14" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line1")) -> Set(
            "Mae’n rhaid i linell 1 fod yn 35 o gymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 2, Welsh  AC33/AC15" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line2")) -> Set(
            "Mae’n rhaid i linell 2 fod yn 35 o gymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 2, Welsh AC33/AC16" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-line3"    -> "S3",
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line2")) -> Set(
            "Mae’n rhaid i linell 2 fod yn 35 o gymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 3, Welsh AC34/AC17" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line3")) -> Set(
            "Mae’n rhaid i linell 3 fod yn 35 o gymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in line 3, Welsh AC34/AC18" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
      "x-city"     -> "City",
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("line3")) -> Set(
            "Mae’n rhaid i linell 3 fod yn 35 o gymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 28 chars in town and city, Welsh AC35/AC19" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> List.fill(ValidationValues.overseasCity + 1)("a").mkString,
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("city")) -> Set(
            "Mae’n rhaid i’r dref neu’r ddinas fod yn 27 o gymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 36 chars in town and city, Welsh AC35/AC20" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-city"     -> List.fill(ValidationValues.overseasCity + 1)("a").mkString,
      "x-country"  -> "Slovakia",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("city")) -> Set(
            "Mae’n rhaid i’r dref neu’r ddinas fod yn 27 o gymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 9 chars in postcode, Welsh AC36/AC21" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-country"  -> "Slovakia",
      "x-city"     -> "City",
      "x-postcode" -> List.fill(ValidationValues.postcodeLimit + 1)("a").mkString
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("postcode")) -> Set(
            "Mae’n rhaid i’r cod post fod yn 8 o gymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 9 chars in postcode, Welsh AC36/AC22" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-country"  -> "Slovakia",
      "x-city"     -> "City",
      "x-postcode" -> List.fill(ValidationValues.postcodeLimit + 1)("a").mkString
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("postcode")) -> Set(
            "Mae’n rhaid i’r cod post fod yn 8 o gymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 51 chars in country, Welsh AC37/AC23" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-country"  -> List.fill(ValidationValues.countryLimit + 1)("a").mkString,
      "x-city"     -> "City",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> Set(
            "Mae’n rhaid i’r wlad fod yn 50 o gyymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter 51 chars in country, Welsh AC37/AC24" in new WithWelsh {
    val speccedAddress = baseAddress
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-country"  -> List.fill(ValidationValues.countryLimit + 1)("a").mkString,
      "x-city"     -> "City",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> Set(
            "Mae’n rhaid i’r wlad fod yn 50 o gyymeriadau neu lai"
          )
        )
      )
    )
  }

  it should "return invalid when enter invalid country name, Welsh AC38/AC25" in new WithWelsh {
    val speccedAddress = baseAddress.copy(countryLookup = true)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-line1"    -> "S1",
      "x-line2"    -> "S2",
      "x-line3"    -> "S3",
      "x-country"  -> "Espana",
      "x-city"     -> "City",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> mutable
            .LinkedHashSet(
              """Dim byd yn cyd-fynd â "Espana". Dewiswch wlad o’r rhestr"""
            )
        )
      )
    )
  }

  trait WithWelsh {
    implicit val messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
    implicit val l: LangADT = LangADT.Cy

  }

}
