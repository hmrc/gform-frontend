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
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Address
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.validation.ComponentsValidator
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AddressCheckerSpec
    extends AnyFlatSpecLike with Matchers with EitherMatchers with ScalaFutures with GraphSpec
    with VariadicFormDataSupport with FormModelSupport with IdiomaticMockito {

  val environment = Environment.simple()
  val configuration = Configuration.load(environment)
  val langs = new DefaultLangs()
  val httpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)

  val messagesApi: MessagesApi =
    new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get

  implicit val messages: Messages = messagesApi.preferred(Seq(langs.availables.head))

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

  val baseAddress = Address(international = false, mandatoryFields = List(), countyDisplayed = false, value = None)
  val baseFormComponent =
    FormComponent(
      FormComponentId("x"),
      baseAddress,
      toSmartString("l"),
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
    override def apply(s: SmartString, markDown: Boolean): String = s.rawValue(_ => false)(LangADT.En)
    override def evalEnglish(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
  }

  private def componentsValidator(
    formTemplate: FormTemplate,
    formComponent: FormComponent,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ) = {

    val fmb = mkFormModelFromSections(formTemplate.formKind.allSections.sections)

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

  "non-international" should "accept uk, street1, street3, streep 3, street4 and postcode" in {
    val speccedAddress = baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
      "x-street2"  -> "S2",
      "x-street3"  -> "S3",
      "x-street4"  -> "S4",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue

    result.value should be(())
  }

  "non-international" should "accept uk, street1 and postcode only" in {
    val speccedAddress = baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S",
      "x-postcode" -> "RG1 1AA"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result.value should be(())
  }

  "non-international" should "should return invalid for uk, street1, street2, street3, street4 but an invalid postcode AC15" in {
    val speccedAddress = baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent.copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
      "x-street2"  -> "S2",
      "x-street3"  -> "S3",
      "x-street4"  -> "S4",
      "x-postcode" -> "BN11 7YHP"
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
            "Enter a real postcode"
          )
        )
      )
    )
  }

  "non-international" should "should return invalid for uk, street1, street2, street3, street4 but an invalid postcode AC16" in {
    val speccedAddress = baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, None, errorExample = Some("like AA1 1AA"))

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
      "x-street2"  -> "S2",
      "x-street3"  -> "S3",
      "x-street4"  -> "S4",
      "x-postcode" -> "BN11 7YHP"
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
            "Enter a real postcode, like AA1 1AA"
          )
        )
      )
    )
  }

  "non-international" should "return invalid for postcode, but no street1, AC1" in {
    val speccedAddress = baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street1")) -> Set(
            "Enter address line 1"
          )
        )
      )
    )
  }

  "non-international" should "return invalid for postcode, but no street1, AC2" in {
    val speccedAddress = baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street1")) -> Set(
            "Enter business address line 1"
          )
        )
      )
    )
  }

  "non-international" should "return invalid if don't enter a town or city if mandatory, AC3" in {
    val speccedAddress =
      baseAddress.copy(international = false, mandatoryFields = List(Address.Configurable.Mandatory.City))
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street3")) -> Set(
            "Enter town or city"
          )
        )
      )
    )
  }

  "non-international" should "return invalid if don't enter a town or city if mandatory, AC4" in {
    val speccedAddress =
      baseAddress.copy(international = false, mandatoryFields = List(Address.Configurable.Mandatory.City))
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street3")) -> Set(
            "Enter business address town or city"
          )
        )
      )
    )
  }

  "non-international" should "return invalid for street1 but no postcode AC5" in {
    val speccedAddress = baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
    // .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"      -> "true",
      "x-street1" -> "S"
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
  "non-international" should "return invalid for street1 but no postcode AC6" in {
    val speccedAddress = baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(errorShortName = Some("business address"), None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"      -> "true",
      "x-street1" -> "S"
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

  "non-international" should "return invalid when enter 36 characters in building and street, AC7" in {
    val speccedAddress =
      baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street1")) -> Set(
            "Address line 1 must be 35 characters or less"
          )
        )
      )
    )
  }

  "non-international" should "return invalid when enter 36 characters in building and street, AC8" in {
    val speccedAddress =
      baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street1")) -> Set(
            "Business address line 1 must be 35 characters or less"
          )
        )
      )
    )
  }

  "non-international" should "return invalid when enter 36 characters in building and street line2, AC9" in {
    val speccedAddress =
      baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
      "x-street2"  -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street2")) -> Set(
            "Address line 2 must be 35 characters or less"
          )
        )
      )
    )
  }

  "non-international" should "return invalid when enter 36 characters in building and street line2, AC10" in {
    val speccedAddress =
      baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
      "x-street2"  -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street2")) -> Set(
            "Business address line 2 must be 35 characters or less"
          )
        )
      )
    )
  }

  "non-international" should "return invalid when enter 36 characters in town or city, AC11" in {
    val speccedAddress =
      baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
      "x-street3"  -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street3")) -> Set(
            "Town or city must be 35 characters or less"
          )
        )
      )
    )
  }

  "non-international" should "return invalid when enter 36 characters in town or city, AC12" in {
    val speccedAddress =
      baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
      "x-street3"  -> List.fill(ValidationValues.addressLine + 1)("a").mkString,
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street3")) -> Set(
            "Business address town or city must be 35 characters or less"
          )
        )
      )
    )
  }

  "non-international" should "return invalid when enter 36 characters in town or city, AC13" in {
    val speccedAddress =
      baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, None, None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
      "x-street4"  -> List.fill(ValidationValues.addressLine4 + 1)("a").mkString,
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street4")) -> Set(
            "County must be 27 characters or less"
          )
        )
      )
    )
  }

  "non-international" should "return invalid when enter 36 characters in town or city, AC14" in {
    val speccedAddress =
      baseAddress.copy(international = false)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)
      .withErrorFields(None, errorShortNameStart = Some("Business address"), None)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> "S1",
      "x-street4"  -> List.fill(ValidationValues.addressLine4 + 1)("a").mkString,
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
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street4")) -> Set(
            "Business address county must be 27 characters or less"
          )
        )
      )
    )
  }

  "international" should "accept not uk, street1, country" in {
    val speccedAddress = baseAddress.copy(international = true)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"      -> "false",
      "x-street1" -> "S",
      "x-country" -> "C"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue
    result.value should be(())
  }

  "international" should "return invalid for not uk, street1, but no country" in {
    val speccedAddress = baseAddress.copy(international = true)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"      -> "false",
      "x-street1" -> "S"
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
            "l country must be entered"
          )
        )
      )
    )
  }

  "international" should "return invalid for not uk, street1, postcode and country" in {
    val speccedAddress = baseAddress.copy(international = true)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "false",
      "x-street1"  -> "S",
      "x-postcode" -> "P1 1P",
      "x-country"  -> "C"
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
            "l must not be entered"
          )
        )
      )
    )
  }

  "international" should "return invalid for uk, street1, country, but no postcode" in {
    val speccedAddress = baseAddress.copy(international = true)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"      -> "true",
      "x-street1" -> "S",
      "x-country" -> "C"
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
          ),
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> Set(
            "l must not be entered"
          )
        )
      )
    )
  }

  "Address validation" should "fail when field separator is wrong" in {
    val speccedAddress = baseAddress.copy(international = true)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x@uk"      -> "true",
      "x@street1" -> "S",
      "x@country" -> "C"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue

    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street1")) -> Set(
            "l line 1 must be entered"
          ),
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("country")) -> Set(
            "l country must be entered"
          )
        )
      )
    )
  }

  "Address validation" should "pass with valid data" in {
    val speccedAddress = baseAddress.copy(international = true)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> List.fill(ValidationValues.addressLine)("a").mkString,
      "x-street2"  -> List.fill(ValidationValues.addressLine)("a").mkString,
      "x-street3"  -> List.fill(ValidationValues.addressLine)("a").mkString,
      "x-street4"  -> List.fill(ValidationValues.addressLine4)("a").mkString,
      "x-postcode" -> "RG1 1PQ"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue

    result.value should be(())
  }

  "Address validation" should "pass with valid required data omitting the optional" in {
    val speccedAddress = baseAddress.copy(international = true)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> List.fill(ValidationValues.addressLine)("a").mkString,
      "x-postcode" -> "RG1 1PQ"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue

    result.value should be(())
  }

  "Address validation" should "fail when the field is fails validation" in {
    val speccedAddress = baseAddress.copy(international = true)
    val speccedFormComponent = baseFormComponent
      .copy(`type` = speccedAddress)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "x-uk"       -> "true",
      "x-street1"  -> List.fill(36)("a").mkString,
      "x-street2"  -> List.fill(36)("a").mkString,
      "x-street3"  -> List.fill(36)("a").mkString,
      "x-street4"  -> List.fill(28)("a").mkString,
      "x-postcode" -> "RG1 1PQ"
    )
    val result: ValidatedType[Unit] =
      componentsValidator(mkFormTemplate(mkSection(speccedFormComponent)), speccedFormComponent, data)
        .validate(GetEmailCodeFieldMatcher.noop)
        .futureValue

    result should be(
      Invalid(
        Map(
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street1")) -> Set(
            "Address line 1 must be 35 characters or less"
          ),
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street2")) -> Set(
            "Address line 2 must be 35 characters or less"
          ),
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street3")) -> Set(
            "Town or city must be 35 characters or less"
          ),
          ModelComponentId
            .Atomic(IndexedComponentId.Pure(speccedFormComponent.id.baseComponentId), Atom("street4")) -> Set(
            "County must be 27 characters or less"
          )
        )
      )
    )
  }

}
