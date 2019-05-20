/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.instances.future._
import cats.scalatest.EitherMatchers
import cats.scalatest.ValidatedValues._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar.mock
import org.scalatest.{ FlatSpec, Matchers }
import play.api.i18n.Messages
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.GraphSpec
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, FormComponent, FormComponentId }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class AddressValidationSpec(implicit messages: Messages, l: LangADT)
    extends FlatSpec with Matchers with EitherMatchers with ScalaFutures with GraphSpec {
  val retrievals = mock[MaterialisedRetrievals]

  val baseAddress = Address(international = false)
  val baseListItem =
    FormComponent(
      FormComponentId("x"),
      baseAddress,
      toLocalisedString("l"),
      None,
      None,
      None,
      true,
      true,
      false,
      true,
      false,
      None)
  val tempList: List[FormComponent] = List(baseListItem, baseListItem)

  private val lookupRegistry = new LookupRegistry(Map.empty)

  implicit lazy val hc = HeaderCarrier()

  private def mkComponentsValidator(data: FormDataRecalculated): ComponentsValidator =
    new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ThirdPartyData.empty,
      ExampleData.formTemplate,
      lookupRegistry)

  "non-international" should "accept uk, street1, street3, streep 3, street4 and postcode" in {
    val address = Address(international = false)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val tempList: List[FormComponent] = List(speccedAddress, speccedAddress)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")       -> Seq("true"),
        FormComponentId("x-street1")  -> Seq("S1"),
        FormComponentId("x-street2")  -> Seq("S2"),
        FormComponentId("x-street3")  -> Seq("S3"),
        FormComponentId("x-street4")  -> Seq("S4"),
        FormComponentId("x-postcode") -> Seq("P1 1P")
      ))
    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.value should be(())
  }

  "non-international" should "accept uk, street1 and postcode only" in {
    val address = Address(international = false)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")       -> Seq("true"),
        FormComponentId("x-street1")  -> Seq("S"),
        FormComponentId("x-postcode") -> Seq("P1 1P")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.value should be(())
  }

  "non-international" should "should return invalid for uk, street1, street2, street3, street4 but an invalid postcode" in {
    val address = Address(international = false)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")       -> Seq("true"),
        FormComponentId("x-street1")  -> Seq("S1"),
        FormComponentId("x-street2")  -> Seq("S2"),
        FormComponentId("x-street3")  -> Seq("S3"),
        FormComponentId("x-street4")  -> Seq("S4"),
        FormComponentId("x-postcode") -> Seq("BN11 7YHP")
      ))
    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.toEither should beLeft(
      Map(speccedAddress.id.withSuffix("postcode") -> Set("l postcode is longer than 8 characters")))
  }

  "non-international" should "return invalid for postcode, but no street1" in {
    val address = Address(international = false)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")       -> Seq("true"),
        FormComponentId("x-postcode") -> Seq("P1 1P")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.toEither should beLeft(
      Map(speccedAddress.id.withSuffix("street1") -> Set("l Building and street must be entered")))
  }

  "non-international" should "return invalid for street1 but no postcode" in {
    val address = Address(international = false)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")      -> Seq("true"),
        FormComponentId("x-street1") -> Seq("S")
      ))

    val result: ValidatedType[Unit] = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ThirdPartyData.empty,
      ExampleData.formTemplate,
      lookupRegistry).validate(speccedAddress, tempList).futureValue

    result.toEither should beLeft(Map(speccedAddress.id.withSuffix("postcode") -> Set("l postcode must be entered")))
  }

  "international" should "accept not uk, street1, country" in {
    val address = Address(international = true)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")      -> Seq("false"),
        FormComponentId("x-street1") -> Seq("S"),
        FormComponentId("x-country") -> Seq("C")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.value should be(())
  }

  "international" should "return invalid for not uk, street1, but no country" in {
    val address = Address(international = true)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")      -> Seq("false"),
        FormComponentId("x-street1") -> Seq("S")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.toEither should beLeft(Map(speccedAddress.id.withSuffix("country") -> Set("l Country must be entered")))
  }

  "international" should "return invalid for not uk, street1, postcode and country" in {
    val address = Address(international = true)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")       -> Seq("false"),
        FormComponentId("x-street1")  -> Seq("S"),
        FormComponentId("x-postcode") -> Seq("P1 1P"),
        FormComponentId("x-country")  -> Seq("C")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.toEither should beLeft(Map(speccedAddress.id.withSuffix("postcode") -> Set("l must not be entered")))
  }

  "international" should "return invalid for uk, street1, country, but no postcode" in {
    val address = Address(international = true)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")      -> Seq("true"),
        FormComponentId("x-street1") -> Seq("S"),
        FormComponentId("x-country") -> Seq("C")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.toEither should beLeft(
      Map(
        speccedAddress.id.withSuffix("postcode") -> Set("l postcode must be entered"),
        speccedAddress.id.withSuffix("country")  -> Set("l must not be entered")
      ))
  }

  "Address validation" should "fail when field separator is wrong" in {
    val address = Address(international = true)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x@uk")      -> Seq("true"),
        FormComponentId("x@street1") -> Seq("S"),
        FormComponentId("x@country") -> Seq("C")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.toEither should beLeft(
      Map(
        FormComponentId("x-country") -> Set("l Country must be entered"),
        FormComponentId("x-street1") -> Set("l line 1 must be entered")
      )
    )
  }

  "Address validation" should "throw back custom validation" in {
    val address = Address(international = true)

    val speccedAddress = FormComponent(
      FormComponentId("x"),
      address,
      toLocalisedString("l"),
      None,
      None,
      None,
      true,
      true,
      false,
      true,
      false,
      Some(toLocalisedString("New Error Message")))

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x@uk")      -> Seq("true"),
        FormComponentId("x@street1") -> Seq("S"),
        FormComponentId("x@country") -> Seq("C")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.toEither should beLeft(
      Map(
        FormComponentId("x-country") -> Set("New Error Message"),
        FormComponentId("x-street1") -> Set("New Error Message")
      )
    )
  }

  "Address validation" should "pass with valid data" in {
    val address = Address(international = true)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")       -> Seq("true"),
        FormComponentId("x-street1")  -> Seq(List.fill(ValidationValues.addressLine)("a").mkString),
        FormComponentId("x-street2")  -> Seq(List.fill(ValidationValues.addressLine)("a").mkString),
        FormComponentId("x-street3")  -> Seq(List.fill(ValidationValues.addressLine)("a").mkString),
        FormComponentId("x-street4")  -> Seq(List.fill(ValidationValues.addressLine4)("a").mkString),
        FormComponentId("x-postcode") -> Seq("C")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.toEither should beRight(())
  }

  "Address validation" should "pass with valid required data omitting the optional" in {
    val address = Address(international = true)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")       -> Seq("true"),
        FormComponentId("x-street1")  -> Seq(List.fill(ValidationValues.addressLine)("a").mkString),
        FormComponentId("x-postcode") -> Seq("C")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.toEither should beRight(())
  }

  "Address validation" should "fail when the field is fails validation" in {
    val address = Address(international = true)

    val speccedAddress =
      FormComponent(
        FormComponentId("x"),
        address,
        toLocalisedString("l"),
        None,
        None,
        None,
        true,
        true,
        false,
        true,
        false,
        None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("x-uk")       -> Seq("true"),
        FormComponentId("x-street1")  -> Seq(List.fill(36)("a").mkString),
        FormComponentId("x-street2")  -> Seq(List.fill(36)("a").mkString),
        FormComponentId("x-street3")  -> Seq(List.fill(36)("a").mkString),
        FormComponentId("x-street4")  -> Seq(List.fill(28)("a").mkString),
        FormComponentId("x-postcode") -> Seq("C")
      ))

    val result: ValidatedType[Unit] = mkComponentsValidator(data).validate(speccedAddress, tempList).futureValue

    result.toEither should beLeft(
      Map(
        FormComponentId("x-street1") -> Set("l Building and street is longer than 35 characters"),
        FormComponentId("x-street2") -> Set("l Building and street line 2 is longer than 35 characters"),
        FormComponentId("x-street3") -> Set("l Town or city is longer than 35 characters"),
        FormComponentId("x-street4") -> Set("l County is longer than 27 characters")
      )
    )
  }
}
