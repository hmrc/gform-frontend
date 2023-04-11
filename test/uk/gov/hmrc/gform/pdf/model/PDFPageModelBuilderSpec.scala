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

package uk.gov.hmrc.gform.pdf.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.i18n.Messages
import play.api.test.Helpers
import play.twirl.api.Html
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.auth.models.Role
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluator }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ addToListQuestion => _, _ }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FormModelSupport, SectionSelectorType }
import uk.gov.hmrc.gform.pdf.model.PDFModel._
import uk.gov.hmrc.gform.pdf.model.PDFPageModelBuilder.makeModel
import uk.gov.hmrc.gform.sharedmodel.ExampleData._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormField, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, FormComponent, FormTemplate, FormTemplateWithRedirects, Instruction, InvisiblePageTitle, Section, Value }
import uk.gov.hmrc.gform.validation.{ FieldOk, ValidationResult }

import scala.collection.immutable.List
import uk.gov.hmrc.gform.lookup.LocalisedLookupOptions

class PDFPageModelBuilderSpec extends AnyFlatSpec with Matchers with FormModelSupport {

  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  trait Fixture {

    lazy val fcName: FormComponent = buildFormComponent(
      "name",
      Value,
      Some(Instruction(Some(toSmartString("name-instruction")), Some(1)))
    )

    lazy val fcEmail: FormComponent = buildFormComponent(
      "email",
      Value,
      Some(Instruction(Some(toSmartString("email-instruction")), Some(2)))
    )

    lazy val fcAge: FormComponent = buildFormComponent(
      "age",
      Value,
      Some(Instruction(Some(toSmartString("age-instruction")), Some(3)))
    )

    lazy val fcAmount: FormComponent = buildFormComponent(
      "amount",
      Value
    )

    lazy val addToListQuestionComponent: FormComponent = addToListQuestion("addToListQuestion")

    lazy val sections: List[Section] = List(
      mkSection(
        List(fcName)
      )
    )

    lazy val form: Form =
      buildForm(
        FormData(List.empty)
      )

    lazy val validationResult: ValidationResult = new ValidationResult(
      Map(
        fcName.id -> FieldOk(fcName, "name&value")
      ),
      None
    )

    lazy val formTemplate: FormTemplate = mkFormTemplate(sections)
    lazy val cache: AuthCacheWithForm =
      AuthCacheWithForm(
        retrievals,
        form,
        FormTemplateWithRedirects.noRedirects(formTemplate, None),
        Role.Customer,
        maybeAccessCode,
        LocalisedLookupOptions(Map())
      )
    lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
      mkFormModelOptics(formTemplate, cache.variadicFormData[SectionSelectorType.WithDeclaration])
        .asInstanceOf[FormModelOptics[DataOrigin.Mongo]]

    implicit lazy val smartStringEvaluator: SmartStringEvaluator = new RealSmartStringEvaluatorFactory()
      .apply(formModelOptics.formModelVisibilityOptics)
  }

  "PDFPageModelBuilder.makeModel - PDFType.Summary" should "build model for non-repeating section" in new Fixture {
    makeModel[DataOrigin.Mongo, PDFType.Summary](
      formModelOptics,
      cache,
      envelopeWithMapping,
      validationResult
    ) shouldBe List(
      PageData(Some("Section Name"), List(SimpleField(Some("name"), List(Html("name&amp;value")))), "0")
    )
  }

  it should "build model for repeating section" in new Fixture {
    override lazy val sections: List[Section] = List(
      mkRepeatingPageSection(
        List(fcName),
        Constant("2")
      )
    )
    override lazy val validationResult: ValidationResult = new ValidationResult(
      Map(
        fcName.id.withIndex(1) -> FieldOk(fcName.copy(id = fcName.id.withIndex(1)), "name-value1"),
        fcName.id.withIndex(2) -> FieldOk(fcName.copy(id = fcName.id.withIndex(2)), "name-value2")
      ),
      None
    )

    makeModel[DataOrigin.Mongo, PDFType.Summary](
      formModelOptics,
      cache,
      envelopeWithMapping,
      validationResult
    ) shouldBe List(
      PageData(Some("Section Name"), List(SimpleField(Some("name"), List(Html("name-value1")))), "0"),
      PageData(Some("Section Name"), List(SimpleField(Some("name"), List(Html("name-value2")))), "1")
    )
  }

  it should "build model for add-to-list section" in new Fixture {

    override lazy val sections: List[Section] = List(
      mkAddToListSection(
        page(List(fcName, fcAge)),
        page(List(fcEmail))
      )
    )

    override lazy val form: Form =
      buildForm(
        FormData(
          List(
            FormField(addToListQuestionComponent.withIndex(1).modelComponentId, "0"),
            FormField(addToListQuestionComponent.withIndex(2).modelComponentId, "1")
          )
        )
      )
    override lazy val validationResult: ValidationResult = new ValidationResult(
      Map(
        fcName.id.withIndex(1)  -> FieldOk(fcName.copy(id = fcName.id.withIndex(1)), "name-value1"),
        fcName.id.withIndex(2)  -> FieldOk(fcName.copy(id = fcName.id.withIndex(2)), "name-value2"),
        fcAge.id.withIndex(1)   -> FieldOk(fcAge.copy(id = fcAge.id.withIndex(1)), "1"),
        fcAge.id.withIndex(2)   -> FieldOk(fcAge.copy(id = fcAge.id.withIndex(2)), "2"),
        fcEmail.id.withIndex(1) -> FieldOk(fcEmail.copy(id = fcEmail.id.withIndex(1)), "somename1@test.com"),
        fcEmail.id.withIndex(2) -> FieldOk(fcEmail.copy(id = fcEmail.id.withIndex(2)), "name-value2@test.com"),
        addToListQuestionComponent.id.withIndex(1) -> FieldOk(
          addToListQuestionComponent.copy(id = addToListQuestionComponent.id.withIndex(1)),
          "0"
        ),
        addToListQuestionComponent.id.withIndex(2) -> FieldOk(
          addToListQuestionComponent.copy(id = addToListQuestionComponent.id.withIndex(2)),
          "1"
        )
      ),
      None
    )

    makeModel[DataOrigin.Mongo, PDFType.Summary](
      formModelOptics,
      cache,
      envelopeWithMapping,
      validationResult
    ) shouldBe List(
      AddToListData(
        "Pet owner summaryName",
        AddToListSummary(
          "Pet owner title",
          List(Html("<p>Pet owner summary description</p>"), Html("<p>Pet owner summary description</p>"))
        ),
        List(
          AddToListPageGroup(
            "Pet owner shortName",
            List(
              PageData(
                Some("Section Name"),
                List(SimpleField(Some("name"), List(Html("name-value1"))), SimpleField(Some("age"), List(Html("1")))),
                "0"
              ),
              PageData(Some("Section Name"), List(SimpleField(Some("email"), List(Html("somename1@test.com")))), "1")
            ),
            "addToListQuestion0"
          ),
          AddToListPageGroup(
            "Pet owner shortName",
            List(
              PageData(
                Some("Section Name"),
                List(SimpleField(Some("name"), List(Html("name-value2"))), SimpleField(Some("age"), List(Html("2")))),
                "3"
              ),
              PageData(Some("Section Name"), List(SimpleField(Some("email"), List(Html("name-value2@test.com")))), "4")
            ),
            "addToListQuestion1"
          )
        ),
        "addToListQuestion"
      )
    )
  }

  it should "not return page title if presentation hint is InvisiblePageTitle" in new Fixture {

    override lazy val sections: List[Section] = List(
      mkSection(
        List(fcName),
        None,
        Some(InvisiblePageTitle)
      )
    )

    override lazy val validationResult: ValidationResult = new ValidationResult(
      Map(
        fcName.id -> FieldOk(fcName, "name-value")
      ),
      None
    )

    makeModel[DataOrigin.Mongo, PDFType.Summary](
      formModelOptics,
      cache,
      envelopeWithMapping,
      validationResult
    ) shouldBe List(
      PageData(
        None,
        List(
          SimpleField(Some("name"), List(Html("name-value")))
        ),
        "0"
      )
    )
  }

  "PDFPageModelBuilder.makeModel - PDFType.Instruction" should
    "build model, with instructions page and fields sorted by instruction order" in new Fixture {

      override lazy val sections: List[Section] = List(
        mkSection(
          List(fcName),
          Some(Instruction(Some(toSmartString("page1-instruction")), Some(2)))
        ),
        mkSection(
          List(fcAge, fcEmail),
          Some(Instruction(Some(toSmartString("page2-instruction")), Some(1)))
        ),
        mkSection(
          List(fcAmount)
        )
      )
      override lazy val validationResult: ValidationResult = new ValidationResult(
        Map(
          fcName.id   -> FieldOk(fcName, "name-value"),
          fcAge.id    -> FieldOk(fcAge, "1"),
          fcEmail.id  -> FieldOk(fcEmail, "somename@test.com"),
          fcAmount.id -> FieldOk(fcAmount, "111")
        ),
        None
      )

      makeModel[DataOrigin.Mongo, PDFType.Instruction](
        formModelOptics,
        cache,
        envelopeWithMapping,
        validationResult
      ) shouldBe List(
        PageData(
          Some("page2-instruction"),
          List(
            SimpleField(Some("email-instruction"), List(Html("somename@test.com"))),
            SimpleField(Some("age-instruction"), List(Html("1")))
          ),
          "1"
        ),
        PageData(
          Some("page1-instruction"),
          List(
            SimpleField(Some("name-instruction"), List(Html("name-value")))
          ),
          "0"
        )
      )
    }

  it should "skip page title when instruction is not defined" in new Fixture {

    makeModel[DataOrigin.Mongo, PDFType.Instruction](
      formModelOptics,
      cache,
      envelopeWithMapping,
      validationResult
    ) shouldBe List(
      PageData(
        None,
        List(
          SimpleField(Some("name-instruction"), List(Html("name&amp;value")))
        ),
        "0"
      )
    )
  }

  it should "skip fields that don't have instruction" in new Fixture {

    override lazy val sections: List[Section] = List(
      mkSection(
        List(fcEmail, fcName.copy(instruction = None)),
        Some(instruction)
      )
    )

    override lazy val validationResult: ValidationResult = new ValidationResult(
      Map(
        fcName.id  -> FieldOk(fcName, "name-value"),
        fcAge.id   -> FieldOk(fcAge, "1"),
        fcEmail.id -> FieldOk(fcEmail, "somename@test.com")
      ),
      None
    )

    makeModel[DataOrigin.Mongo, PDFType.Instruction](
      formModelOptics,
      cache,
      envelopeWithMapping,
      validationResult
    ) shouldBe List(
      PageData(
        Some("some-instruction"),
        List(
          SimpleField(Some("email-instruction"), List(Html("somename@test.com")))
        ),
        "0"
      )
    )
  }

  it should "skip page if none of its fields have instructions defined" in new Fixture {

    override lazy val sections: List[Section] = List(
      mkSection(
        List(fcName.copy(instruction = None)),
        Some(instruction)
      )
    )

    makeModel[DataOrigin.Mongo, PDFType.Instruction](
      formModelOptics,
      cache,
      envelopeWithMapping,
      validationResult
    ) shouldBe List.empty
  }

}
