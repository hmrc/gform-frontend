/*
 * Copyright 2024 HM Revenue & Customs
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

import cats.data.Validated.Valid
import org.junit.Assert.assertTrue
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito
import org.mockito.Mockito.never
import org.mockito.MockitoSugar.{ times, verify, when }
import org.scalatestplus.mockito.MockitoSugar.mock
import org.typelevel.ci.CIString
import play.api.http.HttpConfiguration
import play.api.i18n._
import play.api.{ Configuration, Environment }
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.mkFormTemplate
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.email.{ EmailFieldId, emailFieldId }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.email.{ ConfirmationCodeWithEmailService, EmailConfirmationCode, EmailTemplateId }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailVerifiedBy, FormComponent, FormComponentId, FormTemplate, Mandatory, ShortText, TemplateSectionIndex, Text, Value }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, ExecutionContext, Future }

class ValidationServiceSpec extends Spec with FormModelSupport with VariadicFormDataSupport {
  override val envelopeId: EnvelopeId = EnvelopeId("dummy")

  private val environment: Environment = Environment.simple()
  private val configuration: Configuration = Configuration.load(environment)
  private val langs: Langs = new DefaultLangs()
  private val httpConfiguration: HttpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)
  private val messagesApi: MessagesApi =
    new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get

  implicit val messages: Messages = messagesApi.preferred(Seq(langs.availables.head))
  implicit val lang: LangADT = LangADT.En

  private val emailField: FormComponent = FormComponent(
    FormComponentId("email"),
    Text(
      EmailVerifiedBy(
        FormComponentId("code"),
        EmailVerifierService.digitalContact(EmailTemplateId("code_template"), None)
      ),
      Value
    ),
    toSmartString("Email"),
    false,
    None,
    None,
    None,
    None,
    Mandatory.True,
    true,
    true,
    false,
    false,
    None,
    None
  )
  private val verifyField: FormComponent = FormComponent(
    FormComponentId("code"),
    Text(ShortText.default, Value),
    toSmartString("Verify code"),
    false,
    None,
    None,
    None,
    None,
    Mandatory.True,
    true,
    true,
    false,
    false,
    None,
    None
  )
  private val emailStr: String = "user@test.com"
  private val confirmationCode: String = "ABCD"
  private val formTemplate: FormTemplate = mkFormTemplate(
    ExampleData.nonRepeatingPageSection(fields = List(emailField)),
    ExampleData.nonRepeatingPageSection(fields = List(verifyField))
  )

  private val booleanExprEval: BooleanExprEval[Future] = new BooleanExprEval()
  private val gformConnector: GformConnector = mock[GformConnector]
  private val lookupRegistry: LookupRegistry = mock[LookupRegistry]

  private val validationService: ValidationService = new ValidationService(
    booleanExprEval,
    gformConnector,
    lookupRegistry,
    ComponentChecker.NonShortCircuitInterpreter
  )

  "validatePageModel" should "send 1 email to user@test.com when validating first page" in {
    val variadicFormData: VariadicFormData[OutOfDate] =
      VariadicFormData.create[OutOfDate]((emailField.modelComponentId, VariadicValue.One(emailStr)))

    val result: ValidatedType[ValidatorsResult] = setupAndRun(variadicFormData, false, 0)

    verify(gformConnector, times(1))
      .sendEmail(any[ConfirmationCodeWithEmailService]())(any[HeaderCarrier](), any[ExecutionContext]())

    result match {
      case Valid(ValidatorsResult(map)) =>
        val eac: Option[EmailAndCode] = map.get(emailFieldId(emailField.id))
        eac.isDefined shouldBe true
        eac.get.email shouldBe CIString(emailStr)
      case _ => assertTrue(false)
    }
  }

  it should "not attempt to send any emails when validating first page if email already sent" in {
    val variadicFormData: VariadicFormData[OutOfDate] =
      VariadicFormData.create[OutOfDate]((emailField.modelComponentId, VariadicValue.One(emailStr)))

    val result: ValidatedType[ValidatorsResult] = setupAndRun(variadicFormData, true, 0)

    verify(gformConnector, never())
      .sendEmail(any[ConfirmationCodeWithEmailService]())(any[HeaderCarrier](), any[ExecutionContext]())

    result match {
      case Valid(ValidatorsResult(map)) => map.size shouldBe 0
      case _                            => assertTrue(false)
    }
  }

  it should "fail validation for invalid verification code on second page" in {
    val variadicFormData: VariadicFormData[OutOfDate] =
      VariadicFormData.create[OutOfDate](
        (emailField.modelComponentId, VariadicValue.One(emailStr)),
        (verifyField.modelComponentId, VariadicValue.One("WXYZ"))
      )

    val validationResult: ValidationResult = setupAndRunWithEvaluation(variadicFormData, true, 1)

    verify(gformConnector, never())
      .sendEmail(any[ConfirmationCodeWithEmailService]())(any[HeaderCarrier](), any[ExecutionContext]())
    validationResult.isFormValid shouldBe false
    validationResult.errorsFieldIds.contains(verifyField.id) shouldBe true
  }

  it should "successfully validate correct verification code on second page" in {
    val variadicFormData: VariadicFormData[OutOfDate] =
      VariadicFormData.create[OutOfDate](
        (emailField.modelComponentId, VariadicValue.One(emailStr)),
        (verifyField.modelComponentId, VariadicValue.One(confirmationCode))
      )

    val validationResult: ValidationResult = setupAndRunWithEvaluation(variadicFormData, true, 1)

    verify(gformConnector, never())
      .sendEmail(any[ConfirmationCodeWithEmailService]())(any[HeaderCarrier](), any[ExecutionContext]())

    validationResult.isFormValid shouldBe true
  }

  private def setupAndRunWithEvaluation(
    variadicFormData: VariadicFormData[OutOfDate],
    icludeEmailAndCodeMap: Boolean,
    pageToValidate: Int
  ): ValidationResult = {
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, variadicFormData)
    val visibilityFormModel: FormModel[Visibility] = formModelOptics.formModelVisibilityOptics.formModel
    val visibilityPageModel: PageModel[Visibility] = visibilityFormModel(
      Classic.NormalPage(TemplateSectionIndex(pageToValidate))
    )
    val result: ValidatedType[ValidatorsResult] = setupAndRun(variadicFormData, icludeEmailAndCodeMap, pageToValidate)

    ValidationUtil.evaluateValidationResult(
      visibilityPageModel.allFormComponents,
      result,
      formModelOptics.formModelVisibilityOptics,
      envelopeWithMapping
    )
  }

  private def setupAndRun(
    variadicFormData: VariadicFormData[OutOfDate],
    includeEmailAndCodeMap: Boolean,
    pageToValidate: Int
  ): ValidatedType[ValidatorsResult] = {
    Mockito.clearInvocations(gformConnector)
    when(
      gformConnector.sendEmail(any[ConfirmationCodeWithEmailService]())(any[HeaderCarrier](), any[ExecutionContext]())
    ).thenReturn(Future.successful(()))

    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, variadicFormData)
    val visibilityFormModel: FormModel[Visibility] = formModelOptics.formModelVisibilityOptics.formModel
    val visibilityPageModel: PageModel[Visibility] = visibilityFormModel(
      Classic.NormalPage(TemplateSectionIndex(pageToValidate))
    )

    val emailAndCodeMap: Map[EmailFieldId, EmailAndCode] =
      if (includeEmailAndCodeMap)
        Map(
          emailFieldId(emailField.id) -> EmailAndCode(
            CIString(emailStr),
            EmailConfirmationCode(CIString(confirmationCode))
          )
        )
      else Map.empty

    val cacheData: CacheData = new CacheData(
      envelopeId,
      ThirdPartyData(
        NotChecked,
        emailAndCodeMap,
        QueryParams.empty,
        None,
        BooleanExprCache.empty,
        None,
        None,
        None,
        None,
        None,
        None,
        None
      ),
      formTemplate
    )

    val resultF: Future[ValidatedType[ValidatorsResult]] = validationService.validatePageModel(
      visibilityPageModel,
      cacheData,
      envelopeWithMapping,
      formModelOptics.formModelVisibilityOptics,
      GetEmailCodeFieldMatcher(visibilityFormModel)
    )

    Await.result(resultF, Duration.Inf)
  }
}
