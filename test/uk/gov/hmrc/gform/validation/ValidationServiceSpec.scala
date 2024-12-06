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
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkFormTemplate }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.email.{ EmailFieldId, emailFieldId }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FormModel, FormModelSupport, PageModel, VariadicFormDataSupport, Visibility }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.email.{ ConfirmationCodeWithEmailService, EmailConfirmationCode, EmailTemplateId }
import uk.gov.hmrc.gform.sharedmodel.form.{ EmailAndCode, EnvelopeId, FormModelOptics, QueryParams, ThirdPartyData, ValidatorsResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailVerifiedBy, FormComponent, FormComponentId, FormTemplate, Text, Value }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, ExecutionContext, Future }

class ValidationServiceSpec extends Spec with FormModelSupport with VariadicFormDataSupport {
  override val envelopeId: EnvelopeId = EnvelopeId("dummy")

  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  private val evb: EmailVerifiedBy = EmailVerifiedBy(
    FormComponentId("code"),
    EmailVerifierService.digitalContact(EmailTemplateId("code_template"), None)
  )
  private val emailField: FormComponent = mkFormComponent("email", Text(evb, Value))
  private val emailStr: String = "user@test.com"
  private val formTemplate: FormTemplate = mkFormTemplate(
    ExampleData.nonRepeatingPageSection(fields = List(emailField))
  )
  private val variadicFormData: VariadicFormData[OutOfDate] =
    VariadicFormData.create[OutOfDate]((emailField.modelComponentId, VariadicValue.One(emailStr)))
  private val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, variadicFormData)
  private val visibilityFormModel: FormModel[Visibility] = formModelOptics.formModelVisibilityOptics.formModel
  private val visibilityPageModel: PageModel[Visibility] = visibilityFormModel(Classic(0))

  private val booleanExprEval: BooleanExprEval[Future] = new BooleanExprEval()
  private val gformConnector: GformConnector = mock[GformConnector]
  private val lookupRegistry: LookupRegistry = mock[LookupRegistry]

  private val validationService: ValidationService = new ValidationService(
    booleanExprEval,
    gformConnector,
    lookupRegistry,
    ComponentChecker.NonShortCircuitInterpreter
  )

  "sendVerificationEmails" should "send 1 email to user@test.com" in {
    Mockito.clearInvocations(gformConnector)
    val cacheData: CacheData = new CacheData(
      envelopeId,
      ThirdPartyData.empty,
      formTemplate
    )

    when(
      gformConnector.sendEmail(any[ConfirmationCodeWithEmailService]())(any[HeaderCarrier](), any[ExecutionContext]())
    ).thenReturn(Future.successful(()))

    val resultF: Future[ValidatedType[ValidatorsResult]] = validationService.validatePageModelWithoutCustomValidators(
      visibilityPageModel,
      cacheData,
      envelopeWithMapping,
      formModelOptics.formModelVisibilityOptics,
      GetEmailCodeFieldMatcher(visibilityFormModel)
    )

    val result: ValidatedType[ValidatorsResult] = Await.result(resultF, Duration.Inf)
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

  it should "not attempt to send any emails" in {
    Mockito.clearInvocations(gformConnector)
    val eacMap: Map[EmailFieldId, EmailAndCode] = Map[EmailFieldId, EmailAndCode](
      emailFieldId(emailField.id) -> EmailAndCode(CIString(emailStr), EmailConfirmationCode(CIString("CODE")))
    )
    val cacheData: CacheData = new CacheData(
      envelopeId,
      ThirdPartyData(
        NotChecked,
        eacMap,
        QueryParams.empty,
        None,
        BooleanExprCache.empty,
        None,
        None,
        None,
        None,
        None,
        None
      ),
      formTemplate
    )

    when(
      gformConnector.sendEmail(any[ConfirmationCodeWithEmailService]())(any[HeaderCarrier](), any[ExecutionContext]())
    ).thenReturn(Future.successful(()))

    val resultF: Future[ValidatedType[ValidatorsResult]] = validationService.validatePageModelWithoutCustomValidators(
      visibilityPageModel,
      cacheData,
      envelopeWithMapping,
      formModelOptics.formModelVisibilityOptics,
      GetEmailCodeFieldMatcher(visibilityFormModel)
    )

    val result: ValidatedType[ValidatorsResult] = Await.result(resultF, Duration.Inf)
    verify(gformConnector, never())
      .sendEmail(any[ConfirmationCodeWithEmailService]())(any[HeaderCarrier](), any[ExecutionContext]())

    result match {
      case Valid(ValidatorsResult(map)) => map.size shouldBe 0
      case _                            => assertTrue(false)
    }
  }

}
