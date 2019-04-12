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

package uk.gov.hmrc.gform.controllers
import cats.Monad
import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatest.mockito.MockitoSugar
import play.api.i18n.I18nSupport
import uk.gov.hmrc.gform.auth.models.{ CheckEnrolmentsResult, MaterialisedRetrievals }
import uk.gov.hmrc.gform.auth.{ EnrolmentService, GovernmentGatewayConnector, Identifier, TaxEnrolmentsConnector }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.gform.{ EnrolmentController, SectionRenderingService }
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ AuthConfigGen, SectionGen }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EnrolmentPostCheck, EnrolmentSection, LegacyFcEnrolmentVerifier, ServiceId }
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnrolmentControllerSpec extends UnitSpec with MockitoSugar {

//  i18nSupport: I18nSupport,
//  auth: AuthenticatedRequestActions,
//  renderer: SectionRenderingService,
//  validationService: ValidationService,
//  enrolmentService: EnrolmentService,
//  appConfig: AppConfig,
//  recalculation: Recalculation[Future, Throwable],
//  taxEnrolmentConnector: TaxEnrolmentsConnector,
//  ggConnector: GovernmentGatewayConnector

  val mockI18n: I18nSupport = mock[I18nSupport]
  val mockAuthenticatedRequestActions: AuthenticatedRequestActions = mock[AuthenticatedRequestActions]
  val mockSectionRenderingService: SectionRenderingService = mock[SectionRenderingService]
  val mockValidationService: ValidationService = mock[ValidationService]
  val mockEnrolmentService: EnrolmentService = mock[EnrolmentService]
  val mockAppConfig: AppConfig = mock[AppConfig]
  val mockRecalculation: Recalculation[Future, Throwable] = mock[Recalculation[Future, Throwable]]
  val mockTaxEnrolmentsConnector: TaxEnrolmentsConnector = mock[TaxEnrolmentsConnector]
  val mockGovernmentGatewayConnector: GovernmentGatewayConnector = mock[GovernmentGatewayConnector]

  val controller = new EnrolmentController(
    mockI18n,
    mockAuthenticatedRequestActions,
    mockSectionRenderingService,
    mockValidationService,
    mockEnrolmentService,
    mockAppConfig,
    mockRecalculation,
    mockTaxEnrolmentsConnector,
    mockGovernmentGatewayConnector
  )

  val serviceId: ServiceId = ServiceId("someServiceId")
  val enrolmentSection: EnrolmentSection = getMeAGeneratedValue(SectionGen.enrolmentSectionGen)
  val postCheck: EnrolmentPostCheck = getMeAGeneratedValue(AuthConfigGen.enrolmentPostCheckGen)
//    def checkEnrolment[F[_]: Monad]: NonEmptyList[Identifier] => F[CheckEnrolmentsResult],
//    val validationResult: ValidatedType[Unit],
//    val legacyFcEnrolmentVerifier: Option[LegacyFcEnrolmentVerifier],
//    val retrievals: MaterialisedRetrievals

  private def getMeAGeneratedValue[A](a: Gen[A], tries: Int = 0): A =
    if (tries < 15)
      a.sample match {
        case Some(x) => x
        case None    => getMeAGeneratedValue(a, tries + 1)
      } else throw new Exception(s"failed to generate an instance of using")

//  forAll(SectionGen.enrolmentSectionGen) { obj =>
//    Section.format.reads(Section.format.writes(obj)) shouldBe JsSuccess(obj)
//  }

//  "test" should {
//    "asdasd" in {
//      true == true
//
//      val assertions = Seq()
//      prepareTestData(enrolmentSection, assertions)
//    }
//  }
//
//  private def prepareTestData(a: Gen[EnrolmentSection], assertions: Seq[scalatest.Assertion]): Unit ={
//    for {
//      enrolmentGen <- a
//    } yield {
//
//      assertions.
//      true shouldBe true
//
//    }
//  }
//
//
//  val testData = for {
//  enrolmentSection <- SectionGen.enrolmentSectionGen
//  } yield enrolmentSection
//  val postCheck: EnrolmentPostCheck,
//  val checkEnrolment: NonEmptyList[Identifier] => F[CheckEnrolmentsResult],
//  val validationResult: ValidatedType[Unit],
//  val legacyFcEnrolmentVerifier: Option[LegacyFcEnrolmentVerifier],
//  val retrievals: MaterialisedRetrievals

//  controller.processValidation()

  println("Asdasda" + enrolmentSection)
}
