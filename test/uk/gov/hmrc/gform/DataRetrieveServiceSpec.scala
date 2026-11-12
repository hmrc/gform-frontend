/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import munit.FunSuite

import org.mockito.ArgumentMatchers.any
import org.mockito.IdiomaticMockito
import org.mockito.MockitoSugar.{ times, verify, when }
import play.api.libs.json.Json
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector
import uk.gov.hmrc.gform.gform.DataRetrieveService
import uk.gov.hmrc.gform.eval.DataRetrieveEval
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DataRetrieveCtx, FormTemplateId }
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{ Millis, Span }
import scala.concurrent.{ ExecutionContext, Future }

class DataRetrieveServiceSpec extends FunSuite with Matchers with ScalaFutures with OptionValues with IdiomaticMockito {
  implicit private val ec: ExecutionContext = ExecutionContext.global
  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = scaled(Span(1000, Millis)), interval = scaled(Span(15, Millis)))

  test("FailureHandling should return first time failure request with 1 failure count") {
    new TestFixture {
      mockFailureResponse()

      val resF: Future[Option[DataRetrieveResult]] =
        callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), mkRequest(None, None))

      whenReady(resF) { maybeRes =>
        val newResetTime: LocalDateTime =
          LocalDateTime.now().truncatedTo(ChronoUnit.MINUTES).plusMinutes(resetMins.toLong + 1L)

        maybeRes.fold(DataRetrieveServiceSpec.this.assert(false)) { result =>
          result.failureCountResetTime shouldBe Some(newResetTime)
          result.failureCount shouldBe Some(1)
        }
      }
    }
  }

  test("FailureHandling should return Xth time failure request with incremented failure count") {
    new TestFixture {
      mockFailureResponse()

      val futureTime = LocalDateTime.now.plusMinutes(10).truncatedTo(ChronoUnit.MINUTES)
      val request = mkRequest(Some(2), Some(futureTime))

      val resF: Future[Option[DataRetrieveResult]] =
        callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), request)

      whenReady(resF) { maybeRes =>
        maybeRes.fold(DataRetrieveServiceSpec.this.assert(false)) { result =>
          result.failureCountResetTime shouldBe request.failureResetTime
          result.failureCount shouldBe request.previousFailureCount.map(_ + 1)
        }
      }
    }
  }

  test(
    "FailureHandling should return Xth time failure request with reset failure count and timeout if past previous block time"
  ) {
    new TestFixture {
      mockFailureResponse()

      val pastTime = LocalDateTime.now.minusMinutes(10).truncatedTo(ChronoUnit.MINUTES)
      val request = mkRequest(Some(2), Some(pastTime))

      val resF: Future[Option[DataRetrieveResult]] =
        callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), request)

      whenReady(resF) { maybeRes =>
        val newResetTime: LocalDateTime =
          LocalDateTime.now().truncatedTo(ChronoUnit.MINUTES).plusMinutes(resetMins.toLong + 1L)

        maybeRes.fold(DataRetrieveServiceSpec.this.assert(false)) { result =>
          result.failureCountResetTime shouldBe Some(newResetTime)
          result.failureCount shouldBe Some(1)
        }
      }
    }
  }

  test(
    "FailureHandling should return Xth time success request with reset failure count and timeout if past previous block time"
  ) {
    new TestFixture {
      mockSuccessResponse()

      val pastTime = LocalDateTime.now.minusMinutes(10).truncatedTo(ChronoUnit.MINUTES)
      val request = mkRequest(Some(2), Some(pastTime))

      val resF: Future[Option[DataRetrieveResult]] =
        callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), request)

      whenReady(resF) { maybeRes =>
        maybeRes.fold(DataRetrieveServiceSpec.this.assert(false)) { result =>
          result.failureCountResetTime shouldBe None
          result.failureCount shouldBe Some(0)
        }
      }
    }
  }

  test("FailureHandling should return first time success request with no failure count or reset time") {
    new TestFixture {
      mockSuccessResponse()

      val resF: Future[Option[DataRetrieveResult]] =
        callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), mkRequest(None, None))

      whenReady(resF) { maybeRes =>
        maybeRes.fold(DataRetrieveServiceSpec.this.assert(false)) { result =>
          result.failureCountResetTime shouldBe None
          result.failureCount shouldBe None
        }
      }
    }
  }

  test("FailureHandling should not execute failure processing if reset minutes not defined on dataRetrieve") {
    new TestFixture {
      mockValidateOnlyResponse()

      val resF: Future[Option[DataRetrieveResult]] =
        callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(None), mkRequest(None, None))

      whenReady(resF) { maybeRes =>
        verify(mockBankAccountReputationConnector, times(1))
          .validateBankDetails(any[DataRetrieve], any[DataRetrieve.Request])(any[HeaderCarrier])

        verify(mockBankAccountReputationConnector, times(0))
          .isFailure(any[DataRetrieve.Response])

        maybeRes.fold(DataRetrieveServiceSpec.this.assert(false)) { result =>
          result.failureCountResetTime shouldBe None
          result.failureCount shouldBe None
        }
      }
    }
  }

  test("Manual fallback should default to disabled for existing retrieval definitions and results") {
    new TestFixture {
      val dataRetrieve = validateBankDetailsDataRetrieve(None)
      val oldDefinition = Json.toJson(dataRetrieve).as[play.api.libs.json.JsObject] - "manualFallbackIsDesigned"
      oldDefinition.as[DataRetrieve].manualFallbackIsDesigned shouldBe None
      dataRetrieve.manualFallbackIsDesigned shouldBe None
      List(false, true).foreach { flag =>
        val configured = dataRetrieve.copy(manualFallbackIsDesigned = Some(flag))
        Json.toJson(configured).as[DataRetrieve] shouldBe configured
      }

      val result =
        DataRetrieveResult(dataRetrieve.id, dummyResponse.toRetrieveDataType(), Json.obj(), None, None, None, None)
      val oldResult = Json.toJson(result).as[play.api.libs.json.JsObject] - "hasFailed"
      oldResult.as[DataRetrieveResult].hasFailed shouldBe None
      Json.toJson(result.copy(hasFailed = Some(true))).as[DataRetrieveResult].hasFailed shouldBe Some(true)
      DataRetrieveEval.getDataRetrieveAttribute(
        Map(dataRetrieve.id -> oldResult.as[DataRetrieveResult]),
        DataRetrieveCtx(dataRetrieve.id, DataRetrieve.hasFailedAttribute)
      ) shouldBe Some(List("false"))
      (oldDefinition + ("manualFallbackIsDesigned" -> Json.toJson("true"))).validate[DataRetrieve].isError shouldBe true
      (oldResult + ("hasFailed" -> Json.toJson("true"))).validate[DataRetrieveResult].isError shouldBe true
    }
  }

  test("Manual fallback should fail the journey on an upstream error unless explicitly enabled") {
    new TestFixture {
      mockUpstreamResponse(CannotRetrieveResponse)
      List(None, Some(false)).foreach { flag =>
        val dataRetrieve = validateBankDetailsDataRetrieve(None).copy(manualFallbackIsDesigned = flag)
        val result = callDataRetrieveServiceWith(dataRetrieve, mkRequest(None, None))
        whenReady(result.failed)(_.getMessage shouldBe "Cannot retrieve data")
      }
    }
  }

  test("Manual fallback should fail the journey on an unclassified not-found response by default") {
    new TestFixture {
      mockUpstreamResponse(NotFound)
      List(None, Some(false)).foreach { flag =>
        val dataRetrieve = validateBankDetailsDataRetrieve(None).copy(manualFallbackIsDesigned = flag)
        val result = callDataRetrieveServiceWith(dataRetrieve, mkRequest(None, None))
        whenReady(result.failed)(_.getMessage shouldBe "Cannot retrieve data")
      }
    }
  }

  test("Manual fallback should persist a failed object result with empty attributes when enabled") {
    new TestFixture {
      mockUpstreamResponse(CannotRetrieveResponse)
      val dataRetrieve = validateBankDetailsDataRetrieve(None).copy(manualFallbackIsDesigned = Some(true))
      whenReady(callDataRetrieveServiceWith(dataRetrieve, mkRequest(None, None))) { result =>
        result.value.hasFailed shouldBe Some(true)
        val instructions = dataRetrieve.attributes match {
          case Attr.FromObject(inst) => inst
          case _                     => DataRetrieveServiceSpec.this.fail("Expected object attributes")
        }
        result.value.data shouldBe RetrieveDataType.ObjectType(instructions.map(_.attribute -> "").toMap)
        DataRetrieveEval.getDataRetrieveAttribute(
          Map(dataRetrieve.id -> result.value),
          DataRetrieveCtx(dataRetrieve.id, DataRetrieve.hasFailedAttribute)
        ) shouldBe Some(List("true"))
      }
    }
  }

  test("Manual fallback should expose failure independently of an empty list payload") {
    new TestFixture {
      mockUpstreamResponse(NotFound)
      val dataRetrieve = validateBankDetailsDataRetrieve(None).copy(
        manualFallbackIsDesigned = Some(true),
        attributes = Attr.FromArray(List.empty)
      )
      whenReady(callDataRetrieveServiceWith(dataRetrieve, mkRequest(None, None))) { result =>
        result.value.hasFailed shouldBe Some(true)
        result.value.data shouldBe RetrieveDataType.ListType(List.empty)
        DataRetrieveEval.getDataRetrieveAttribute(
          Map(dataRetrieve.id -> result.value),
          DataRetrieveCtx(dataRetrieve.id, DataRetrieve.hasFailedAttribute)
        ) shouldBe Some(List("true"))
      }
    }
  }

  test("Manual fallback should keep successful empty responses on the normal path") {
    new TestFixture {
      mockUpstreamResponse(ServiceResponse(DataRetrieve.Response.Array(List.empty)))
      val dataRetrieve = validateBankDetailsDataRetrieve(None).copy(
        manualFallbackIsDesigned = Some(true),
        attributes = Attr.FromArray(List.empty)
      )
      whenReady(callDataRetrieveServiceWith(dataRetrieve, mkRequest(None, None))) { result =>
        result.value.hasFailed shouldBe Some(false)
        DataRetrieveEval.getDataRetrieveAttribute(
          Map(dataRetrieve.id -> result.value),
          DataRetrieveCtx(dataRetrieve.id, DataRetrieve.hasFailedAttribute)
        ) shouldBe Some(List("false"))
      }
    }
  }

  List(false, true).foreach { manualFallback =>
    test(
      "Manual fallback" + " should " + s"keep a connector-classified no-record result on the normal path with fallback $manualFallback"
    ) {
      new TestFixture {
        val dataRetrieve = validateBankDetailsDataRetrieve(None).copy(manualFallbackIsDesigned = Some(manualFallback))
        val emptyResponse = DataRetrieve.Response.Object(
          Map(DataRetrieve.Attribute("isValid") -> "")
        )
        mockUpstreamResponse(ServiceResponse(emptyResponse))
        whenReady(callDataRetrieveServiceWith(dataRetrieve, mkRequest(None, None))) { result =>
          result.value.hasFailed shouldBe Some(false)
          result.value.data shouldBe emptyResponse.toRetrieveDataType()
          DataRetrieveEval.getDataRetrieveAttribute(
            Map(dataRetrieve.id -> result.value),
            DataRetrieveCtx(dataRetrieve.id, DataRetrieve.hasFailedAttribute)
          ) shouldBe Some(List("false"))
        }
      }
    }
  }

  test(
    "Manual fallback should not retry after recovery, changed parameters or callOnNoChange when fallback is saved"
  ) {
    new TestFixture {
      mockSuccessResponse()
      val dataRetrieve = validateBankDetailsDataRetrieve(None).copy(
        manualFallbackIsDesigned = Some(true),
        callOnNoChange = true,
        id = DataRetrieveId("1_bankDetails")
      )
      val failure = DataRetrieveResult(
        dataRetrieve.id,
        RetrieveDataType.ObjectType(Map.empty),
        Json.obj("accountNumber" -> "old"),
        None,
        None,
        None,
        hasFailed = Some(true)
      )
      val form = Form.dummy(FormTemplateId("4290"))
      val savedForm = form.copy(thirdPartyData = form.thirdPartyData.updateDataRetrieve(List(failure)))
      val resumedForm = Json.toJson(savedForm).as[Form]
      val request = mkRequest(None, None).copy(params = List("accountNumber" -> "changed"))
      val result = DataRetrieveService.retrieveDataResult(
        dataRetrieve,
        Some(resumedForm),
        request,
        Some(mockBankAccountReputationConnector),
        None,
        None,
        None,
        None,
        None,
        None
      )
      whenReady(result) { result =>
        result shouldBe None
        verify(mockBankAccountReputationConnector, times(0))
          .validateBankDetails(any[DataRetrieve], any[DataRetrieve.Request])(any[HeaderCarrier])
        resumedForm.thirdPartyData.dataRetrieve.value(dataRetrieve.id).hasFailed shouldBe Some(true)
      }
    }
  }

  trait TestFixture {
    implicit val hc: HeaderCarrier = HeaderCarrier()

    val mockBankAccountReputationConnector: BankAccountReputationConnector[Future] =
      mock[BankAccountReputationConnector[Future]]

    val resetMins: Int = 60
    val dummyResponse = DataRetrieve.Response.Object(
      Map(
        DataRetrieve.Attribute("accountNumberIsWellFormatted") -> "yes",
        DataRetrieve.Attribute("accountExists")                -> "inapplicable",
        DataRetrieve.Attribute("nameMatches")                  -> "indeterminate"
      )
    )

    def validateBankDetailsDataRetrieve(resetMinutes: Option[Int]): DataRetrieve = DataRetrieve(
      DataRetrieve.Type("validateBankDetails"),
      DataRetrieveId("someId"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("isValid"),
            ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeBankName"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeBankName")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
            ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit")))
          ),
          AttributeInstruction(DataRetrieve.Attribute("iban"), ConstructAttribute.AsIs(Fetch(List("iban"))))
        )
      ),
      Map.empty[DataRetrieve.Attribute, DataRetrieve.AttrType],
      List.empty[DataRetrieve.ParamExpr],
      None,
      resetMinutes.map(_ => 3),
      resetMinutes,
      false,
      None,
      UrlDescriptor(
        urlPath = "/bank-details/validate",
        destination = UrlDestination.MDTP
      ),
      None,
      manualFallbackIsDesigned = None
    )

    def mkRequest(count: Option[Int], resetTime: Option[LocalDateTime]) = DataRetrieve.Request(
      Json.obj(),
      List.empty[(String, String)],
      count,
      resetTime,
      None
    )

    def mockFailureResponse() =
      mockValidateResponseWithFailureProcessing(true)

    def mockSuccessResponse() =
      mockValidateResponseWithFailureProcessing(false)

    def mockValidateResponseWithFailureProcessing(isFailure: Boolean) = {
      mockValidateOnlyResponse()

      when(
        mockBankAccountReputationConnector.isFailure(any[DataRetrieve.Response])
      ).thenReturn(isFailure)
    }

    def mockValidateOnlyResponse() =
      when(
        mockBankAccountReputationConnector.validateBankDetails(any[DataRetrieve], any[DataRetrieve.Request])(
          any[HeaderCarrier]
        )
      ).thenReturn(Future.successful(ServiceResponse(dummyResponse)))

    def mockUpstreamResponse(response: ServiceCallResponse[DataRetrieve.Response]) =
      when(
        mockBankAccountReputationConnector.validateBankDetails(any[DataRetrieve], any[DataRetrieve.Request])(
          any[HeaderCarrier]
        )
      ).thenReturn(Future.successful(response))

    def callDataRetrieveServiceWith(dataRetrieve: DataRetrieve, request: DataRetrieve.Request) =
      DataRetrieveService.retrieveDataResult(
        dataRetrieve,
        None,
        request,
        Some(mockBankAccountReputationConnector),
        None,
        None,
        None,
        None,
        None,
        None
      )
  }
}
