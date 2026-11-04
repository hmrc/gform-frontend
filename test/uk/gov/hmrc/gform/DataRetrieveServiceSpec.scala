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
import scala.concurrent.Future

class DataRetrieveServiceSpec extends Spec with IdiomaticMockito {

  "FailureHandling" should "return first time failure request with 1 failure count" in new TestFixture {
    mockFailureResponse()

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), mkRequest(None, None))

    whenReady(resF) { maybeRes =>
      val newResetTime: LocalDateTime =
        LocalDateTime.now().truncatedTo(ChronoUnit.MINUTES).plusMinutes(resetMins.toLong + 1L)

      maybeRes.fold(assert(false)) { result =>
        result.failureCountResetTime shouldBe Some(newResetTime)
        result.failureCount shouldBe Some(1)
      }
    }
  }

  it should "return Xth time failure request with incremented failure count" in new TestFixture {
    mockFailureResponse()

    val futureTime = LocalDateTime.now.plusMinutes(10).truncatedTo(ChronoUnit.MINUTES)
    val request = mkRequest(Some(2), Some(futureTime))

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), request)

    whenReady(resF) { maybeRes =>
      maybeRes.fold(assert(false)) { result =>
        result.failureCountResetTime shouldBe request.failureResetTime
        result.failureCount shouldBe request.previousFailureCount.map(_ + 1)
      }
    }
  }

  it should "return Xth time failure request with reset failure count and timeout if past previous block time" in new TestFixture {
    mockFailureResponse()

    val pastTime = LocalDateTime.now.minusMinutes(10).truncatedTo(ChronoUnit.MINUTES)
    val request = mkRequest(Some(2), Some(pastTime))

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), request)

    whenReady(resF) { maybeRes =>
      val newResetTime: LocalDateTime =
        LocalDateTime.now().truncatedTo(ChronoUnit.MINUTES).plusMinutes(resetMins.toLong + 1L)

      maybeRes.fold(assert(false)) { result =>
        result.failureCountResetTime shouldBe Some(newResetTime)
        result.failureCount shouldBe Some(1)
      }
    }
  }

  it should "return Xth time success request with reset failure count and timeout if past previous block time" in new TestFixture {
    mockSuccessResponse()

    val pastTime = LocalDateTime.now.minusMinutes(10).truncatedTo(ChronoUnit.MINUTES)
    val request = mkRequest(Some(2), Some(pastTime))

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), request)

    whenReady(resF) { maybeRes =>
      maybeRes.fold(assert(false)) { result =>
        result.failureCountResetTime shouldBe None
        result.failureCount shouldBe Some(0)
      }
    }
  }

  it should "return first time success request with no failure count or reset time" in new TestFixture {
    mockSuccessResponse()

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), mkRequest(None, None))

    whenReady(resF) { maybeRes =>
      maybeRes.fold(assert(false)) { result =>
        result.failureCountResetTime shouldBe None
        result.failureCount shouldBe None
      }
    }
  }

  it should "not execute failure processing if reset minutes not defined on dataRetrieve" in new TestFixture {
    mockValidateOnlyResponse()

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(None), mkRequest(None, None))

    whenReady(resF) { maybeRes =>
      verify(mockBankAccountReputationConnector, times(1))
        .validateBankDetails(any[DataRetrieve], any[DataRetrieve.Request])(any[HeaderCarrier])

      verify(mockBankAccountReputationConnector, times(0))
        .isFailure(any[DataRetrieve.Response])

      maybeRes.fold(assert(false)) { result =>
        result.failureCountResetTime shouldBe None
        result.failureCount shouldBe None
      }
    }
  }

  "Manual fallback" should "default to disabled for existing retrieval definitions and results" in new TestFixture {
    val dataRetrieve = validateBankDetailsDataRetrieve(None)
    val oldDefinition = Json.toJson(dataRetrieve).as[play.api.libs.json.JsObject] - "manualFallbackIsDesigned"
    oldDefinition.as[DataRetrieve].manualFallbackIsDesigned shouldBe false
    dataRetrieve.manualFallbackIsDesigned shouldBe false

    val result = DataRetrieveResult(dataRetrieve.id, dummyResponse.toRetrieveDataType(), Json.obj(), None, None, None)
    val oldResult = Json.toJson(result).as[play.api.libs.json.JsObject] - "hasFailed"
    oldResult.as[DataRetrieveResult].hasFailed shouldBe false
    Json.toJson(result.copy(hasFailed = true)).as[DataRetrieveResult].hasFailed shouldBe true
    (oldDefinition + ("manualFallbackIsDesigned" -> Json.toJson("true"))).validate[DataRetrieve].isError shouldBe true
    (oldResult + ("hasFailed" -> Json.toJson("true"))).validate[DataRetrieveResult].isError shouldBe true
  }

  it should "fail the journey on an upstream error unless explicitly enabled" in new TestFixture {
    mockUpstreamResponse(CannotRetrieveResponse)
    val result = callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(None), mkRequest(None, None))
    whenReady(result.failed)(_.getMessage shouldBe "Cannot retrieve data")
  }

  it should "fail the journey on a not-found response by default" in new TestFixture {
    mockUpstreamResponse(NotFound)
    val result = callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(None), mkRequest(None, None))
    whenReady(result.failed)(_.getMessage shouldBe "Cannot retrieve data")
  }

  it should "persist a failed object result with empty attributes when enabled" in new TestFixture {
    mockUpstreamResponse(CannotRetrieveResponse)
    val dataRetrieve = validateBankDetailsDataRetrieve(None).copy(manualFallbackIsDesigned = true)
    whenReady(callDataRetrieveServiceWith(dataRetrieve, mkRequest(None, None))) { result =>
      result.value.hasFailed shouldBe true
      val instructions = dataRetrieve.attributes match {
        case Attr.FromObject(inst) => inst
        case _                     => fail("Expected object attributes")
      }
      result.value.data shouldBe RetrieveDataType.ObjectType(instructions.map(_.attribute -> "").toMap)
      DataRetrieveEval.getDataRetrieveAttribute(
        Map(dataRetrieve.id -> result.value),
        DataRetrieveCtx(dataRetrieve.id, DataRetrieve.hasFailedAttribute)
      ) shouldBe Some(List("true"))
    }
  }

  it should "expose failure independently of an empty list payload" in new TestFixture {
    mockUpstreamResponse(NotFound)
    val dataRetrieve = validateBankDetailsDataRetrieve(None)
      .copy(manualFallbackIsDesigned = true, attributes = Attr.FromArray(List.empty))
    whenReady(callDataRetrieveServiceWith(dataRetrieve, mkRequest(None, None))) { result =>
      result.value.hasFailed shouldBe true
      result.value.data shouldBe RetrieveDataType.ListType(List.empty)
      DataRetrieveEval.getDataRetrieveAttribute(
        Map(dataRetrieve.id -> result.value),
        DataRetrieveCtx(dataRetrieve.id, DataRetrieve.hasFailedAttribute)
      ) shouldBe Some(List("true"))
    }
  }

  it should "keep successful empty responses on the normal path" in new TestFixture {
    mockUpstreamResponse(ServiceResponse(DataRetrieve.Response.Array(List.empty)))
    val dataRetrieve = validateBankDetailsDataRetrieve(None)
      .copy(manualFallbackIsDesigned = true, attributes = Attr.FromArray(List.empty))
    whenReady(callDataRetrieveServiceWith(dataRetrieve, mkRequest(None, None))) { result =>
      result.value.hasFailed shouldBe false
      DataRetrieveEval.getDataRetrieveAttribute(
        Map(dataRetrieve.id -> result.value),
        DataRetrieveCtx(dataRetrieve.id, DataRetrieve.hasFailedAttribute)
      ) shouldBe Some(List("false"))
    }
  }

  it should "not retry after recovery, changed parameters or callOnNoChange when fallback is saved" in new TestFixture {
    mockSuccessResponse()
    val dataRetrieve = validateBankDetailsDataRetrieve(None)
      .copy(manualFallbackIsDesigned = true, callOnNoChange = true, id = DataRetrieveId("1_bankDetails"))
    val failure = DataRetrieveResult(
      dataRetrieve.id,
      RetrieveDataType.ObjectType(Map.empty),
      Json.obj("accountNumber" -> "old"),
      None,
      None,
      None,
      hasFailed = true
    )
    val form = Form.dummy(FormTemplateId("4290"))
    val savedForm = form.copy(thirdPartyData = form.thirdPartyData.updateDataRetrieve(List(failure)))
    val resumedForm = Json.toJson(savedForm).as[Form]
    val request = mkRequest(None, None).copy(params = List("accountNumber" -> "changed"))
    whenReady(callDataRetrieveServiceWith(dataRetrieve, request, Some(resumedForm))) { result =>
      result shouldBe None
      verify(mockBankAccountReputationConnector, times(0))
        .validateBankDetails(any[DataRetrieve], any[DataRetrieve.Request])(any[HeaderCarrier])
      resumedForm.thirdPartyData.dataRetrieve.value(dataRetrieve.id).hasFailed shouldBe true
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
      None
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

    def callDataRetrieveServiceWith(
      dataRetrieve: DataRetrieve,
      request: DataRetrieve.Request,
      form: Option[Form] = None
    ) =
      DataRetrieveService.retrieveDataResult(
        dataRetrieve,
        form,
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
