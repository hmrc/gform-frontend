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
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.{ Duration, MILLISECONDS }
import scala.concurrent.{ Await, Future }

class DataRetrieveServiceSpec extends Spec with IdiomaticMockito {

  "FailureHandling" should "return first time failure request with 1 failure count" in new TestFixture {
    mockFailureResponse()

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), mkRequest(None, None))

    val maybeRes: Option[DataRetrieveResult] = Await.result[Option[DataRetrieveResult]](resF, defaultWait)
    val newResetTime: LocalDateTime =
      LocalDateTime.now().truncatedTo(ChronoUnit.MINUTES).plusMinutes(resetMins.toLong + 1L)

    maybeRes.fold(assert(false)) { result =>
      result.failureCountResetTime shouldBe Some(newResetTime)
      result.failureCount shouldBe Some(1)
    }
  }

  it should "return Xth time failure request with incremented failure count" in new TestFixture {
    mockFailureResponse()

    val futureTime = LocalDateTime.now.plusMinutes(10).truncatedTo(ChronoUnit.MINUTES)
    val request = mkRequest(Some(2), Some(futureTime))

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), request)

    val maybeRes: Option[DataRetrieveResult] = Await.result[Option[DataRetrieveResult]](resF, defaultWait)

    maybeRes.fold(assert(false)) { result =>
      result.failureCountResetTime shouldBe request.failureResetTime
      result.failureCount shouldBe request.previousFailureCount.map(_ + 1)
    }
  }

  it should "return Xth time failure request with reset failure count and timeout if past previous block time" in new TestFixture {
    mockFailureResponse()

    val futureTime = LocalDateTime.now.minusMinutes(10).truncatedTo(ChronoUnit.MINUTES)
    val request = mkRequest(Some(2), Some(futureTime))

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), request)

    val maybeRes: Option[DataRetrieveResult] = Await.result[Option[DataRetrieveResult]](resF, defaultWait)
    val newResetTime: LocalDateTime =
      LocalDateTime.now().truncatedTo(ChronoUnit.MINUTES).plusMinutes(resetMins.toLong + 1L)

    maybeRes.fold(assert(false)) { result =>
      result.failureCountResetTime shouldBe Some(newResetTime)
      result.failureCount shouldBe Some(1)
    }
  }

  it should "return Xth time success request with reset failure count and timeout if past previous block time" in new TestFixture {
    mockSuccessResponse()

    val futureTime = LocalDateTime.now.minusMinutes(10).truncatedTo(ChronoUnit.MINUTES)
    val request = mkRequest(Some(2), Some(futureTime))

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), request)

    val maybeRes: Option[DataRetrieveResult] = Await.result[Option[DataRetrieveResult]](resF, defaultWait)

    maybeRes.fold(assert(false)) { result =>
      result.failureCountResetTime shouldBe None
      result.failureCount shouldBe Some(0)
    }
  }

  it should "return first time success request with no failure count or reset time" in new TestFixture {
    mockSuccessResponse()

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(Some(resetMins)), mkRequest(None, None))

    val maybeRes: Option[DataRetrieveResult] = Await.result[Option[DataRetrieveResult]](resF, defaultWait)

    maybeRes.fold(assert(false)) { result =>
      result.failureCountResetTime shouldBe None
      result.failureCount shouldBe None
    }
  }

  it should "not execute failure processing if reset minutes not defined on dataRetrieve" in new TestFixture {
    mockValidateOnlyResponse()

    val resF: Future[Option[DataRetrieveResult]] =
      callDataRetrieveServiceWith(validateBankDetailsDataRetrieve(None), mkRequest(None, None))

    val maybeRes: Option[DataRetrieveResult] = Await.result[Option[DataRetrieveResult]](resF, defaultWait)

    verify(mockBankAccountReputationConnector, times(1))
      .validateBankDetails(any[DataRetrieve], any[DataRetrieve.Request])(any[HeaderCarrier])

    verify(mockBankAccountReputationConnector, times(0))
      .isFailure(any[DataRetrieve.Response])

    maybeRes.fold(assert(false)) { result =>
      result.failureCountResetTime shouldBe None
      result.failureCount shouldBe None
    }
  }

  trait TestFixture {
    implicit val hc: HeaderCarrier = HeaderCarrier()

    val mockBankAccountReputationConnector: BankAccountReputationConnector[Future] =
      mock[BankAccountReputationConnector[Future]]

    val resetMins: Int = 60
    val defaultWait = Duration(1000, MILLISECONDS)
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
      resetMinutes
    )

    def mkRequest(count: Option[Int], resetTime: Option[LocalDateTime]) = DataRetrieve.Request(
      Json.obj(),
      List.empty[(String, String)],
      count,
      resetTime
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
