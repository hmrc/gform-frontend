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

package uk.gov.hmrc.gform.sharedmodel.form

import munit.FunSuite
import play.api.libs.json.Json
import uk.gov.hmrc.gform.models.email.EmailFieldId
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, DataRetrieveAttribute, DataRetrieveId, DataRetrieveResult, NotChecked, RetrieveDataType }

class ThirdPartyDataSuite extends FunSuite {

  test("Roundtrip of RetrieveDataType.ObjectType") {

    val dataRetrieveId = DataRetrieveId("bankDetails")

    val dataRetrieveResult = DataRetrieveResult(
      id = dataRetrieveId,
      data = RetrieveDataType.ObjectType(
        Map(
          DataRetrieveAttribute.NonStandardAccountDetailsRequiredForBacs -> "no",
          DataRetrieveAttribute.SortCodeSupportsDirectDebit              -> "no",
          DataRetrieveAttribute.IsValid                                  -> "yes",
          DataRetrieveAttribute.SortCodeBankName                         -> "BARCLAYS BANK UK PLC",
          DataRetrieveAttribute.SortCodeSupportsDirectCredit             -> "no",
          DataRetrieveAttribute.SortCodeIsPresentOnEISCD                 -> "yes",
          DataRetrieveAttribute.Iban                                     -> "GB21BARC20670586473611"
        )
      ),
      requestParams = Json.obj("accountNumber" -> "86473611", "sortCode" -> "206705")
    )

    val thirdPartyData = ThirdPartyData(
      desRegistrationResponse = None,
      obligations = NotChecked,
      emailVerification = Map.empty[EmailFieldId, EmailAndCode],
      queryParams = QueryParams.empty,
      reviewData = None,
      booleanExprCache = BooleanExprCache.empty,
      dataRetrieve = Some(Map(dataRetrieveId -> dataRetrieveResult)),
      postcodeLookup = None,
      selectedAddresses = None,
      enteredAddresses = None,
      confirmedAddresses = None,
      itmpRetrievals = None
    )

    val expected =
      """|{
         |  "obligations": {
         |    "NotChecked": {}
         |  },
         |  "emailVerification": {},
         |  "queryParams": {
         |    "params": {}
         |  },
         |  "booleanExprCache": {
         |    "mapping": {}
         |  },
         |  "dataRetrieve": {
         |    "bankDetails": {
         |      "id": "bankDetails",
         |      "data": {
         |        "nonStandardAccountDetailsRequiredForBacs": "no",
         |        "sortCodeSupportsDirectDebit": "no",
         |        "isValid": "yes",
         |        "sortCodeBankName": "BARCLAYS BANK UK PLC",
         |        "sortCodeSupportsDirectCredit": "no",
         |        "sortCodeIsPresentOnEISCD": "yes",
         |        "iban": "GB21BARC20670586473611"
         |      },
         |      "requestParams": {
         |        "accountNumber": "86473611",
         |        "sortCode": "206705"
         |      }
         |    }
         |  }
         |}""".stripMargin

    val asJson = Json.toJson(thirdPartyData)
    val asCaseClass = Json.parse(expected).as[ThirdPartyData]

    assertEquals(asJson, Json.parse(expected))
    assertEquals(asCaseClass, thirdPartyData)

  }

  test("Roundtrip of RetrieveDataType.ListType") {
    val dataRetrieveId = DataRetrieveId("individualsEmployments")
    val dataRetrieveResult = DataRetrieveResult(
      id = dataRetrieveId,
      data = RetrieveDataType.ListType(
        List(
          Map(
            DataRetrieveAttribute.EmployerName -> "John"
          ),
          Map(
            DataRetrieveAttribute.EmployerName -> "Doe"
          )
        )
      ),
      requestParams = Json.obj("nino" -> "CC111111C")
    )

    val thirdPartyData = ThirdPartyData(
      desRegistrationResponse = None,
      obligations = NotChecked,
      emailVerification = Map.empty[EmailFieldId, EmailAndCode],
      queryParams = QueryParams.empty,
      reviewData = None,
      booleanExprCache = BooleanExprCache.empty,
      dataRetrieve = Some(Map(dataRetrieveId -> dataRetrieveResult)),
      postcodeLookup = None,
      selectedAddresses = None,
      enteredAddresses = None,
      confirmedAddresses = None,
      itmpRetrievals = None
    )
    val expected =
      """|{
         |  "obligations": {
         |    "NotChecked": {}
         |  },
         |  "emailVerification": {},
         |  "queryParams": {
         |    "params": {}
         |  },
         |  "booleanExprCache": {
         |    "mapping": {}
         |  },
         |  "dataRetrieve": {
         |    "individualsEmployments": {
         |      "id": "individualsEmployments",
         |      "data": [
         |        {
         |          "employerName": "John"
         |        },
         |        {
         |          "employerName": "Doe"
         |        }
         |      ],
         |      "requestParams": {
         |        "nino": "CC111111C"
         |      }
         |    }
         |  }
         |}""".stripMargin

    val asJson = Json.toJson(thirdPartyData)
    val asCaseClass = Json.parse(expected).as[ThirdPartyData]

    assertEquals(asJson, Json.parse(expected))
    assertEquals(asCaseClass, thirdPartyData)
  }
}
