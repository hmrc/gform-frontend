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
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataSource.Mongo
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, DataRetrieve, DataRetrieveId, DataRetrieveResult, NotChecked, RetrieveDataType }

class ThirdPartyDataSuite extends FunSuite {

  test("Roundtrip of RetrieveDataType.ObjectType") {

    val thirdPartyData = ThirdPartyData(
      obligations = NotChecked,
      emailVerification = Map.empty[EmailFieldId, EmailAndCode],
      queryParams = QueryParams.empty,
      reviewData = None,
      booleanExprCache = BooleanExprCache.empty,
      dataRetrieve = Some(Map(DataRetrieveId("bankDetails") -> getDrResultWithIndex(id = "bankDetails"))),
      postcodeLookup = None,
      selectedAddresses = None,
      enteredAddresses = None,
      confirmedAddresses = None,
      itmpRetrievals = None,
      confirmations = None
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
            DataRetrieve.Attribute("employerName") -> "John"
          ),
          Map(
            DataRetrieve.Attribute("employerName") -> "Doe"
          )
        )
      ),
      requestParams = Json.obj("nino" -> "CC111111C")
    )

    val thirdPartyData = ThirdPartyData(
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
      itmpRetrievals = None,
      confirmations = Some(
        Map(
          FormComponentId("fcId1") -> List("1", "2"),
          FormComponentId("fcId2") -> List("3", "4")
        )
      )
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
         |  },
         |  "confirmations": {
         |    "fcId1": [
         |      "1",
         |      "2"
         |    ],
         |    "fcId2": [
         |      "3",
         |      "4"
         |    ]
         |  }
         |}""".stripMargin

    val asJson = Json.toJson(thirdPartyData)
    val asCaseClass = Json.parse(expected).as[ThirdPartyData]

    assertEquals(asJson, Json.parse(expected))
    assertEquals(asCaseClass, thirdPartyData)
  }

  test("removeDataRetrieve ATL data and re-key whilst ignoring other data retrieves") {
    var thirdPartyData = ThirdPartyData(
      obligations = NotChecked,
      emailVerification = Map.empty[EmailFieldId, EmailAndCode],
      queryParams = QueryParams.empty,
      reviewData = None,
      booleanExprCache = BooleanExprCache.empty,
      dataRetrieve = Some(
        Map(
          //Non-ATL
          DataRetrieveId("bankDetails") -> getDrResultWithIndex(id = "bankDetails"),
          //ATL 1
          DataRetrieveId("1_bankDetails")      -> getDrResultWithIndex(Option(1), "bankDetails"),
          DataRetrieveId("2_bankDetails")      -> getDrResultWithIndex(Option(2), "bankDetails"),
          DataRetrieveId("1_bankDetailsOther") -> getDrResultWithIndex(Option(1), "bankDetailsOther"),
          DataRetrieveId("2_bankDetailsOther") -> getDrResultWithIndex(Option(2), "bankDetailsOther"),
          //ATL 2
          DataRetrieveId("1_bankDetails2") -> getDrResultWithIndex(Option(1), "bankDetails2"),
          DataRetrieveId("2_bankDetails2") -> getDrResultWithIndex(Option(2), "bankDetails2")
        )
      ),
      postcodeLookup = None,
      selectedAddresses = None,
      enteredAddresses = None,
      confirmedAddresses = None,
      itmpRetrievals = None,
      confirmations = None
    )

    thirdPartyData = thirdPartyData.removeDataRetrieveData(
      0,
      Set(DataRetrieveId("1_bankDetails"), DataRetrieveId("1_bankDetailsOther"))
    )
    assertEquals(thirdPartyData.dataRetrieve.get.size, 5)
    //Non-ATL
    assertEquals(thirdPartyData.dataRetrieve.get(DataRetrieveId("bankDetails")).id, DataRetrieveId("bankDetails"))
    //ATL 1
    assertEquals(thirdPartyData.dataRetrieve.get(DataRetrieveId("1_bankDetails")).id, DataRetrieveId("1_bankDetails"))
    assertEquals(
      thirdPartyData.dataRetrieve.get(DataRetrieveId("1_bankDetailsOther")).id,
      DataRetrieveId("1_bankDetailsOther")
    )
    //ATL 2
    assertEquals(thirdPartyData.dataRetrieve.get(DataRetrieveId("1_bankDetails2")).id, DataRetrieveId("1_bankDetails2"))
    assertEquals(thirdPartyData.dataRetrieve.get(DataRetrieveId("2_bankDetails2")).id, DataRetrieveId("2_bankDetails2"))
  }

  test("ThirdPartyData json serialise and de-serialise to same value") {
    val booleanExprCache = BooleanExprCache(
      Map.from(
        Seq(
          Mongo(CollectionName("name")) -> Map.from(Seq("value" -> true))
        )
      )
    )

    val oldThirdPartyData = ThirdPartyData(
      NotChecked,
      Map.empty,
      QueryParams.empty,
      None,
      booleanExprCache,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )

    val newThirdPartyData = Json
      .parse(
        Json.stringify(
          Json.toJson(oldThirdPartyData)
        )
      )
      .as[ThirdPartyData]

    assertEquals(oldThirdPartyData, newThirdPartyData)
  }

  def getDrResultWithIndex(idx: Option[Int] = None, id: String): DataRetrieveResult = {
    val dataRetrieveId = idx match {
      case Some(i) => DataRetrieveId(s"${i}_$id")
      case _       => DataRetrieveId(id)
    }
    DataRetrieveResult(
      id = dataRetrieveId,
      data = RetrieveDataType.ObjectType(
        Map(
          DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs") -> "no",
          DataRetrieve.Attribute("sortCodeSupportsDirectDebit")              -> "no",
          DataRetrieve.Attribute("isValid")                                  -> "yes",
          DataRetrieve.Attribute("sortCodeBankName")                         -> "BARCLAYS BANK UK PLC",
          DataRetrieve.Attribute("sortCodeSupportsDirectCredit")             -> "no",
          DataRetrieve.Attribute("sortCodeIsPresentOnEISCD")                 -> "yes",
          DataRetrieve.Attribute("iban")                                     -> "GB21BARC20670586473611"
        )
      ),
      requestParams = Json.obj("accountNumber" -> "86473611", "sortCode" -> "206705")
    )
  }
}
