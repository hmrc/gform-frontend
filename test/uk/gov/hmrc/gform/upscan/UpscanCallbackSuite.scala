/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.upscan

import java.time.Instant
import munit.FunSuite
import play.api.libs.json.{ JsPath, JsResult, JsSuccess, JsValue, Json, PathNode }

class UpscanCallbackSuite extends FunSuite {

  test("Deserializing of UpscanCallback.Success") {
    val payload =
      """|{
         |  "reference": "ad22887f-d670-4766-beff-1c30d8200156",
         |  "downloadUrl": "http://localhost:9570/upscan/download/5544a613-316b-41e9-9e16-6d72ddfea2f5",
         |  "fileStatus": "READY",
         |  "uploadDetails": {
         |    "uploadTimestamp": "2021-09-20T20:37:07.606Z",
         |    "checksum": "82d95d433e8c37dcb755846e7d0947690922ea069e89c3bc387794249047457f",
         |    "fileMimeType": "application/pdf",
         |    "fileName": "a_hello_world_3.pdf",
         |    "size": 9053
         |  }
         |}""".stripMargin

    val json: JsValue = Json.parse(payload)

    val expected = JsSuccess(
      UpscanCallback.Success(
        UpscanReference("ad22887f-d670-4766-beff-1c30d8200156"),
        "http://localhost:9570/upscan/download/5544a613-316b-41e9-9e16-6d72ddfea2f5",
        UpscanFileStatus.Ready,
        UploadDetails(
          Instant.parse("2021-09-20T20:37:07.606Z"),
          "82d95d433e8c37dcb755846e7d0947690922ea069e89c3bc387794249047457f",
          "application/pdf",
          "a_hello_world_3.pdf",
          9053L
        )
      ),
      JsPath(List.empty[PathNode])
    )

    val response: JsResult[UpscanCallback] = json.validate[UpscanCallback]

    assertEquals(response, expected)

  }

  test("Deserializing of UpscanCallback.Failure") {
    val payload =
      """|{
         |  "reference": "772afbd6-f208-4a65-b57e-95a23bb68d02",
         |  "fileStatus": "FAILED",
         |  "failureDetails": {
         |    "failureReason": "QUARANTINE",
         |    "message": "Eicar-Test-Signature"
         |  }
         |}""".stripMargin

    val json: JsValue = Json.parse(payload)

    val expected = JsSuccess(
      UpscanCallback.Failure(
        UpscanReference("772afbd6-f208-4a65-b57e-95a23bb68d02"),
        UpscanFileStatus.Failed,
        FailureDetails(
          "QUARANTINE",
          "Eicar-Test-Signature"
        )
      ),
      JsPath(List.empty[PathNode])
    )

    val response: JsResult[UpscanCallback] = json.validate[UpscanCallback]

    assertEquals(response, expected)

  }
}
