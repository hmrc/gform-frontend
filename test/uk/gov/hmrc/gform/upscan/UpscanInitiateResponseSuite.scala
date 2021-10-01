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

import munit.FunSuite
import play.api.libs.json.{ JsPath, JsResult, JsSuccess, JsValue, Json, PathNode }

class UpscanInitiateResponseSuite extends FunSuite {

  test("Deserializing of UpscanInitiateResponse") {
    val payload =
      """|{
         |  "reference": "4c2f64e2-e0b0-4290-ae3f-447dddf0ddfd",
         |  "uploadRequest": {
         |    "href": "http://localhost:9570/upscan/upload-proxy",
         |    "fields": {
         |      "x-amz-meta-callback-url": "https://myservice.com/callback",
         |      "x-amz-date": "20210918T151154Z",
         |      "success_action_redirect": "https://myservice.com/nextPage?key=4c2f64e2-e0b0-4290-ae3f-447dddf0ddfd",
         |      "x-amz-credential": "ASIAxxxxxxxxx/20180202/eu-west-2/s3/aws4_request",
         |      "x-amz-meta-upscan-initiate-response": "2021-09-18T15:11:54.127Z",
         |      "x-amz-meta-upscan-initiate-received": "2021-09-18T15:11:54.127Z",
         |      "x-amz-meta-request-id": "c2485367-7e0e-4605-bbaf-958988f8045d",
         |      "x-amz-meta-original-filename": "${filename}",
         |      "x-amz-algorithm": "AWS4-HMAC-SHA256",
         |      "key": "4c2f64e2-e0b0-4290-ae3f-447dddf0ddfd",
         |      "acl": "private",
         |      "x-amz-signature": "xxxx",
         |      "error_action_redirect": "https://myservice.com/errorPage",
         |      "x-amz-meta-session-id": "9d950635-55e3-4c02-9595-ccabc6a3a166",
         |      "x-amz-meta-consuming-service": "testing",
         |      "policy": "eyJjb25kaXRpb25zIjpbWyJjb250ZW50LWxlbmd0aC1yYW5nZSIsMCwxMDI0XV19"
         |    }
         |  }
         |}""".stripMargin

    val json: JsValue = Json.parse(payload)

    val expected = JsSuccess(
      UpscanInitiateResponse(
        UpscanReference("4c2f64e2-e0b0-4290-ae3f-447dddf0ddfd"),
        UploadRequest(
          "http://localhost:9570/upscan/upload-proxy",
          Map(
            "x-amz-meta-callback-url"             -> "https://myservice.com/callback",
            "x-amz-date"                          -> "20210918T151154Z",
            "success_action_redirect"             -> "https://myservice.com/nextPage?key=4c2f64e2-e0b0-4290-ae3f-447dddf0ddfd",
            "x-amz-credential"                    -> "ASIAxxxxxxxxx/20180202/eu-west-2/s3/aws4_request",
            "x-amz-meta-upscan-initiate-response" -> "2021-09-18T15:11:54.127Z",
            "x-amz-meta-upscan-initiate-received" -> "2021-09-18T15:11:54.127Z",
            "x-amz-meta-request-id"               -> "c2485367-7e0e-4605-bbaf-958988f8045d",
            "x-amz-meta-original-filename"        -> "${filename}",
            "x-amz-algorithm"                     -> "AWS4-HMAC-SHA256",
            "key"                                 -> "4c2f64e2-e0b0-4290-ae3f-447dddf0ddfd",
            "acl"                                 -> "private",
            "x-amz-signature"                     -> "xxxx",
            "error_action_redirect"               -> "https://myservice.com/errorPage",
            "x-amz-meta-session-id"               -> "9d950635-55e3-4c02-9595-ccabc6a3a166",
            "x-amz-meta-consuming-service"        -> "testing",
            "policy"                              -> "eyJjb25kaXRpb25zIjpbWyJjb250ZW50LWxlbmd0aC1yYW5nZSIsMCwxMDI0XV19"
          )
        )
      ),
      JsPath(List.empty[PathNode])
    )

    val response: JsResult[UpscanInitiateResponse] = json.validate[UpscanInitiateResponse]

    assertEquals(response, expected)

  }

}
