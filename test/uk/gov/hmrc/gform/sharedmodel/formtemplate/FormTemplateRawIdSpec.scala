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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{ JsString, JsSuccess, Json }

class FormTemplateRawIdSpec extends AnyFlatSpecLike with Matchers {
  "FormTemplateRawId" should "serialise to and from json" in {
    Json.toJson(FormTemplateRawId("12")) shouldBe JsString("12")
    Json.fromJson[FormTemplateRawId](JsString("12")) shouldBe JsSuccess(FormTemplateRawId("12"))
  }
}
