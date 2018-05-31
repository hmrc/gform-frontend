/*
 * Copyright 2018 HM Revenue & Customs
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

import org.scalatest.{ FlatSpec, Matchers }
import play.api.libs.json.{ JsString, JsSuccess, Json }

class FormTemplateId4GaSpec extends FlatSpec with Matchers {
  "FormTemplateId4Ga" should "serialise to and from json" in {
    Json.toJson(FormTemplateId4Ga("12")) shouldBe JsString("12")
    Json.fromJson[FormTemplateId4Ga](JsString("12")) shouldBe JsSuccess(FormTemplateId4Ga("12"))
  }
}
