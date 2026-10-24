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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import play.api.libs.json.{ JsNull, Json }
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkPage => buildPage }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentValidatorGen

class FormComponentValidatorSpec extends Spec {
  "FormComponentValidator" should "round trip derived JSON" in {
    forAll(FormComponentValidatorGen.formComponentValidatorGen) { obj =>
      verifyRoundTrip(obj)
    }
  }

  "PageValidator" should "round trip field IDs as JSON strings" in {
    val validator = PageValidator(
      ValidIf(Equals(FormCtx(FormComponentId("first")), FormCtx(FormComponentId("second")))),
      NonEmptyList.of(FormComponentId("first"), FormComponentId("second")),
      toSmartString("The values must match")
    )

    (Json.toJson(validator) \ "fieldIds").as[List[String]] shouldBe List("first", "second")
    verifyRoundTrip(validator)
  }

  it should "reject invalid field IDs when reading JSON" in {
    val validator =
      PageValidator(ValidIf(IsTrue), NonEmptyList.one(FormComponentId("first")), toSmartString("The values must match"))
    val json = Json.toJsObject(validator) ++ Json.obj("fieldIds" -> List("invalid-id"))

    json.validate[PageValidator].isError shouldBe true
    (Json.toJsObject(validator) ++ Json.obj("fieldIds" -> Json.arr())).validate[PageValidator].isError shouldBe true
  }

  "Page" should "round trip a non-empty validators list" in {
    val validator = PageValidator(
      ValidIf(IsTrue),
      NonEmptyList.one(FormComponentId("first")),
      toSmartString("The values must match")
    )
    val page = buildPage(Nil).copy(
      validators = Some(NonEmptyList.one(validator))
    )

    verifyRoundTrip(page)
  }

  it should "read missing or null validators as None" in {
    val page = buildPage(Nil)
    val json = Json.toJsObject(page)

    (json - "validators").as[Page] shouldBe page
    (json ++ Json.obj("validators" -> JsNull)).as[Page] shouldBe page
    json.as[Page] shouldBe page
  }

  it should "reject malformed validators instead of treating them as an empty list" in {
    val json = Json.toJsObject(buildPage(Nil))

    (json ++ Json.obj("validators" -> "invalid")).validate[Page].isError shouldBe true
    (json ++ Json.obj("validators" -> Json.arr(Json.obj()))).validate[Page].isError shouldBe true
    (json ++ Json.obj("validators" -> Json.arr())).validate[Page].isError shouldBe true
  }
}
