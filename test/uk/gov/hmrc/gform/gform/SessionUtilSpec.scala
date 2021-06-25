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

package uk.gov.hmrc.gform.gform

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.typelevel.ci.CIString
import play.api.libs.json.{ Format, Json }
import play.api.test.FakeRequest
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import org.typelevel.ci._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils

class SessionUtilSpec extends AnyFlatSpecLike with Matchers with JsonUtils {

  case class SessionObject(value: CIString = ci"")
  object SessionObject {
    implicit val format: Format[SessionObject] = Json.format[SessionObject]
  }

  "jsonFromSession" should "parse value for given key as object" in {
    val request = FakeRequest().withSession("someKey" -> """{"value": "someValue"}""")
    jsonFromSession(request, "someKey", SessionObject()) shouldBe SessionObject(ci"someValue")
  }

  it should "default to defaultValue if JSON parsing fails" in {
    val request = FakeRequest().withSession("someKey" -> """{"value": "someValue"""")
    jsonFromSession(request, "someKey", SessionObject()) shouldBe SessionObject()
  }

  it should "default to defaultValue if JSON doesn't confirm to object format" in {
    val request = FakeRequest().withSession("someKey" -> """{}""")
    jsonFromSession(request, "someKey", SessionObject()) shouldBe SessionObject()
  }
}
