/*
 * Copyright 2016 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.controllers

import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.HttpGet
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.bforms.controllers.auth.{TestBFormsAuth, TestUsers}

import scala.concurrent.ExecutionContext


class LandfillTaxControllerSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with TestUsers with CSRFTest {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]

  val fakeRequest = addToken(FakeRequest("GET", "/landfill-tax"))

  "GET /landfill-tax" should {
    "return 200" in {
      val controller = landfillTaxController(agentUser)

      val result = controller.landfillTaxDisplay("")(fakeRequest).futureValue
      status(result) shouldBe Status.OK
    }

    "return HTML" in {
      val controller = landfillTaxController(agentUser)

      val result = controller.landfillTaxDisplay("YZAL123")(fakeRequest)
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      contentAsString(result) should include("landfill tax")
      contentAsString(result) should include("YZAL123")
    }
  }

  def landfillTaxController(user: AuthContext)(implicit messagesApi: MessagesApi) = {
    new LandfillTax(messagesApi) with TestBFormsAuth {

      override lazy val authConnector: AuthConnector = new AuthConnector {
        def http: HttpGet = ???

        val serviceUrl: String = "test-service-url"

      }

      def authContext: AuthContext = user
    }
  }


}
