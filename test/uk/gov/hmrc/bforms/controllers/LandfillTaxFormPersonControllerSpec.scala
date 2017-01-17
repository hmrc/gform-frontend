/*
 * Copyright 2017 HM Revenue & Customs
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

import helpers.Mongo
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.i18n.MessagesApi
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.bforms.models.LandfillTaxDetailsPerson
import uk.gov.hmrc.bforms.models.persistence.LandfillTaxDetailsPersonPersistence
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.{ExecutionContext, Future}


class LandfillTaxFormPersonControllerSpec extends UnitSpec with ScalaFutures with OneAppPerSuite with CSRFTest {

  implicit val ec = app.injector.instanceOf[ExecutionContext]
  implicit val messagesApi = app.injector.instanceOf[MessagesApi]

  val fakeRequest = addToken(FakeRequest("GET", "/landfill-tax-form-person"))

  "GET /landfill-tax-form-person" should {
    "return 200" in {
      val controller = landfillTaxFormPersonController

      val result = controller.landfillTaxFormPersonDisplay("")(fakeRequest).futureValue
      status(result) shouldBe Status.OK
    }

    "return HTML" in {
      val controller = landfillTaxFormPersonController

      val result = controller.landfillTaxFormPersonDisplay("YZAL123")(fakeRequest)
      contentType(result) shouldBe Some("text/html")
      charset(result) shouldBe Some("utf-8")
      contentAsString(result) should include("landfill tax")
      contentAsString(result) should include("YZAL123")
    }
  }

  def landfillTaxFormPersonController(implicit messagesApi: MessagesApi) = {
    implicit val db = Mongo.stubDb

    new LandfillTaxFormPerson(messagesApi)
  }


}
