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

package uk.gov.hmrc.bforms.views

import org.scalatestplus.play.OneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext


//class LandfillTaxViewSpec extends UnitSpec with OneAppPerSuite  {
//
//  implicit val ec = app.injector.instanceOf[ExecutionContext]
//  implicit val messagesApi = app.injector.instanceOf[MessagesApi]
//  implicit val lang = app.injector.instanceOf[Lang]
////  implicit val messs = app.injector.instanceOf[Messages]
//
//  val fakeRequest = FakeRequest("GET", "/landfill-tax")
//
//  "render index template" in {
//    val messages : Messages = new Messages(lang, messagesApi)
//    val html = uk.gov.hmrc.bforms.views.html.landfill_tax("YZAL123")(fakeRequest, messages)
//
//    contentAsString(html) should include ("YZAL123")
//  }
//
//}
