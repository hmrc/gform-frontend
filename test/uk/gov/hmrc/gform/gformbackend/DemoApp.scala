/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gformbackend

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.libs.ws.WSRequest
import play.api.libs.ws.ahc.AhcWSClient
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import uk.gov.hmrc.http.HeaderCarrier

object DemoApp extends App {

  val gformConnector = new GformConnector(TestWSHttp, "http://localhost:9196/gform")

  val formTemplateId = FormTemplateId("AAA999")
  val userId = UserId("TestUser")

  val result = for {
    formId <- gformConnector.newForm(formTemplateId, userId, None)
    form   <- gformConnector.getForm(formId)
    _ = println(s"formId = $formId")
    _ = println(s"form = $form")
  } yield ()

  Await.result(result, Duration.Inf)
  implicit lazy val hc: HeaderCarrier = new HeaderCarrier()
}

//If you want to use WSHttp outside play app you must provide your WSClient. Otherwise it blows up.
//See https://github.com/hmrc/http-verbs/issues/60
//Don't use it on production ('ws.close()' logic is missing)
object TestWSHttp extends WSHttp {
  override def buildRequest[A](url: String)(implicit hc: HeaderCarrier): WSRequest = ws.url(url)
  private implicit lazy val s: ActorSystem = ActorSystem()
  private implicit lazy val mat: ActorMaterializer = ActorMaterializer()
  private lazy val ws = AhcWSClient()(mat)
}
