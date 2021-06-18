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

package uk.gov.hmrc.gform.wshttp

import akka.actor.ActorSystem
import com.typesafe.config.{ Config, ConfigFactory }
import play.api.libs.json.Writes
import play.api.libs.ws.WSClient
import uk.gov.hmrc.http.hooks.HttpHook

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HttpResponse

/** Stubbed WSHttp which responses always with the same HttpResponse. Use it for test purposes
  */
class StubbedWSHttp(response: HttpResponse) extends WSHttp {
  override def doGet(url: String, headers: Seq[(String, String)] = Seq.empty)(implicit
    ec: ExecutionContext
  ): Future[HttpResponse] = Future.successful(response)
  override def doPost[A](url: String, body: A, headers: Seq[(String, String)])(implicit
    rds: Writes[A],
    ec: ExecutionContext
  ) = Future.successful(response)
  override def doFormPost(url: String, body: Map[String, Seq[String]], headers: Seq[(String, String)] = Seq.empty)(
    implicit ec: ExecutionContext
  ) =
    Future.successful(response)
  override def doPostString(url: String, body: String, headers: Seq[(String, String)] = Seq.empty)(implicit
    ec: ExecutionContext
  ) =
    Future.successful(response)
  override def doEmptyPost[A](url: String, headers: Seq[(String, String)] = Seq.empty)(implicit ec: ExecutionContext) =
    Future.successful(response)
  //TODO: PUT, PATCH, DELETE

  override val hooks: Seq[HttpHook] = Seq.empty

  override protected def actorSystem: ActorSystem = null

  override protected def configuration: Config = ConfigFactory.parseString("""
                                                                             |internalServiceHostPatterns = []
                                                                             |bootstrap.http.headersAllowlist = []
                                                                             |http-verbs.retries.intervals = []
                                                                             |""".stripMargin)

  override val wsClient: WSClient = null
}
