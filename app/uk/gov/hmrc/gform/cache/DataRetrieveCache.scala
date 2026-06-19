/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.cache

import cats.implicits.catsSyntaxEq
import com.github.benmanes.caffeine.cache.AsyncCache
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieveDescription, UrlDestination }
import uk.gov.hmrc.http.HeaderCarrier

import scala.compat.java8.FutureConverters.FutureOps
import scala.concurrent.{ ExecutionContext, Future }

class DataRetrieveCache(
  cache: AsyncCache[String, DataRetrieveDescription],
  connector: => GformConnector
)(implicit
  ec: ExecutionContext
) {
  implicit val hc: HeaderCarrier = HeaderCarrier()
  def get(dataRetrieveType: String)(implicit hc: HeaderCarrier): Future[DataRetrieveDescription] = {
    val completableFuture = cache.getIfPresent(dataRetrieveType)
    if (completableFuture != null) {
      Future.successful(completableFuture.get())
    } else {
      println(s"CM: cache miss for key: $dataRetrieveType, populating cache")
      populateCache().map { _ =>
        val completableFuture = cache.getIfPresent(dataRetrieveType)
        if (completableFuture != null) {
          completableFuture.get()
        } else {
          throw new NoSuchElementException(s"DataRetrieveDescription not found for key: $dataRetrieveType")
        }
      }
    }
  }

  def getUrlFragment(drType: String, urlDestination: UrlDestination): Future[String] = {
    println(s"CM: trying to get url fragment for type: $drType and destination: $urlDestination")
    get(drType)
      .map { desc =>
        println(s"CM: found $desc")
        desc.urlDescriptors
          .find(_.destination === urlDestination)
          .map { d =>
            println(s"CM: got $d")
            d.urlPath
          }
          .getOrElse {
            throw new NoSuchElementException(
              s"UrlDescriptor not found for type: $drType and destination: $urlDestination"
            )
          }
      }
  }

  private def populateCache()(implicit hc: HeaderCarrier): Future[Unit] =
    connector
      .getDataRetrieveDefinitions()
      .map { descriptions =>
        descriptions.foreach { description =>
          println(s"CM: adding ${description.tpe} to cache")
          cache.put(description.tpe, Future.successful(description).toJava.toCompletableFuture)
        }
      }
      .map(_ => ())
}
