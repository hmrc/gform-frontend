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

package uk.gov.hmrc.gform.repo

import cats.data.EitherT
import cats.implicits._
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.{ Filters, FindOneAndReplaceOptions, IndexModel }
import org.mongodb.scala.model.Filters._
import play.api.libs.json._
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import scala.concurrent.{ ExecutionContext, Future }

class Repo[T: OWrites: Manifest](
  name: String,
  mongoComponent: MongoComponent,
  idLens: T => String,
  indexes: Seq[IndexModel] = Seq.empty,
  replaceIndexes: Boolean = false
)(implicit
  formatT: Format[T],
  ec: ExecutionContext
) extends PlayMongoRepository[T](mongoComponent, name, formatT, indexes, None, replaceIndexes) {
  underlying =>
  def find(id: String): Future[Option[T]] =
    underlying.collection
      .find(equal("_id", id))
      .first()
      .toFutureOption()

  def upsert(t: T): FOpt[Unit] = EitherT {
    underlying.collection
      .findOneAndReplace(idSelector(t), t, FindOneAndReplaceOptions().upsert(true))
      .toFuture()
      .asEither
  }

  def delete(id: String): FOpt[DeleteResult] = EitherT {
    underlying.collection
      .deleteOne(Filters.equal("_id", id))
      .toFuture()
      .map(wr => DeleteResult(id, wr.getDeletedCount === 1).asRight)
      .recover { case lastError =>
        UnexpectedState(lastError.getMessage).asLeft
      }
  }

  private def idSelector(item: T): Bson = Filters.equal("_id", idLens(item))

  implicit class FutureWriteResultOps[R](t: Future[R]) {
    def asEither: Future[Either[UnexpectedState, Unit]] =
      t.map { _ =>
        ().asRight[UnexpectedState]
      } recover { case lastError =>
        UnexpectedState(lastError.getMessage).asLeft[Unit]
      }
  }
}
