/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.eval

import cats.Id
import cats.syntax.applicative._
import scala.language.higherKinds
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.http.HeaderCarrier

class DbLookupChecker[F[_]](dbLookupStatus: (String, CollectionName, HeaderCarrier) => F[Boolean])
    extends Function3[String, CollectionName, HeaderCarrier, F[Boolean]] {
  def apply(value: String, collectionName: CollectionName, hc: HeaderCarrier): F[Boolean] =
    dbLookupStatus(value, collectionName, hc)
}

object DbLookupChecker {
  val alwaysPresent: DbLookupChecker[Id] = new DbLookupChecker[Id]((_, _, _) => true.pure[Id])
}
