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

package uk.gov.hmrc.gform.eval

import cats.instances.list._
import cats.syntax.traverse._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataRetrieveCtx
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieveId, DataRetrieveResult, RetrieveDataType }

object DataRetrieveEval {
  private[eval] def getDataRetrieveAttribute(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): Option[List[String]] =
    dataRetrieve
      .get(dataRetrieveCtx.id)
      .flatMap { case DataRetrieveResult(_, data, _) =>
        data match {
          case RetrieveDataType.ObjectType(map) => map.get(dataRetrieveCtx.attribute).map(List(_))
          case RetrieveDataType.ListType(xs)    => xs.traverse(_.get(dataRetrieveCtx.attribute))
        }
      }
}
