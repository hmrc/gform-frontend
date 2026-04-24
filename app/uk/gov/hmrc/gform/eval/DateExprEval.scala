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

import java.time.LocalDate
import scala.util.Try
import uk.gov.hmrc.gform.recalculation.{ EvaluationContext, EvaluationStatus }
import uk.gov.hmrc.gform.recalculation.EvaluationStatus.DateResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataRetrieveCtx
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ OffsetUnit, OffsetYMD }

object DateExprEval {

  def evalDateConstructExpr(
    dayMonthResult: EvaluationStatus,
    yearResult: EvaluationStatus
  ): EvaluationStatus = {
    val resultOpt: Option[LocalDate] = dayMonthResult match {
      case dm: DateResult =>
        yearResult.fold[Option[LocalDate]](_ => None)(_ => None)(d =>
          Try(LocalDate.of(d.value.toInt, dm.value.getMonthValue, dm.value.getDayOfMonth)).toOption
        )(e => Try(LocalDate.of(e.value.toInt, dm.value.getMonthValue, dm.value.getDayOfMonth)).toOption)(f =>
          Try(LocalDate.of(f.value.head.toInt, dm.value.getMonthValue, dm.value.getDayOfMonth)).toOption
        )(g => Try(LocalDate.of(g.value.getYear, dm.value.getMonthValue, dm.value.getDayOfMonth)).toOption)(_ => None)(
          _ => None
        )(_ => None)(_ => None)
      case otherwise => None
    }

    resultOpt.map(DateResult.mkDate).getOrElse(EvaluationStatus.Empty)
  }

  def toDateResult(value: String): EvaluationStatus =
    Try(DateResult.mkDate(LocalDate.parse(value))).toOption.getOrElse(EvaluationStatus.Empty)

  def evalDataRetrieveDate(
    id: DataRetrieveId,
    attribute: DataRetrieve.Attribute,
    evaluationContext: EvaluationContext
  ): EvaluationStatus =
    evaluationContext.thirdPartyData.dataRetrieve
      .flatMap { dataRetrieve =>
        DataRetrieveEval
          .getDataRetrieveAttribute(dataRetrieve, DataRetrieveCtx(id, attribute))
          .map {
            case s :: Nil => toDateResult(s)
            case xs       => EvaluationStatus.ListResult(xs.map(d => toDateResult(d)))
          }
      }
      .getOrElse(EvaluationStatus.Empty)

  def addOffset(d: LocalDate, offset: OffsetYMD): LocalDate =
    offset.offsets.foldLeft(d) { (acc, offset) =>
      offset match {
        case OffsetUnit.Year(n)  => acc.plusYears(n.toLong)
        case OffsetUnit.Month(n) => acc.plusMonths(n.toLong)
        case OffsetUnit.Day(n)   => acc.plusDays(n.toLong)
      }
    }
}
