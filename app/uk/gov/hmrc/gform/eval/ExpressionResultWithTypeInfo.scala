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

import cats.syntax.eq._
import play.api.i18n.Messages

import java.time.LocalDate
import uk.gov.hmrc.gform.recalculation.EvaluationStatus

final case class ExpressionResultWithTypeInfo(evaluationStatus: EvaluationStatus, staticTypeData: StaticTypeData) {

  def stringRepresentation(implicit messages: Messages): String =
    evaluationStatus.stringRepresentation(staticTypeData, messages)

  def handlebarRepresentation(implicit messages: Messages): String =
    evaluationStatus.handlebarRepresentation(staticTypeData, messages)

  def addressRepresentation: List[String] =
    evaluationStatus match {
      case ar: EvaluationStatus.AddressResult => ar.lines
      case EvaluationStatus.ListResult(results) =>
        results.flatMap {
          case ar: EvaluationStatus.AddressResult => ar.lines
          case _                                  => List.empty[String]
        }
      case _ => List.empty[String]
    }

  def numberRepresentation: Option[BigDecimal] = evaluationStatus.numberRepresentation

  def optionRepresentation: Option[Seq[String]] = evaluationStatus.optionRepresentation

  def dateRepresentation: Option[LocalDate] = evaluationStatus.dateRepresentation

  def listRepresentation(implicit messages: Messages): List[String] = evaluationStatus match {
    case EvaluationStatus.ListResult(xs) => xs.map(_.stringRepresentation(staticTypeData, messages))
    case otherwise                       => List(otherwise.stringRepresentation(staticTypeData, messages))
  }

  def isEmpty = evaluationStatus === EvaluationStatus.Empty

}
