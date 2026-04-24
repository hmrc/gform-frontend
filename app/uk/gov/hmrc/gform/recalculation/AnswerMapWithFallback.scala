/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.recalculation

import scala.util.Try
import uk.gov.hmrc.gform.commons.BigDecimalUtil
import uk.gov.hmrc.gform.eval.{ ExprType, StaticTypeInfo }
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.VariadicValue
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

class AnswerMapWithFallback(
  val answerMap: AnswerMap,
  val mongoUserData: MongoUserData,
  staticTypeInfo: StaticTypeInfo
) {

  def cleared(modelComponentIds: List[ModelComponentId]): AnswerMapWithFallback = {
    answerMap.cleared(modelComponentIds) // Side effect
    new AnswerMapWithFallback(
      answerMap,
      mongoUserData.cleared(modelComponentIds),
      staticTypeInfo
    )
  }

  def toStringResultOrOptionResult(modelComponentId: ModelComponentId): EvaluationStatus =
    answerMap
      .get(modelComponentId)
      .orElse(
        mongoUserData.lookup
          .get(modelComponentId)
          .map(
            _.fold[EvaluationStatus] { one =>
              if (one.isEmpty) { // Optional field
                EvaluationStatus.Empty
              } else {
                val componentStaticType: ExprType = staticTypeInfo(modelComponentId.baseComponentId).exprType
                componentStaticType match {
                  case ExprType.Number =>
                    BigDecimalUtil
                      .toBigDecimalSafe(one.value)
                      .fold[EvaluationStatus](EvaluationStatus.StringResult(one.value))(bd =>
                        EvaluationStatus.NumberResult(bd)
                      )
                  case _ => EvaluationStatus.StringResult(one.value)
                }
              }

            }(many => EvaluationStatus.OptionResult(many.value))
          )
      )
      .getOrElse(EvaluationStatus.Empty)

  def expectNumberResult(fcId: FormComponentId, atom: Atom) = {
    val modelComponentId = fcId.toAtomicFormComponentId(atom)
    answerMap
      .get(modelComponentId)
      .orElse(mongoUserData.lookup.get(modelComponentId).map(toOneNumber))
      .getOrElse(EvaluationStatus.Empty)
  }

  def maxIndexOf(baseComponentId: BaseComponentId): Option[Int] =
    mongoUserData.lookup
      .forBaseComponentId(baseComponentId)
      .flatMap { case (k, _) => k.maybeIndex }
      .maxOption

  def allModelComponentIds(
    modelComponentId: ModelComponentId
  ): List[(ModelComponentId, EvaluationStatus)] =
    if (answerMap.isManaging(modelComponentId)) {
      answerMap.forBaseComponentId(modelComponentId.baseComponentId)
    } else {
      mongoUserData.lookup.forBaseComponentId(modelComponentId.baseComponentId).map {
        case (modelComponentId, variadicValue) =>
          val componentStaticType: ExprType = staticTypeInfo(modelComponentId.baseComponentId).exprType
          val evaluationStatus = toEvaluationStatus(variadicValue, componentStaticType)
          (modelComponentId, evaluationStatus)
      }
    }

  private def toEvaluationStatus(variadicValue: VariadicValue, componentStaticType: ExprType): EvaluationStatus =
    variadicValue match {
      case VariadicValue.One(v) =>
        componentStaticType match {
          case ExprType.Number => EvaluationStatus.NumberResult(BigDecimal(v))
          case _               => EvaluationStatus.StringResult(v)
        }
      case VariadicValue.Many(vs) =>
        EvaluationStatus.OptionResult(vs)
    }

  private def toOneNumber(variadicValue: VariadicValue): EvaluationStatus.NumberResult = {
    val v = variadicValue.toOne.value
    Try(v.toLong)
      .map(EvaluationStatus.NumberResult(_))
      .getOrElse(throw new RuntimeException(s"$v cannot be converted to number."))
  }
}

object AnswerMapWithFallback {
  def apply(
    answerMap: AnswerMap,
    mongoUserData: MongoUserData,
    staticTypeInfo: StaticTypeInfo
  ): AnswerMapWithFallback =
    new AnswerMapWithFallback(answerMap, mongoUserData, staticTypeInfo)
}
