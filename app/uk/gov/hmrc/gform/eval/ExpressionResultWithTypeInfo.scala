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

case class ExpressionResultWithTypeInfo(expressionResult: ExpressionResult, typeInfo: TypeInfo) {

  def stringRepresentation(implicit messages: Messages): String =
    expressionResult.stringRepresentation(typeInfo, messages)

  def addressRepresentation: List[String] = expressionResult.addressRepresentation(typeInfo)

  def numberRepresentation: Option[BigDecimal] = expressionResult.numberRepresentation

  def optionRepresentation: Option[Seq[String]] = expressionResult.optionRepresentation

  def dateRepresentation: Option[LocalDate] = expressionResult.dateRepresentation(typeInfo)

  def listRepresentation(implicit messages: Messages): List[String] =
    expressionResult.listRepresentation(typeInfo, messages)

  def isEmpty = expressionResult === ExpressionResult.empty

}
