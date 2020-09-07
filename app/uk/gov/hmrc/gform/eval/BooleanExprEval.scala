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

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.auth.UtrEligibilityRequest

import scala.language.higherKinds
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.graph.{ Convertible, Evaluator, NewValue }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataSource.SeissEligible
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.GraphNode
import uk.gov.hmrc.http.HeaderCarrier

class BooleanExprEval[F[_]: Monad](
  val evaluator: Evaluator[F],
  val seissConnectorEligibilityStatus: (UtrEligibilityRequest, HeaderCarrier) => F[Boolean]
) {
  def isTrue(
    expr: BooleanExpr,
    data: VariadicFormData,
    retrievals: MaterialisedRetrievals,
    visSet: Set[GraphNode],
    thirdPartyData: ThirdPartyData,
    maybeAccessCode: Option[AccessCode],
    envelopeId: EnvelopeId,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): F[Boolean] = {

    def loop(expr: BooleanExpr): F[Boolean] =
      isTrue(expr, data, retrievals, visSet, thirdPartyData, maybeAccessCode, envelopeId, formTemplate)

    def compare(
      leftField: Expr,
      rightField: Expr,
      bigDecimalRelation: (BigDecimal, BigDecimal) => Boolean,
      stringRelation: (String, String) => Boolean,
      formTemplate: FormTemplate)(implicit hc: HeaderCarrier): F[Boolean] = {

      def doComparison(left: Convertible[F], right: Convertible[F])(
        maybeBigDecimalA: Option[BigDecimal],
        maybeBigDecimalB: Option[BigDecimal]): F[Boolean] =
        (maybeBigDecimalA, maybeBigDecimalB) match {
          case (Some(bdA), Some(bdB)) => bigDecimalRelation(bdA, bdB).pure[F]
          case (_, _) =>
            for {
              maybeStringA <- Convertible.asString(left, formTemplate)
              maybeStringB <- Convertible.asString(right, formTemplate)
            } yield
              (maybeStringA, maybeStringB) match {
                case (Some(NewValue(strA)), Some(NewValue(strB))) => stringRelation(strA, strB)
                case (_, _)                                       => false
              }
        }

      val fcId = FormComponentId("dummy")
      evaluator
        .makeCalc(
          visSet,
          fcId,
          data,
          leftField,
          rightField,
          retrievals,
          formTemplate,
          doComparison,
          thirdPartyData,
          maybeAccessCode,
          envelopeId)
    }

    def includes(field: FormCtx, value: Expr): F[Boolean] = {
      val options: F[Set[String]] =
        evaluator.evalVariadicFormCtx(visSet, field, data).toSet.flatMap((_: VariadicValue).toSet).pure[F]

      val testValue: F[Option[String]] =
        Convertible
          .asString(
            evaluator.eval(
              visSet,
              FormComponentId("dummy"),
              value,
              data,
              retrievals,
              formTemplate,
              thirdPartyData,
              maybeAccessCode,
              envelopeId),
            formTemplate)
          .map(_.flatMap(_.cast[NewValue].map(_.value)))

      for {
        os <- options
        tv <- testValue
      } yield
        tv.fold(false) { v =>
          os.contains(v)
        }
    }

    def exists(value: Expr, dataSource: DataSource): F[Boolean] = {
      val expValue: F[Option[String]] =
        Convertible
          .asString(
            evaluator.eval(
              visSet,
              FormComponentId("dummy"),
              value,
              data,
              retrievals,
              formTemplate,
              thirdPartyData,
              maybeAccessCode,
              envelopeId),
            formTemplate)
          .map(_.flatMap(_.cast[NewValue].map(_.value)))

      for {
        ev <- expValue
        b <- ev.fold(false.pure[F]) { v =>
              dataSource match {
                case DataSource.SeissEligible     => seissConnectorEligibilityStatus(UtrEligibilityRequest(v), hc)
                case DataSource.Mongo(collection) => ???
                case DataSource.Enrolment(serviceName, identifierName) =>
                  retrievals.enrolmentExists(serviceName, identifierName, v).pure[F]
              }
            }
      } yield b
    }

    expr match {
      case Equals(field1, field2)              => compare(field1, field2, _ == _, _ == _, formTemplate)
      case NotEquals(field1, field2)           => compare(field1, field2, _ != _, _ != _, formTemplate)
      case GreaterThan(field1, field2)         => compare(field1, field2, _ > _, _ > _, formTemplate)
      case GreaterThanOrEquals(field1, field2) => compare(field1, field2, _ >= _, _ >= _, formTemplate)
      case LessThan(field1, field2)            => compare(field1, field2, _ < _, _ < _, formTemplate)
      case LessThanOrEquals(field1, field2)    => compare(field1, field2, _ <= _, _ <= _, formTemplate)
      case Not(invertedExpr)                   => loop(invertedExpr).map(!_)
      case Or(expr1, expr2)                    => for { e1 <- loop(expr1); e2 <- loop(expr2) } yield e1 | e2
      case And(expr1, expr2)                   => for { e1 <- loop(expr1); e2 <- loop(expr2) } yield e1 & e2
      case IsTrue                              => true.pure[F]
      case IsFalse                             => false.pure[F]
      case Contains(ctx, value)                => includes(ctx, value)
      case In(value, dataSource)               => exists(value, dataSource)
    }
  }
}
