/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.gform.eval.ExpressionResult
import uk.gov.hmrc.gform.gform.ExprUpdater
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.ExpressionResult.Hidden

/*
 The SummarySubstituter is designed to expand Sum expressions.
 It operates by unfolding expressions of the form `fc1 + if (fc2 > 10) then 1 else fc3`,
 where `fc2` and `fc3` are fields within the same AddToList component or a group component,
 and `fc1` represents an external, non-indexed component.

 Given an expression and a mapping (Map[Expr, ExpressionResult]) for ATL (e.g., `fc2` and `fc3`)
 or VariadicFormData for a group, the substituter expands the expression accordingly.

 For instance, consider the expression map:
 Map[Expr, ExpressionResult](
   FormCtx("1_fc2") -> NumberResult(10.00),
   FormCtx("2_fc2") -> NumberResult(20.00),
   FormCtx("1_fc3") -> NumberResult(10.00),
   FormCtx("2_fc3") -> NumberResult(20.00),
 )
 and variadicFormData:
  "1_fc2" -> "10",
  "2_fc2" -> "20",
  "1_fc3" -> "10",
  "2_fc3" -> "20"

 The substituter expands the expression to:
 (fc1 + if(fc2(1) > 10) then 1 else fc3(1)) + (fc1 + if(fc2(2) > 10) then 1 else fc3(2))

 If both the exprMap and variadicFormData are empty, the substituter does not expand the expression.
 This indicates that there is only a single element to sum, and thus, the sum of it is equivalent to the element itself.

 Note: Currently, the Sum expression supports only one indexed form component in the expression.
 Keep this in mind when putting together your expressions to make sure everything works as expected.
 */
object SummarySubstituter {
  import Substituter._

  implicit val exprSubstituter: Substituter[SummarySubstitutions, Expr] = new Substituter[SummarySubstitutions, Expr] {
    override def substitute(substitutions: SummarySubstitutions, expr: Expr): Expr = expr match {
      case Add(field1: Expr, field2: Expr) => Add(substitute(substitutions, field1), substitute(substitutions, field2))
      case Multiply(field1: Expr, field2: Expr) =>
        Multiply(substitute(substitutions, field1), substitute(substitutions, field2))
      case Subtraction(field1: Expr, field2: Expr) =>
        Subtraction(substitute(substitutions, field1), substitute(substitutions, field2))
      case Divide(field1: Expr, field2: Expr) =>
        Divide(substitute(substitutions, field1), substitute(substitutions, field2))
      case HideZeroDecimals(field1: Expr) => HideZeroDecimals(substitute(substitutions, field1))
      case IfElse(cond, field1: Expr, field2: Expr) =>
        IfElse(cond(substitutions), substitute(substitutions, field1), substitute(substitutions, field2))
      case Else(field1: Expr, field2: Expr) =>
        Else(substitute(substitutions, field1), substitute(substitutions, field2))
      case e: FormCtx                   => e
      case Sum(field1: Expr)            => substitutions.replaceSumWithAdds(substitute(substitutions, field1))
      case DateCtx(dateExpr)            => DateCtx(dateExpr(substitutions))
      case e: Count                     => e
      case e: Index                     => e
      case e: AuthCtx                   => e
      case e: UserCtx                   => e
      case e: Constant                  => e
      case e: PeriodValue               => e
      case Value                        => Value
      case e: FormTemplateCtx           => e
      case e: ParamCtx                  => e
      case e: LinkCtx                   => e
      case LangCtx                      => LangCtx
      case DateFunction(dateProjection) => DateFunction(dateProjection(substitutions))
      case Period(field1, field2)       => Period(substitute(substitutions, field1), substitute(substitutions, field2))
      case PeriodExt(period, func)      => PeriodExt(substitute(substitutions, period), func)
      case e: AddressLens               => e
      case e: DataRetrieveCtx           => e
      case e: DataRetrieveCount         => e
      case e: LookupColumn              => e
      case e: CsvCountryCountCheck      => e
      case e: Size                      => e
      case Typed(e, tpe)                => Typed(substitute(substitutions, e), tpe)
      case e: IndexOf                   => e
      case e: IndexOfDataRetrieveCtx    => e
      case e: NumberedList              => e
      case e: BulletedList              => e
      case StringOps(field1, stringFnc) => StringOps(substitute(substitutions, field1), stringFnc)
      case Concat(exprs)                => Concat(exprs.map(substitute(substitutions, _)))
      case CountryOfItmpAddress         => CountryOfItmpAddress
      case e: ChoicesRevealedField      => e
      case e: ChoiceLabel               => e
      case e: ChoicesSelected           => e
      case e: ChoicesAvailable          => e
    }
  }

  implicit val booleanExprSubstituter: Substituter[SummarySubstitutions, BooleanExpr] =
    new Substituter[SummarySubstitutions, BooleanExpr] {
      override def substitute(substitutions: SummarySubstitutions, expr: BooleanExpr): BooleanExpr = expr match {
        case Equals(left, right)              => Equals(left(substitutions), right(substitutions))
        case GreaterThan(left, right)         => GreaterThan(left(substitutions), right(substitutions))
        case GreaterThanOrEquals(left, right) => GreaterThanOrEquals(left(substitutions), right(substitutions))
        case LessThan(left, right)            => LessThan(left(substitutions), right(substitutions))
        case LessThanOrEquals(left, right)    => LessThanOrEquals(left(substitutions), right(substitutions))
        case Not(e)                           => Not(substitute(substitutions, e))
        case Or(left, right)                  => Or(substitute(substitutions, left), substitute(substitutions, right))
        case And(left, right)                 => And(substitute(substitutions, left), substitute(substitutions, right))
        case IsTrue                           => IsTrue
        case IsFalse                          => IsFalse
        case Contains(multiValueField, value) => Contains(multiValueField, value(substitutions))
        case In(formCtx, dataSource)          => In(formCtx(substitutions), dataSource)
        case HasAnswer(formCtx, atlFormCtx)   => HasAnswer(formCtx, atlFormCtx)
        case MatchRegex(expr, regex)          => MatchRegex(expr(substitutions), regex)
        case FormPhase(value)                 => FormPhase(value)
        case First(formCtx)                   => First(formCtx)
        case IsLogin(value)                   => IsLogin(value)
        case DateBefore(left, right)          => DateBefore(left(substitutions), right(substitutions))
        case DateAfter(left, right)           => DateAfter(left(substitutions), right(substitutions))
        case DuplicateExists(fieldList)       => DuplicateExists(fieldList)
      }
    }

  implicit val dateExprSubstituter: Substituter[SummarySubstitutions, DateExpr] =
    new Substituter[SummarySubstitutions, DateExpr] {
      override def substitute(substitutions: SummarySubstitutions, expr: DateExpr): DateExpr = expr match {
        case DateValueExpr(value)                         => DateValueExpr(value)
        case DateFormCtxVar(formCtx)                      => DateFormCtxVar(formCtx)
        case DateExprWithOffset(dExpr, o)                 => DateExprWithOffset(substitute(substitutions, dExpr), o)
        case HmrcTaxPeriodCtx(formCtx, hmrcTaxPeriodInfo) => HmrcTaxPeriodCtx(formCtx, hmrcTaxPeriodInfo)
        case d @ DataRetrieveDateCtx(_, _)                => d
        case DateIfElse(ifElse, field1, field2) =>
          DateIfElse(ifElse(substitutions), substitute(substitutions, field1), substitute(substitutions, field2))
        case DateOrElse(field1, field2) =>
          DateOrElse(substitute(substitutions, field1), substitute(substitutions, field2))
        case DateConstructExpr(dm, year) => DateConstructExpr(dm, year(substitutions))
      }

    }
  implicit val dateProjectionSubstituter: Substituter[SummarySubstitutions, DateProjection] =
    new Substituter[SummarySubstitutions, DateProjection] {
      override def substitute(substitutions: SummarySubstitutions, expr: DateProjection): DateProjection = expr match {
        case DateProjection.Day(dateExpr)   => DateProjection.Day(dateExpr(substitutions))
        case DateProjection.Month(dateExpr) => DateProjection.Month(dateExpr(substitutions))
        case DateProjection.Year(dateExpr)  => DateProjection.Year(dateExpr(substitutions))

      }

    }

}

case class SummarySubstitutions(
  exprMap: Map[Expr, ExpressionResult],
  repeatedComponentsDetails: RepeatedComponentsDetails
) {
  def replaceSumWithAdds(sumExpr: Expr): Expr = {
    val nestedAddHelper = NestedAddHelper(sumExpr, exprMap, repeatedComponentsDetails)
    nestedAddHelper.resolveSumExpression()
  }
}

case class NestedAddHelper(
  sumExpr: Expr,
  exprMap: Map[Expr, ExpressionResult],
  repeatedComponentsDetails: RepeatedComponentsDetails
) {
  private val sumFormComponentIds = sumExpr.allFormComponentIds().filter(repeatedComponentsDetails.isRepeated)
  private val sumModelComponentIds = sumFormComponentIds.map(_.modelComponentId)
  private val sumsParents = repeatedComponentsDetails.getParentIdsFor(sumFormComponentIds)
  private val isParentHidden = isAnyParentHidden(sumFormComponentIds)
  private val indices = calculateIndices()

  def resolveSumExpression(): Expr =
    if (isParentHidden) {
      Constant("0") // the whole repeated component is hidden
    } else if (sumsParents.isEmpty) {
      sumExpr // there is no repeated component in the sumExpr
    } else {
      constructNestedAddExpression()
    }

  private def isAnyParentHidden(formComponentIds: List[FormComponentId]): Boolean = {
    val hiddenComponents = exprMap
      .collect { case (FormCtx(fcId), Hidden) => fcId }
      .toList
      .distinct
    repeatedComponentsDetails.hasParent(hiddenComponents, formComponentIds)
  }

  /** Initial indexes are derived from exprMap
    * These are then constrained by the maximum index present in sumExpr, if it exists. This
    * scenario occurs when a sum expression is used within an indexed component that is
    * being summed.
    */
  private def calculateIndices(): List[Int] = {
    val initialIndices = exprMap
      .collect {
        case (FormCtx(fcId), _) if shouldBeIndexed(fcId) => fcId.modelComponentId.maybeIndex
      }
      .flatten
      .toList
      .distinct
      .sorted
    limitIndicesByMax(initialIndices)
  }

  private def shouldBeIndexed(fcId: FormComponentId): Boolean =
    sumModelComponentIds.exists { modelComponentId =>
      modelComponentId.baseComponentId == fcId.baseComponentId && fcId.modelComponentId.indexedComponentId.isIndexed
    }

  private def limitIndicesByMax(indices: List[Int]): List[Int] =
    extractIndices(sumModelComponentIds).maxOption match {
      case Some(maxIndex) => indices.filter(_ <= maxIndex)
      case None           => indices
    }

  private def extractIndices(ids: List[ModelComponentId]): List[Int] =
    ids.flatMap(_.maybeIndex).distinct.sorted

  private def constructNestedAddExpression(): Expr = {
    import FormComponentIdSubstituter._
    val substitutions = new FormComponentIdSubstitutions()
    val baseSumExpr = implicitly[Substituter[FormComponentIdSubstitutions, Expr]]
      .substitute(substitutions, sumExpr)
    val fcIdsWithoutIndices = sumModelComponentIds.map(_.removeIndex.toFormComponentId).distinct

    indices.foldLeft[Expr](Constant("0")) { (acc, index) =>
      val updatedExpr = ExprUpdater(baseSumExpr, index, fcIdsWithoutIndices)
      Add(acc, updatedExpr)
    }
  }
}
