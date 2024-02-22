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
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.models.ids.IndexedComponentId

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
      case IfElse(cond, field1: Expr, field2: Expr) =>
        IfElse(cond(substitutions), substitute(substitutions, field1), substitute(substitutions, field2))
      case Else(field1: Expr, field2: Expr) =>
        Else(substitute(substitutions, field1), substitute(substitutions, field2))
      case e: FormCtx                   => e
      case Sum(field1: Expr)            => substitutions.replaceSumWithAdds(substitute(substitutions, field1))
      case DateCtx(dateExpr)            => DateCtx(dateExpr(substitutions))
      case e: Count                     => e
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
      case e: CsvCountryCheck           => e
      case e: CsvOverseasCountryCheck   => e
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
        case MatchRegex(expr, regex)          => MatchRegex(expr(substitutions), regex)
        case FormPhase(value)                 => FormPhase(value)
        case First(formCtx)                   => First(formCtx)
        case IsLogin(value)                   => IsLogin(value)
        case DateBefore(left, right)          => DateBefore(left(substitutions), right(substitutions))
        case DateAfter(left, right)           => DateAfter(left(substitutions), right(substitutions))
      }
    }

  implicit val dateExprSubstituter: Substituter[SummarySubstitutions, DateExpr] =
    new Substituter[SummarySubstitutions, DateExpr] {
      override def substitute(substitutions: SummarySubstitutions, expr: DateExpr): DateExpr = expr match {
        case DateValueExpr(value)                         => DateValueExpr(value)
        case DateFormCtxVar(formCtx)                      => DateFormCtxVar(formCtx)
        case DateExprWithOffset(dExpr, o)                 => DateExprWithOffset(substitute(substitutions, dExpr), o)
        case HmrcTaxPeriodCtx(formCtx, hmrcTaxPeriodInfo) => HmrcTaxPeriodCtx(formCtx, hmrcTaxPeriodInfo)
        case DateIfElse(ifElse, field1, field2) =>
          DateIfElse(ifElse(substitutions), substitute(substitutions, field1), substitute(substitutions, field2))
        case DateOrElse(field1, field2) =>
          DateOrElse(substitute(substitutions, field1), substitute(substitutions, field2))
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

  case class SummarySubstitutions(
    exprMap: Map[Expr, ExpressionResult],
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ) {

    def replaceSumWithAdds(sumExpr: Expr): Expr = {
      val atlHelper = new ATLNestedAddHelper(exprMap, sumExpr)
      val groupHelper = new GroupNestedAddHelper(data, sumExpr)
      if (atlHelper.indexedFormComponentIds.nonEmpty) {
        atlHelper.toNestedAdd()
      } else if (groupHelper.indexedFormComponentIds.nonEmpty) {
        groupHelper.toNestedAdd()
      } else {
        sumExpr
      }
    }
  }

  abstract class NestedAddHelper(sumExpr: Expr) {

    val sumExprFcIds = sumExpr.allFormComponentIds()

    def computeIndexedFormComponentIds(): List[FormComponentId]
    def computeIndices(): List[Int]

    lazy val indexedFormComponentIds: List[FormComponentId] = computeIndexedFormComponentIds()
    lazy val indices: List[Int] = computeIndices()

    def toNestedAdd(): Expr = {
      import FormComponentIdSubstituter._
      val substitutions = new FormComponentIdSubstitutions()
      val baseSumExpr: Expr =
        implicitly[Substituter[FormComponentIdSubstitutions, Expr]].substitute(substitutions, sumExpr)
      val fcs = indexedFormComponentIds.map(_.modelComponentId.removeIndex.toFormComponentId).distinct
      indices
        .map(i => ExprUpdater(baseSumExpr, i, fcs))
        .foldLeft[Expr](Constant("0")) { case (acc, e) =>
          Add(acc, e)
        }
    }

    protected def extractIndices(fcIds: List[FormComponentId]): List[Int] =
      fcIds
        .map(fcId => fcId.modelComponentId.indexedComponentId)
        .collect { case IndexedComponentId.Indexed(_, index) =>
          index
        }
        .toList
        .sorted
        .distinct
  }

  class ATLNestedAddHelper(exprMap: Map[Expr, ExpressionResult], sumExpr: Expr) extends NestedAddHelper(sumExpr) {
    override def computeIndexedFormComponentIds(): List[FormComponentId] =
      exprMap
        .collect {
          case (FormCtx(fcId), evalResult)
              if sumExprFcIds
                .map(_.baseComponentId)
                .contains(fcId.baseComponentId) && fcId.modelComponentId.indexedComponentId.isIndexed =>
            fcId -> evalResult
        }
        .toMap
        .map(_._1)
        .toList
    override def computeIndices(): List[Int] = {
      val maybeMaxIndex = extractIndices(sumExprFcIds).maxOption
      extractIndices(indexedFormComponentIds).filter(i => maybeMaxIndex.map(_ >= i).getOrElse(true))
    }
  }

  class GroupNestedAddHelper(data: VariadicFormData[SourceOrigin.OutOfDate], sumExpr: Expr)
      extends NestedAddHelper(sumExpr) {
    override def computeIndexedFormComponentIds(): List[FormComponentId] = {
      def indicesFromData(
        formComponentId: FormComponentId
      ): List[FormComponentId] = {
        val allValues = data.forBaseComponentId(formComponentId.baseComponentId)
        allValues.map(_._1).map(_.toFormComponentId).toList.filter(_.modelComponentId.maybeIndex.isDefined)
      }
      sumExpr.allFormComponentIds().flatMap(indicesFromData(_))
    }
    override def computeIndices(): List[Int] = extractIndices(indexedFormComponentIds)

  }
}
