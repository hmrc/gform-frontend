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

package uk.gov.hmrc.gform.testonly

import cats.data.NonEmptyList
import play.api.libs.json.{ JsObject, Json }
import play.api.mvc.MessagesControllerComponents
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.eval.{ AllFormComponentExpressions, ExprMetadata, ExprType, StaticTypeData, TypeInfo }
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

class DebugController(
  auth: AuthenticatedRequestActions,
  fileUploadService: FileUploadService,
  messagesControllerComponents: MessagesControllerComponents
)(
  implicit ec: ExecutionContext
) extends FrontendController(messagesControllerComponents) {

  def model(formTemplateId: FormTemplateId) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, None) {
      implicit request => lang => cache => sse => formModelOptics =>
        val totalColumns = formModelOptics.formModelRenderPageOptics.formModel.pages.size

        for {
          envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
        } yield {
          val page = html.debug.model(formModelOptics, totalColumns, envelope)
          Ok(page)
        }

    }
  def exprs(formTemplateId: FormTemplateId) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, None) {
      request => lang => cache => sse => formModelOptics =>
        val graphTopologicalOrder: List[JsObject] =
          formModelOptics.formModelVisibilityOptics.graphData.graphTopologicalOrder.toList.map {
            case (layerNumber, nodes) =>
              Json.obj(
                "layerNumber" -> layerNumber.toString,
                "nodes"       -> nodes.toString
              )
          }

        val exprs: List[JsObject] =
          formModelOptics.formModelVisibilityOptics.evaluationResults.exprMap.toList.sortBy(_._1.toString).map {
            case (typedExpr, expressionResult) =>
              Json.obj(
                "typedExpr"        -> typedExpr.toString,
                "expressionResult" -> expressionResult.toString
              )
          }

        val result = Json.obj(
          "expression" -> Json.toJson(exprs),
          "graph"      -> Json.toJson(graphTopologicalOrder)
        )

        Future.successful(Ok(result))
    }

  def verify(formTemplateId: FormTemplateId) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, None) {
      request => lang => cache => sse => formModelOptics =>
        val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel

        val inspectedExplicitExprs: List[InspectedExplicitExpr] = formModel.allFormComponents.collect {
          case fc @ HasValueExpr(expr) => inspectExplicitExpr(expr, fc.id, formModel)
        }

        val formComponentMetadatas: List[ExprMetadata] =
          formModel.allFormComponents.flatMap(AllFormComponentExpressions.unapply).flatten

        val exprMetadatas: List[ExprMetadata] = formModel.exprsMetadata ++ formComponentMetadatas

        val inspectedExprs: List[InspectedFirstExpr] =
          exprMetadatas.map(_.expr).map(inspectExpr(_, formModel))

        val includeIfs: List[IncludeIf] = formModel.allIncludeIfsWithDependingFormComponents.map(_._1)

        val inspectedIncludeIfs: List[BooleanExprInfo] =
          includeIfs.map(includeIf => inspectBooleanExpr(includeIf.booleanExpr, formModel))

        val validIfs: List[ValidIf] = formModel.allValidIfs.flatMap(_._1)

        val inspectedValidIfs: List[BooleanExprInfo] =
          validIfs.map(validIf => inspectBooleanExpr(validIf.booleanExpr, formModel))

        val page =
          html.debug.verify.verify(inspectedExplicitExprs, inspectedExprs, inspectedIncludeIfs, inspectedValidIfs)
        Future.successful(Ok(page))

    }

  def inspectBooleanExpr(booleanExpr: BooleanExpr, formModel: FormModel[DataExpanded]): BooleanExprInfo = {

    def loop(booleanExpr: BooleanExpr): List[InspectedBooleanExpr] = {
      def inner(expr1: Expr, expr2: Expr): InspectedBooleanExpr.Binary =
        InspectedBooleanExpr.Binary(booleanExpr, inspectExpr(expr1, formModel), inspectExpr(expr2, formModel))
      booleanExpr match {
        case Equals(left, right)              => inner(left, right) :: Nil
        case GreaterThan(left, right)         => inner(left, right) :: Nil
        case GreaterThanOrEquals(left, right) => inner(left, right) :: Nil
        case LessThan(left, right)            => inner(left, right) :: Nil
        case LessThanOrEquals(left, right)    => inner(left, right) :: Nil
        case Not(e)                           => loop(e)
        case Or(left, right)                  => loop(left) ::: loop(right)
        case And(left, right)                 => loop(left) ::: loop(right)
        case IsTrue                           => Nil
        case IsFalse                          => Nil
        case Contains(multiValueField, value) =>
          InspectedBooleanExpr
            .Contains(booleanExpr, inspectExpr(multiValueField, formModel), inspectExpr(value, formModel)) :: Nil
        case In(formCtx, dataSource) => Nil
      }
    }

    BooleanExprInfo(booleanExpr, loop(booleanExpr))
  }

  def inspectExpr(
    expr: Expr,
    formModel: FormModel[DataExpanded]
  ): InspectedFirstExpr = {
    val firstOperandInfo: TypeInfo = formModel.toFirstOperandTypeInfo(expr)
    val typeInfos = NonEmptyList(firstOperandInfo, new ExprInspector(formModel)(expr))
    InspectedFirstExpr(expr, typeInfos)
  }

  def inspectExplicitExpr(
    expr: Expr,
    formComponentId: FormComponentId,
    formModel: FormModel[DataExpanded]
  ): InspectedExplicitExpr = {
    val explicitTypeInfo: TypeInfo = formModel.explicitTypedExpr(expr, formComponentId)
    val typeInfos = NonEmptyList(explicitTypeInfo, new ExprInspector(formModel)(expr))
    InspectedExplicitExpr(expr, typeInfos, formComponentId)
  }

}

case class BooleanExprInfo(booleanExpr: BooleanExpr, inspectedBooleanExprs: List[InspectedBooleanExpr])
sealed trait InspectedBooleanExpr extends Product with Serializable {
  def booleanExpr: BooleanExpr
  def isOk: Boolean
  def fold[B](
    a: InspectedBooleanExpr.Binary => B
  )(
    b: InspectedBooleanExpr.Contains => B
  ): B =
    this match {
      case t: InspectedBooleanExpr.Binary   => a(t)
      case t: InspectedBooleanExpr.Contains => b(t)
    }
}
object InspectedBooleanExpr {
  case class Binary(booleanExpr: BooleanExpr, left: InspectedFirstExpr, right: InspectedFirstExpr)
      extends InspectedBooleanExpr {
    val isOk = {
      if (CompatibilityRules.isWholeNumberConstant(left.expr)) {
        CompatibilityRules.compatibleWithNumberConstant(right.typeInfos.head)
      } else if (CompatibilityRules.isWholeNumberConstant(right.expr)) {
        CompatibilityRules.compatibleWithNumberConstant(left.typeInfos.head)
      } else {
        CompatibilityRules.compatibleTypeInfos(left.typeInfos.head, right.typeInfos.head)
      }

    }
  }
  case class Contains(
    booleanExpr: BooleanExpr,
    fieldInspectedExpr: InspectedFirstExpr,
    inspectedExpr: InspectedFirstExpr
  ) extends InspectedBooleanExpr {
    val isOk = fieldInspectedExpr.typeInfos.head.staticTypeData.exprType == ExprType.ChoiceSelection &&
      inspectedExpr.typeInfos.head.staticTypeData.exprType == ExprType.Number &&
      (CompatibilityRules.isWholeNumberConstant(inspectedExpr.typeInfos.head.expr) || inspectedExpr.typeInfos.head.staticTypeData.textConstraint
        .fold(true) {
          case Number(_, 0, _, _)         => true
          case PositiveNumber(_, 0, _, _) => true
          case _                          => false
        })
  }

}

sealed trait InspectedExpr extends Product with Serializable {
  def expr: Expr
  def typeInfos: NonEmptyList[TypeInfo]
  def isOk: Boolean
}
case class InspectedExplicitExpr(expr: Expr, typeInfos: NonEmptyList[TypeInfo], formComponentId: FormComponentId)
    extends InspectedExpr {
  def compatibleWithHead(typeInfo: TypeInfo): Boolean = CompatibilityRules.compatibleTypeInfos(typeInfos.head, typeInfo)

  val isOk: Boolean = typeInfos.tail.forall(compatibleWithHead)
}

case class InspectedFirstExpr(expr: Expr, typeInfos: NonEmptyList[TypeInfo]) extends InspectedExpr {
  def compatibleWithHead(typeInfo: TypeInfo): Boolean = CompatibilityRules.compatibleTypeInfos(typeInfos.head, typeInfo)

  val isOk: Boolean = typeInfos.tail.forall(compatibleWithHead)
}

class ExprInspector(formModel: FormModel[DataExpanded]) {
  private def inner(expr1: Expr, expr2: Expr) = {
    val typeInfo1 = formModel.toFirstOperandTypeInfo(expr1)
    val typeInfo2 = formModel.toFirstOperandTypeInfo(expr2)
    List(typeInfo1, typeInfo2) ::: apply(expr1) ::: apply(expr2)
  }

  def apply(expr: Expr): List[TypeInfo] =
    expr match {
      case Add(expr1: Expr, expr2: Expr)         => inner(expr1, expr2)
      case Multiply(expr1: Expr, expr2: Expr)    => inner(expr1, expr2)
      case Subtraction(expr1: Expr, expr2: Expr) => inner(expr1, expr2)
      case Else(expr1: Expr, expr2: Expr)        => inner(expr1, expr2)
      case otherwise                             => Nil
    }
}

object CompatibilityRules {

  def isWholeNumberConstant(expr: Expr): Boolean = expr match {
    case Constant(c) => Try(c.toInt).isSuccess
    case _           => false
  }

  def compatibleWithNumberConstant(typeInfo: TypeInfo): Boolean =
    typeInfo.staticTypeData match {
      case StaticTypeData(ExprType.Number, Some(textConstraint)) =>
        textConstraint match {
          case Sterling(_, _)             => true
          case Number(_, x, _, _)         => x <= 2
          case PositiveNumber(_, x, _, _) => x <= 2
          case _                          => false
        }

      case _ => false
    }
  def compatibleTypeInfos(typeInfo1: TypeInfo, typeInfo2: TypeInfo): Boolean =
    (typeInfo1.staticTypeData, typeInfo2.staticTypeData) match {
      case (
          StaticTypeData(ExprType.Number, Some(textConstraint1)),
          StaticTypeData(ExprType.Number, Some(textConstraint2))) =>
        CompatibilityRules.compatibleTextConstraint(textConstraint1, textConstraint2)
      case (StaticTypeData(ExprType.String, _), StaticTypeData(ExprType.String, _)) => true
      case (a, b)                                                                   => a == b

    }

  def compatibleTextConstraint(constraint1: TextConstraint, constraint2: TextConstraint): Boolean =
    (constraint1, constraint2) match {
      // format: off
      case (Sterling(roundingMode1, _), Sterling(roundingMode2, _))                       => roundingMode1 == roundingMode2
      case (Sterling(roundingMode1, _), Number(_, x, roundingMode2, _))         if x <= 2 => roundingMode1 == roundingMode2
      case (Sterling(roundingMode1, _), PositiveNumber(_, x, roundingMode2, _)) if x <= 2 => roundingMode1 == roundingMode2
      case (Number(_, x, roundingMode1, _), Sterling(roundingMode2, _))         if x <= 2 => roundingMode1 == roundingMode2
      case (PositiveNumber(_, x, roundingMode1, _), Sterling(roundingMode2, _)) if x <= 2 => roundingMode1 == roundingMode2
      case (a, b) => a == b
      // format: on
    }
}
