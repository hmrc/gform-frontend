/*
 * Copyright 2021 HM Revenue & Customs
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

import cats.Monoid
import cats.instances.either._
import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.traverse._
import uk.gov.hmrc.gform.commons.BigDecimalUtil.toBigDecimalSafe
import uk.gov.hmrc.gform.eval.DateExprEval.evalDateExpr
import uk.gov.hmrc.gform.gform.AuthContextPrepop
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.graph.processor.UserCtxEvaluatorProcessor
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicValue }

case class EvaluationResults(
  exprMap: Map[Expr, ExpressionResult]
) {

  def +(expr: Expr, result: ExpressionResult): EvaluationResults = this.copy(exprMap = exprMap + (expr -> result))
  def ++(otherExprMap: Map[Expr, ExpressionResult]): EvaluationResults =
    this.copy(exprMap = exprMap ++ otherExprMap)

  def get(expr: Expr): Option[ExpressionResult] = exprMap.get(expr)

  import ExpressionResult._

  private def unsupportedMany(str: String)(many: VariadicValue.Many): ExpressionResult =
    ExpressionResult.invalid(s"$str - unsupported value $many")

  private def unsupportedOperation(str: String)(expr: Expr): ExpressionResult =
    Invalid(s"$str - unsupported computation. Cannot combine $str and $expr")

  private def get(
    expr: FormCtx,
    recData: RecData[SourceOrigin.OutOfDate],
    fromVariadicValue: VariadicValue => ExpressionResult,
    fileIdsWithMapping: FileIdsWithMapping
  ): ExpressionResult = {
    val modelComponentId = expr.formComponentId.modelComponentId
    val expressionResult = exprMap.getOrElse(
      expr,
      recData.variadicFormData
        .get(modelComponentId)
        .fold(ExpressionResult.empty)(fromVariadicValue)
    )
    if (fileIdsWithMapping.isFileField(modelComponentId))
      stripFileName(expressionResult, modelComponentId, fileIdsWithMapping.mapping)
    else expressionResult
  }

  private def stripFileName(
    expressionResult: ExpressionResult,
    modelComponentId: ModelComponentId,
    componentIdToFileId: FormComponentIdToFileIdMapping
  ): ExpressionResult = {
    val fileIdPrefix: String =
      componentIdToFileId.find(modelComponentId).fold(modelComponentId.toMongoIdentifier)(_.value)
    expressionResult.withStringResult(expressionResult) { fileName =>
      StringResult(fileName.replace(fileIdPrefix + "_", ""))
    }
  }

  private def whenVisible(formComponentId: FormComponentId)(body: => ExpressionResult) = {
    val isHidden = exprMap.get(FormCtx(formComponentId)).fold(false)(_ === Hidden)
    if (isHidden) {
      ExpressionResult.Hidden
    } else body
  }

  // Sum field may be hidden by AddToList or by Revealing choice
  private def isSumHidden(modelComponentId: ModelComponentId): Boolean = {
    val expr = FormCtx(modelComponentId.toFormComponentId)
    exprMap.get(expr).fold(true)(_ === Hidden)
  }

  private def calculateSum(
    formComponentId: FormComponentId,
    recData: RecData[SourceOrigin.OutOfDate],
    invalidResult: ExpressionResult
  ): ExpressionResult = {
    val maybeListToSum: Either[ExpressionResult, List[BigDecimal]] =
      recData.variadicFormData
        .forBaseComponentId(formComponentId.baseComponentId)
        .toList
        .collect {
          case (k, v) if !isSumHidden(k) => v
        }
        .traverse {
          case VariadicValue.One(v) =>
            toBigDecimalSafe(v)
              .fold[Either[ExpressionResult, BigDecimal]](Left(invalidResult))(Right(_))
          case VariadicValue.Many(_) => Left(invalidResult)
        }
    maybeListToSum.map(listToSum => NumberResult(listToSum.sum)).merge
  }

  private def addToListCount(formComponentId: FormComponentId, recData: RecData[SourceOrigin.OutOfDate]) = {
    val firstQuestionFcId = formComponentId.withFirstIndex
    val isHidden = exprMap.get(FormCtx(firstQuestionFcId))
    if (isHidden.contains(Hidden)) {
      NumberResult(0)
    } else {
      val xs: Iterable[(ModelComponentId, VariadicValue)] =
        recData.variadicFormData.forBaseComponentId(formComponentId.baseComponentId)
      val zeros: Int = xs.map(_._2).count(_.contains(0.toString))

      NumberResult(zeros + 1)
    }
  }

  private def evalNumber(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  ): ExpressionResult = {

    def fromVariadicValue(variadicValue: VariadicValue): ExpressionResult =
      variadicValue.fold(one => toNumberResult(one.value))(unsupportedMany("Number"))

    def toNumberResult(value: String): ExpressionResult =
      toBigDecimalSafe(value).fold(ExpressionResult.invalid(s"Number - cannot convert '$value' to number"))(
        NumberResult.apply
      )

    def loop(expr: Expr): ExpressionResult = expr match {
      case Add(field1: Expr, field2: Expr)            => loop(field1) + loop(field2)
      case Multiply(field1: Expr, field2: Expr)       => loop(field1) * loop(field2)
      case Subtraction(field1: Expr, field2: Expr)    => loop(field1) - loop(field2)
      case Else(field1: Expr, field2: Expr)           => loop(field1) orElse loop(field2)
      case ctx @ FormCtx(formComponentId)             => get(ctx, recData, fromVariadicValue, evaluationContext.fileIdsWithMapping)
      case Sum(FormCtx(formComponentId))              => calculateSum(formComponentId, recData, unsupportedOperation("Number")(expr))
      case Sum(_)                                     => unsupportedOperation("Number")(expr)
      case Count(formComponentId)                     => addToListCount(formComponentId, recData)
      case AuthCtx(value: AuthInfo)                   => unsupportedOperation("Number")(expr)
      case UserCtx(value: UserField)                  => unsupportedOperation("Number")(expr)
      case Constant(value: String)                    => toNumberResult(value)
      case HmrcRosmRegistrationCheck(value: RosmProp) => unsupportedOperation("Number")(expr)
      case Value                                      => Empty
      case FormTemplateCtx(value: FormTemplateProp)   => unsupportedOperation("Number")(expr)
      case ParamCtx(_)                                => unsupportedOperation("Number")(expr)
      case LinkCtx(_)                                 => unsupportedOperation("Number")(expr)
      case DateCtx(_)                                 => unsupportedOperation("Number")(expr)
      case Period(_, _)                               => unsupportedOperation("Number")(expr)
      case PeriodExt(_, _)                            => unsupportedOperation("Number")(expr)
      case PeriodValue(_)                             => unsupportedOperation("Number")(expr)
      case AddressLens(_, _)                          => unsupportedOperation("Number")(expr)
    }

    loop(typeInfo.expr)
  }

  private def evalString(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  ): ExpressionResult = {

    def nonEmpty(stringResult: StringResult): ExpressionResult =
      if (stringResult.value.trim.isEmpty) Empty else stringResult

    def fromVariadicValue(variadicValue: VariadicValue): ExpressionResult =
      variadicValue.fold[ExpressionResult](one => nonEmpty(StringResult(one.value)))(many =>
        ExpressionResult.OptionResult(many.value.map(_.toInt))
      )

    def loop(expr: Expr): ExpressionResult = expr match {
      case Add(field1: Expr, field2: Expr)         => loop(field1) + loop(field2)
      case Multiply(field1: Expr, field2: Expr)    => unsupportedOperation("String")(expr)
      case Subtraction(field1: Expr, field2: Expr) => unsupportedOperation("String")(expr)
      case Else(field1: Expr, field2: Expr)        => loop(field1) orElse loop(field2)
      case ctx @ FormCtx(formComponentId: FormComponentId)
          if evaluationContext.addressLookup(formComponentId.baseComponentId) || evaluationContext
            .overseasAddressLookup(formComponentId.baseComponentId) =>
        whenVisible(formComponentId) {
          val indexedComponentId = formComponentId.modelComponentId.indexedComponentId
          val addressAtoms: List[ModelComponentId.Atomic] =
            if (evaluationContext.addressLookup(formComponentId.baseComponentId))
              Address.fields(indexedComponentId).filter(_.atom != Address.uk)
            else
              OverseasAddress.fields(indexedComponentId).toList

          val variadicValues: List[Option[VariadicValue]] = addressAtoms.map(atom => recData.variadicFormData.get(atom))
          val addressLines = variadicValues.collect { case Some(VariadicValue.One(value)) if value.nonEmpty => value }
          ExpressionResult.AddressResult(addressLines)
        }
      case ctx @ FormCtx(formComponentId: FormComponentId) =>
        get(ctx, recData, fromVariadicValue, evaluationContext.fileIdsWithMapping)
      case Sum(field1: Expr) => unsupportedOperation("String")(expr)
      case Count(formComponentId) =>
        nonEmpty(StringResult(addToListCount(formComponentId, recData).stringRepresentation(typeInfo)))
      case AuthCtx(value: AuthInfo) =>
        nonEmpty(StringResult(AuthContextPrepop.values(value, evaluationContext.retrievals)))
      case UserCtx(value: UserField) =>
        nonEmpty(
          StringResult(
            UserCtxEvaluatorProcessor
              .processEvaluation(evaluationContext.retrievals, value, evaluationContext.authConfig)
          )
        )
      case Constant(value: String) => nonEmpty(StringResult(value))
      case HmrcRosmRegistrationCheck(value: RosmProp) =>
        nonEmpty(StringResult(UserCtxEvaluatorProcessor.evalRosm(evaluationContext.thirdPartyData, value)))
      case Value => Empty
      case FormTemplateCtx(value: FormTemplateProp) =>
        nonEmpty {
          value match {
            case FormTemplateProp.Id                  => StringResult(evaluationContext.formTemplateId.value)
            case FormTemplateProp.SubmissionReference => StringResult(evaluationContext.submissionRef.value)
          }
        }

      case ParamCtx(queryParam) =>
        nonEmpty(StringResult(evaluationContext.thirdPartyData.queryParams(queryParam)))
      case LinkCtx(internalLink) =>
        val link =
          internalLink match {
            case InternalLink.PrintSummaryPdf =>
              uk.gov.hmrc.gform.gform.routes.SummaryController
                .downloadPDF(evaluationContext.formTemplateId, evaluationContext.maybeAccessCode)
            case InternalLink.PrintAcknowledgementPdf =>
              uk.gov.hmrc.gform.gform.routes.AcknowledgementController
                .downloadPDF(evaluationContext.maybeAccessCode, evaluationContext.formTemplateId)

          }
        nonEmpty(StringResult(link.url))
      case DateCtx(dateExpr) =>
        StringResult(evalDateExpr(recData, evaluationContext, this)(dateExpr).stringRepresentation(typeInfo))
      case Period(_, _) =>
        StringResult(evalPeriod(typeInfo, recData, evaluationContext).stringRepresentation(typeInfo))
      case PeriodExt(_, _) =>
        StringResult(evalPeriod(typeInfo, recData, evaluationContext).stringRepresentation(typeInfo))
      case AddressLens(formComponentId, details) =>
        whenVisible(formComponentId) {
          val atomic: ModelComponentId.Atomic =
            formComponentId.modelComponentId.toAtomicFormComponentId(
              if (evaluationContext.addressLookup(formComponentId.baseComponentId))
                details.toAddressAtom
              else
                details.toOverseasAddressAtom
            )
          recData.variadicFormData
            .get(atomic)
            .collectFirst {
              case VariadicValue.One(value) if value.nonEmpty =>
                ExpressionResult.StringResult(value)
            }
            .getOrElse(ExpressionResult.empty)
        }
    }

    loop(typeInfo.expr)
  }

  private def evalDateString(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  ): ExpressionResult = {

    def loop(expr: Expr): ExpressionResult = expr match {
      case ctx @ FormCtx(_) =>
        evalDateExpr(recData, evaluationContext, this)(DateFormCtxVar(ctx))
      case Else(field1: Expr, field2: Expr) =>
        loop(field1) orElse loop(field2)
      case DateCtx(dateExpr) =>
        evalDateExpr(recData, evaluationContext, this)(dateExpr)
      case _ => ExpressionResult.empty
    }

    loop(typeInfo.expr) orElse evalString(typeInfo, recData, evaluationContext)
  }

  private def evalPeriod(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  ): ExpressionResult = {
    def loop(expr: Expr): ExpressionResult = expr match {
      case Add(field1: Expr, field2: Expr) =>
        loop(field1) + loop(field2)
      case Else(field1: Expr, field2: Expr) =>
        loop(field1) orElse loop(field2)
      case PeriodValue(value) => PeriodResult(java.time.Period.parse(value))
      case Period(DateCtx(dateExpr1), DateCtx(dateExpr2)) =>
        periodBetween(recData, evaluationContext)(dateExpr1, dateExpr2)
      case PeriodExt(Period(DateCtx(dateExpr1), DateCtx(dateExpr2)), prop) =>
        def doSum(mapper: PeriodResult => ExpressionResult): ExpressionResult =
          dateExpr1.maybeFormCtx.orElse(dateExpr2.maybeFormCtx).fold(ExpressionResult.empty) { formCtx =>
            val modelComponentIds = recData.variadicFormData
              .forBaseComponentId(formCtx.formComponentId.baseComponentId)
              .map { case (id, _) =>
                id
              }
            val indexedCompExists = modelComponentIds.exists(_.indexedComponentId.fold(_ => false)(_ => true))
            val periodFunctionExprs = if (indexedCompExists) {
              modelComponentIds
                .flatMap(_.maybeIndex)
                .toList
                .distinct
                .map(index => Period(DateCtx(dateExpr1.expand(index)), DateCtx(dateExpr2.expand(index))))
            } else {
              List(Period(DateCtx(dateExpr1), DateCtx(dateExpr2)))
            }
            periodFunctionExprs
              .map(p =>
                evalPeriod(
                  typeInfo.copy(expr = p),
                  recData,
                  evaluationContext
                )
              )
              .reduce(_ + _)
              .fold[ExpressionResult](identity)(identity)(identity)(identity)(identity)(identity)(identity)(
                identity
              )(mapper)
          }
        prop match {
          case PeriodFn.Sum => doSum(identity)
          case PeriodFn.TotalMonths =>
            doSum(p => NumberResult(p.value.toTotalMonths))
          case PeriodFn.Years =>
            doSum(p => NumberResult(p.value.getYears))
          case PeriodFn.Months =>
            doSum(p => NumberResult(p.value.getMonths))
          case PeriodFn.Days =>
            doSum(p => NumberResult(p.value.getDays))
        }
      case _ => ExpressionResult.empty
    }
    loop(typeInfo.expr)
  }

  private def periodBetween(
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  )(dateExpr1: DateExpr, dateExpr2: DateExpr): ExpressionResult = {
    val dateResult1 = evalDateExpr(recData, evaluationContext, this)(dateExpr1)
    val dateResult2 = evalDateExpr(recData, evaluationContext, this)(dateExpr2)
    (dateResult1, dateResult2) match {
      case (DateResult(value1), DateResult(value2)) => PeriodResult(java.time.Period.between(value1, value2))
      case _                                        => ExpressionResult.empty
    }
  }

  def evalExprCurrent(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.Current],
    evaluationContext: EvaluationContext
  ): ExpressionResult = evalExpr(typeInfo, recData.asInstanceOf[RecData[SourceOrigin.OutOfDate]], evaluationContext)

  def evalExpr(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  ): ExpressionResult =
    typeInfo.staticTypeData.exprType.fold { number =>
      evalNumber(typeInfo, recData, evaluationContext)
    } { string =>
      evalString(typeInfo, recData, evaluationContext)
    } { choiceSelection =>
      evalString(typeInfo, recData, evaluationContext)
    } { dateString =>
      evalDateString(typeInfo, recData, evaluationContext)
    } { addressString =>
      evalString(typeInfo, recData, evaluationContext)
    } { period =>
      evalPeriod(typeInfo, recData, evaluationContext)
    } { illegal =>
      ExpressionResult.invalid("[evalTyped] Illegal expression " + typeInfo.expr)
    }
}

object EvaluationResults {
  val empty = EvaluationResults(Map.empty)

  def one(expr: Expr, result: ExpressionResult): EvaluationResults = empty.+(expr, result)

  def unapply(a: EvaluationResults): Option[Map[Expr, ExpressionResult]] = Some(a.exprMap)

  implicit val monoidEvaluationResults: Monoid[EvaluationResults] = new Monoid[EvaluationResults] {
    def empty = EvaluationResults.empty
    def combine(l: EvaluationResults, r: EvaluationResults): EvaluationResults = (l, r) match {
      case (EvaluationResults(em1), EvaluationResults(em2)) => EvaluationResults(em1 ++ em2)
    }
  }
}
