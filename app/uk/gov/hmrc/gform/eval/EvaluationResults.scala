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

import cats.Monoid
import cats.syntax.eq._
import play.api.i18n.Messages

import scala.util.Try
import uk.gov.hmrc.gform.commons.BigDecimalUtil.toBigDecimalSafe
import uk.gov.hmrc.gform.commons.NumberSetScale
import uk.gov.hmrc.gform.eval.DateExprEval.evalDateExpr
import uk.gov.hmrc.gform.gform.AuthContextPrepop
import uk.gov.hmrc.gform.gform.{ Substituter, SummarySubstituter, SummarySubstitutions }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.graph.processor.UserCtxEvaluatorProcessor
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.ids.ModelComponentId.Atomic
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString, SourceOrigin, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.getMonthValue
import uk.gov.hmrc.gform.lookup.{ LookupLabel, LookupOptions }
import uk.gov.hmrc.gform.lookup.LocalisedLookupOptions
import uk.gov.hmrc.gform.models.ids.IndexedComponentId
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl

import SummarySubstituter._

case class EvaluationResults(
  exprMap: Map[Expr, ExpressionResult],
  recData: RecData[SourceOrigin.Current],
  repeatedComponentsDetails: RepeatedComponentsDetails
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

  private def isPureAndRefereceIndexed(
    modelComponentId: ModelComponentId,
    evaluationContext: EvaluationContext
  ): Boolean = {
    val isModelFormComponentIdPure = modelComponentId.indexedComponentId.isPure
    val isReferenceIndexed = evaluationContext.indexedComponentIds.exists(
      _.baseComponentId === modelComponentId.baseComponentId
    )
    isModelFormComponentIdPure && isReferenceIndexed
  }

  private def get(
    expr: FormCtx,
    fromVariadicValue: VariadicValue => ExpressionResult,
    evaluationContext: EvaluationContext
  ): ExpressionResult = {
    val modelComponentId = expr.formComponentId.modelComponentId
    if (isPureAndRefereceIndexed(modelComponentId, evaluationContext)) {
      ListResult(
        exprMap
          .collect {
            case (FormCtx(c), r)
                if c.baseComponentId === modelComponentId.baseComponentId && c.modelComponentId.maybeIndex.nonEmpty =>
              (c.modelComponentId, r)
          }
          .toList
          .sortBy { case (id, _) =>
            id.maybeIndex
          }
          .map(_._2)
      )
    } else {
      val expressionResult = exprMap.getOrElse(
        expr,
        recData.variadicFormData
          .get(modelComponentId)
          .fold(ExpressionResult.empty)(fromVariadicValue)
      )
      if (evaluationContext.fileIdsWithMapping.isFileField(modelComponentId))
        stripFileName(expressionResult, modelComponentId, evaluationContext.fileIdsWithMapping.mapping)
      else
        expressionResult
    }
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

  private def addToListCount(
    formComponentId: FormComponentId,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  ): NumberResult =
    if (evaluationContext.addToListIds.contains(AddToListId(formComponentId))) {
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
    } else {
      val xs: Iterable[(ModelComponentId, VariadicValue)] =
        recData.variadicFormData
          .forBaseComponentId(formComponentId.baseComponentId)
          .filterNot { case (baseId, _) =>
            exprMap.get(FormCtx(baseId.toFormComponentId)).contains(Hidden)
          }

      NumberResult(xs.size)
    }

  private def addToListValues(
    formComponentId: FormComponentId,
    recData: RecData[SourceOrigin.OutOfDate]
  ): List[String] = {
    val firstQuestionFcId = formComponentId.withFirstIndex
    val isHidden = exprMap.get(FormCtx(firstQuestionFcId))
    val allValues = recData.variadicFormData.forBaseComponentId(formComponentId.baseComponentId)
    if (isHidden.contains(Hidden)) {
      List.empty[String]
    } else {
      allValues.map(_._2).collect { case VariadicValue.One(str) => str }.toList
    }
  }

  private def evalSize(
    formComponentId: FormComponentId,
    recData: RecData[SourceOrigin.OutOfDate],
    index: SizeRefType
  ): ExpressionResult = {
    val xs: Iterable[(ModelComponentId, VariadicValue)] =
      recData.variadicFormData.forBaseComponentIdLessThen(formComponentId.modelComponentId)

    val indexString = index match {
      case SizeRefType.IndexBased(index) => index.toString
      case SizeRefType.ValueBased(value) => value
    }

    val size: Int = xs.map(_._2).count(_.contains(indexString))

    NumberResult(size)
  }

  private def getDataRetrieveCount(
    evaluationContext: EvaluationContext,
    dataRetrieveCount: DataRetrieveCount
  ): Option[Int] =
    for {
      dataRetrieve <- evaluationContext.thirdPartyData.dataRetrieve
      result       <- dataRetrieve.get(dataRetrieveCount.id)
    } yield result.data.size

  private def evalNumber(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext
  ): ExpressionResult = {

    implicit val m = evaluationContext.messages

    def fromVariadicValue(variadicValue: VariadicValue): ExpressionResult =
      variadicValue.fold(one => toNumberResult(one.value))(unsupportedMany("Number"))

    def toNumberResult(value: String): ExpressionResult =
      toBigDecimalSafe(value).fold(ExpressionResult.invalid(s"Number - cannot convert '$value' to number"))(
        NumberResult.apply
      )

    def loop(expr: Expr): ExpressionResult =
      expr match {
        case Add(field1: Expr, field2: Expr)         => loop(field1) + loop(field2)
        case Multiply(field1: Expr, field2: Expr)    => loop(field1) * loop(field2)
        case Subtraction(field1: Expr, field2: Expr) => loop(field1) - loop(field2)
        case Divide(field1: Expr, field2: Expr)      => loop(field1) / loop(field2)
        case HideZeroDecimals(field1: Expr) =>
          loop(field1).numberRepresentation match {
            case Some(value) =>
              if (value.isValidInt) NumberResult(value.intValue).hideZeroDecimals() else NumberResult(value)
            case None => unsupportedOperation("Number")(expr)
          }
        case IfElse(cond, field1: Expr, field2: Expr) =>
          if (booleanExprResolver.resolve(cond)) loop(field1) else loop(field2)
        case Else(field1: Expr, field2: Expr) => loop(field1) orElse loop(field2)
        case ctx @ FormCtx(formComponentId)   => get(ctx, fromVariadicValue, evaluationContext)
        case Sum(_) =>
          val substitutions = SummarySubstitutions(exprMap, repeatedComponentsDetails)
          loop(implicitly[Substituter[SummarySubstitutions, Expr]].substitute(substitutions, expr))
        case Count(formComponentId)   => addToListCount(formComponentId, recData, evaluationContext)
        case AuthCtx(value: AuthInfo) => unsupportedOperation("Number")(expr)
        case UserCtx(value: UserField) =>
          value.fold(_ => unsupportedOperation("Number")(expr))(enrolment =>
            toNumberResult(
              UserCtxEvaluatorProcessor
                .processEvaluation(evaluationContext.retrievals, enrolment, evaluationContext.authConfig)
            )
          )(_ => unsupportedOperation("Number")(expr))
        case Constant(value: String)                  => toNumberResult(value)
        case Value                                    => Empty
        case FormTemplateCtx(value: FormTemplateProp) => unsupportedOperation("Number")(expr)
        case ParamCtx(queryParam)                     => toNumberResult(evaluationContext.thirdPartyData.queryParams(queryParam))
        case LinkCtx(_)                               => unsupportedOperation("Number")(expr)
        case LangCtx                                  => unsupportedOperation("Number")(expr)
        case DateCtx(_)                               => unsupportedOperation("Number")(expr)
        case DateFunction(dateFunc) =>
          evalDateExpr(recData, evaluationContext, this, booleanExprResolver)(dateFunc.dateExpr) match {
            case ExpressionResult.DateResult(localDate) => ExpressionResult.NumberResult(dateFunc.toValue(localDate))
            case otherwise                              => otherwise
          }
        case Period(_, _)      => unsupportedOperation("Number")(expr)
        case PeriodExt(_, _)   => evalPeriod(typeInfo, recData, booleanExprResolver, evaluationContext)
        case PeriodValue(_)    => unsupportedOperation("Number")(expr)
        case AddressLens(_, _) => unsupportedOperation("Number")(expr)
        case d @ DataRetrieveCtx(_, _) =>
          evaluationContext.thirdPartyData.dataRetrieve
            .fold(Option.empty[ExpressionResult]) { dataRetrieve =>
              DataRetrieveEval
                .getDataRetrieveAttribute(dataRetrieve, d)
                .map {
                  case s :: Nil => toNumberResult(s)
                  case xs       => ListResult(xs.map(toNumberResult))
                }
            }
            .getOrElse(unsupportedOperation("Number")(expr))
        case d @ DataRetrieveCount(_) =>
          val count = getDataRetrieveCount(evaluationContext, d).getOrElse(0)
          NumberResult(count)
        case CsvCountryCheck(_, _)         => unsupportedOperation("Number")(expr)
        case CsvOverseasCountryCheck(_, _) => unsupportedOperation("Number")(expr)
        case CsvCountryCountCheck(fcId, column, value) =>
          val count = addToListValues(fcId, recData)
            .filter(str =>
              countryLookup(str, column, evaluationContext.lookupOptions, evaluationContext.lang) match {
                case StringResult(r) => r == value
                case _               => false
              }
            )
            .size
          NumberResult(count)
        case Size(formComponentId, index) => evalSize(formComponentId, recData, index)
        case Typed(expr, tpe)             => evalTyped(loop(expr), tpe)
        case IndexOf(fcId, index) =>
          loop(FormCtx(fcId)) match {
            case ListResult(xs) => Try(xs(index)).getOrElse(Empty)
            case _              => unsupportedOperation("Number")(expr)
          }
        case IndexOfDataRetrieveCtx(ctx, index) =>
          loop(ctx) match {
            case ListResult(xs) => Try(xs(index)).getOrElse(Empty)
            case _              => unsupportedOperation("Number")(expr)
          }
        case NumberedList(_)         => unsupportedOperation("Number")(expr)
        case BulletedList(_)         => unsupportedOperation("Number")(expr)
        case StringOps(_, _)         => unsupportedOperation("Number")(expr)
        case Concat(_)               => unsupportedOperation("Number")(expr)
        case CountryOfItmpAddress    => unsupportedOperation("Number")(expr)
        case ChoicesRevealedField(_) => unsupportedOperation("Number")(expr)
        case ChoiceLabel(_)          => unsupportedOperation("Number")(expr)
      }

    loop(typeInfo.expr)
  }

  def evalTyped(er: ExpressionResult, tpe: ExplicitExprType): ExpressionResult =
    tpe match {
      case ExplicitExprType.Sterling(roundingMode) =>
        er.withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, 2, roundingMode))
      case _ => er
    }

  def evalTaxPeriodYear(
    componentId: FormComponentId,
    recData: RecData[SourceOrigin.OutOfDate],
    messages: Messages
  ): ExpressionResult = {
    val monthsValidationRegex = "0?\\d|1[012]".r
    val yearAtom = Atomic(componentId.modelComponentId.indexedComponentId, TaxPeriodDate.year)
    val monthAtom = Atomic(componentId.modelComponentId.indexedComponentId, TaxPeriodDate.month)

    val monthKey = recData.variadicFormData
      .one(monthAtom)
      .filter {
        case monthsValidationRegex() => true
        case _                       => false
      }
      .map(getMonthValue)
      .getOrElse("")

    val monthAsText = messages(s"date.$monthKey")
    val year = recData.variadicFormData.one(yearAtom).getOrElse("")

    StringResult(s"$monthAsText $year")
  }

  private def evalString(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext
  ): ExpressionResult = {
    implicit val m = evaluationContext.messages

    def nonEmptyStringResult(stringResult: StringResult): ExpressionResult =
      if (stringResult.value.trim.isEmpty) Empty else stringResult

    def nonEmptyExpressionResult(exprResult: ExpressionResult): ExpressionResult =
      exprResult match {
        case sr @ StringResult(_)        => nonEmptyStringResult(sr)
        case ar @ AddressResult(address) => if (address.isEmpty) Empty else ar
        case otherwise                   => otherwise
      }

    def fromVariadicValue(variadicValue: VariadicValue): ExpressionResult =
      variadicValue.fold[ExpressionResult](one => nonEmptyStringResult(StringResult(one.value)))(many =>
        ExpressionResult.OptionResult(many.value)
      )

    def getAddressResult(indexedComponentId: IndexedComponentId): AddressResult = {
      val addressAtoms: List[ModelComponentId.Atomic] =
        if (evaluationContext.addressLookup(indexedComponentId.baseComponentId))
          Address.fields(indexedComponentId).filter(_.atom != Address.uk)
        else if (evaluationContext.overseasAddressLookup(indexedComponentId.baseComponentId))
          OverseasAddress.fields(indexedComponentId).toList
        else
          PostcodeLookup.fields(indexedComponentId).toList

      val variadicValues: List[Option[VariadicValue]] =
        addressAtoms.map(atom => recData.variadicFormData.get(atom))
      val addressLines = variadicValues.collect { case Some(VariadicValue.One(value)) if value.nonEmpty => value }
      ExpressionResult.AddressResult(addressLines)
    }

    def loop(expr: Expr): ExpressionResult = {
      expr match {
        case Add(field1: Expr, field2: Expr)         => loop(field1) + loop(field2)
        case Multiply(field1: Expr, field2: Expr)    => unsupportedOperation("String")(expr)
        case Subtraction(field1: Expr, field2: Expr) => unsupportedOperation("String")(expr)
        case Divide(field1: Expr, field2: Expr)      => unsupportedOperation("String")(expr)
        case HideZeroDecimals(field1)                => loop(field1)
        case IfElse(cond, field1: Expr, field2: Expr) =>
          if (booleanExprResolver.resolve(cond)) loop(field1) else loop(field2)
        case Else(field1: Expr, field2: Expr) => loop(field1) orElse loop(field2)
        case FormCtx(formComponentId: FormComponentId)
            if evaluationContext.addressLookup(formComponentId.baseComponentId) || evaluationContext
              .overseasAddressLookup(formComponentId.baseComponentId) || evaluationContext
              .addressLookup(formComponentId.baseComponentId) =>
          whenVisible(formComponentId) {
            val modelComponentId = formComponentId.modelComponentId
            if (isPureAndRefereceIndexed(modelComponentId, evaluationContext)) {
              val addresses =
                recData.variadicFormData.distinctIndexedComponentIds(formComponentId.modelComponentId).map {
                  indexedComponentId =>
                    getAddressResult(indexedComponentId)
                }
              ListResult(addresses)
            } else {
              val indexedComponentId = formComponentId.modelComponentId.indexedComponentId
              getAddressResult(indexedComponentId)
            }
          }
        case FormCtx(formComponentId: FormComponentId)
            if evaluationContext.postcodeLookup(formComponentId.baseComponentId) =>
          whenVisible(formComponentId) {
            ExpressionResult.StringResult(
              evaluationContext.thirdPartyData.addressLines(formComponentId).fold("")(_.mkString(", "))
            )
          }
        case FormCtx(formComponentId: FormComponentId)
            if evaluationContext.taxPeriodYear(formComponentId.baseComponentId) =>
          whenVisible(formComponentId) {
            evalTaxPeriodYear(formComponentId, recData, evaluationContext.messages)
          }
        case ctx @ FormCtx(formComponentId: FormComponentId) =>
          get(ctx, fromVariadicValue, evaluationContext)
        case Sum(field1: Expr) => unsupportedOperation("String")(expr)
        case Count(formComponentId) =>
          nonEmptyStringResult(
            StringResult(
              addToListCount(formComponentId, recData, evaluationContext)
                .stringRepresentation(typeInfo, evaluationContext.messages)
            )
          )
        case AuthCtx(value: AuthInfo) =>
          nonEmptyExpressionResult(
            AuthContextPrepop
              .values(value, evaluationContext.retrievals, evaluationContext.thirdPartyData.itmpRetrievals)
          )
        case UserCtx(value: UserField) =>
          nonEmptyStringResult(
            StringResult(
              UserCtxEvaluatorProcessor
                .processEvaluation(evaluationContext.retrievals, value, evaluationContext.authConfig)
            )
          )
        case Constant(value: String) => nonEmptyStringResult(StringResult(value))
        case Value                   => Empty
        case FormTemplateCtx(value: FormTemplateProp) =>
          nonEmptyStringResult {
            value match {
              case FormTemplateProp.Id                  => StringResult(evaluationContext.formTemplateId.value)
              case FormTemplateProp.SubmissionReference => StringResult(evaluationContext.submissionRef.value)
              case FormTemplateProp.FileSizeLimit       => StringResult(evaluationContext.fileSizeLimit.value.toString)
            }
          }

        case ParamCtx(queryParam) =>
          nonEmptyStringResult(StringResult(evaluationContext.thirdPartyData.queryParams(queryParam)))
        case LinkCtx(internalLink) =>
          val link =
            internalLink match {
              case InternalLink.PrintSummaryPdf =>
                uk.gov.hmrc.gform.gform.routes.SummaryController
                  .downloadPDF(evaluationContext.formTemplateId, evaluationContext.maybeAccessCode)
                  .url
              case InternalLink.PrintAcknowledgementPdf =>
                uk.gov.hmrc.gform.gform.routes.AcknowledgementController
                  .downloadPDF(evaluationContext.maybeAccessCode, evaluationContext.formTemplateId)
                  .url
              case InternalLink.NewFormForTemplate(formTemplateId) =>
                uk.gov.hmrc.gform.gform.routes.NewFormController
                  .dashboardClean(formTemplateId)
                  .url
              case InternalLink.NewForm =>
                uk.gov.hmrc.gform.gform.routes.NewFormController
                  .dashboardClean(evaluationContext.formTemplateId)
                  .url
              case InternalLink.NewSession =>
                uk.gov.hmrc.gform.gform.routes.NewFormController
                  .dashboardWithNewSession(evaluationContext.formTemplateId)
                  .url
              case InternalLink.SignOut =>
                uk.gov.hmrc.gform.gform.routes.SignOutController
                  .signOut(evaluationContext.formTemplateId)
                  .url
              case PageLink(id) =>
                computePageLink(id, evaluationContext)
              case InternalLink.Download(fileName) =>
                uk.gov.hmrc.gform.gform.routes.DownloadController
                  .downloadFile(
                    evaluationContext.formTemplateId,
                    evaluationContext.maybeAccessCode,
                    fileName
                  )
                  .url
              case InternalLink.Image(fileName) =>
                uk.gov.hmrc.gform.gform.routes.ImageController
                  .image(
                    evaluationContext.formTemplateId,
                    evaluationContext.maybeAccessCode,
                    fileName
                  )
                  .url
              case InternalLink.UrlLink(url) =>
                uk.gov.hmrc.gform.gform.routes.RedirectController.redirect(RedirectUrl(url)).url
            }
          nonEmptyStringResult(StringResult(link))
        case DateCtx(dateExpr) => evalDateExpr(recData, evaluationContext, this, booleanExprResolver)(dateExpr)
        case DateFunction(dateFunc) =>
          evalDateExpr(recData, evaluationContext, this, booleanExprResolver)(dateFunc.dateExpr) match {
            case ExpressionResult.DateResult(localDate) =>
              ExpressionResult.StringResult(dateFunc.toValue(localDate).toString)
            case otherwise => otherwise
          }
        case Period(_, _)    => evalPeriod(typeInfo, recData, booleanExprResolver, evaluationContext)
        case PeriodExt(_, _) => evalPeriod(typeInfo, recData, booleanExprResolver, evaluationContext)
        case AddressLens(formComponentId, details) =>
          whenVisible(formComponentId) {
            val atomic: ModelComponentId.Atomic =
              formComponentId.modelComponentId.toAtomicFormComponentId(
                if (evaluationContext.addressLookup(formComponentId.baseComponentId))
                  details.toAddressAtom
                else if (evaluationContext.overseasAddressLookup(formComponentId.baseComponentId))
                  details.toOverseasAddressAtom
                else
                  PostcodeLookup.postcode
              )
            recData.variadicFormData
              .get(atomic)
              .collectFirst {
                case VariadicValue.One(value) if value.nonEmpty =>
                  ExpressionResult.StringResult(value)
              }
              .getOrElse(ExpressionResult.empty)
          }
        case LangCtx => StringResult(evaluationContext.lang.langADTToString)
        case d @ DataRetrieveCtx(_, _) =>
          val exprResult =
            evaluationContext.thirdPartyData.dataRetrieve.fold(ExpressionResult.empty) { dr =>
              DataRetrieveEval.getDataRetrieveAttribute(dr, d) match {
                case Some(h :: Nil) => StringResult(h)
                case Some(xs)       => ListResult(xs.map(StringResult))
                case None           => Empty
              }
            }
          nonEmptyExpressionResult(exprResult)

        case d @ DataRetrieveCount(_) =>
          val count = getDataRetrieveCount(evaluationContext, d).getOrElse(0)
          StringResult(count.toString)

        case Size(formComponentId, index) => evalSize(formComponentId, recData, index)
        case Typed(expr, tpe) =>
          (expr, tpe) match {
            case (Constant(value), ExplicitExprType.Sterling(_)) =>
              nonEmptyStringResult(StringResult(TextFormatter.formatSterling(value)))
            case (Constant(value), ExplicitExprType.Number(fractionalDigits, roundingMode)) =>
              nonEmptyStringResult(
                StringResult(TextFormatter.formatNumberWithPrecise(value, fractionalDigits, roundingMode))
              )
            case _ => loop(expr)
          }
        case CsvCountryCheck(fcId, column) =>
          loop(FormCtx(fcId)) match {
            case StringResult(value) =>
              countryLookup(value, column, evaluationContext.lookupOptions, evaluationContext.lang)
            case _ => Empty
          }

        case CsvOverseasCountryCheck(fcId, column) if evaluationContext.overseasAddressLookup(fcId.baseComponentId) =>
          whenVisible(fcId) {
            val indexedComponentId = fcId.modelComponentId.indexedComponentId
            val addressAtoms: List[ModelComponentId.Atomic] = OverseasAddress.fields(indexedComponentId).toList
            val variadicValues: List[Option[VariadicValue]] =
              addressAtoms.filter(_.atom == OverseasAddress.country).map(atom => recData.variadicFormData.get(atom))
            variadicValues
              .collect { case Some(VariadicValue.One(value)) if value.nonEmpty => value }
              .headOption
              .map(country => countryLookup(country, column, evaluationContext.lookupOptions, evaluationContext.lang))
              .getOrElse(Empty)
          }
        case CsvOverseasCountryCheck(fcId, column) => Empty
        case CsvCountryCountCheck(fcId, column, value) =>
          val count = addToListValues(fcId, recData)
            .filter(str =>
              countryLookup(str, column, evaluationContext.lookupOptions, evaluationContext.lang) match {
                case StringResult(r) => r == value
                case _               => false
              }
            )
            .size
          StringResult(s"$count")
        case IndexOf(fcId, index) =>
          loop(FormCtx(fcId)) match {
            case ListResult(xs) => Try(xs(index)).getOrElse(Empty)
            case _              => unsupportedOperation("String")(expr)
          }
        case IndexOfDataRetrieveCtx(ctx, index) =>
          loop(ctx) match {
            case ListResult(xs) => Try(xs(index)).getOrElse(Empty)
            case _              => unsupportedOperation("String")(expr)
          }
        case NumberedList(fcId)         => loop(FormCtx(fcId))
        case BulletedList(fcId)         => loop(FormCtx(fcId))
        case StringOps(expr, stringFnc) =>
          val str = loop(expr).withStringResult("")(s =>
            stringFnc match {
              case StringFnc.Capitalize    => s.capitalize
              case StringFnc.CapitalizeAll => s.split(' ').map(_.capitalize).mkString(" ")
              case StringFnc.LowerCase     => s.toLowerCase
              case StringFnc.UpperCase     => s.toUpperCase
              case StringFnc.RemoveSpaces  => s.replaceAll(" ", "")
              case StringFnc.SubString(beginIndex, endIndex) =>
                s.substring(Math.min(beginIndex, s.length), Math.min(endIndex, s.length))
            }
          )
          StringResult(str)
//          }
        case Concat(exprs) => evalConcat(exprs, recData, booleanExprResolver, evaluationContext)
        case CountryOfItmpAddress =>
          val itmpRetrievals = evaluationContext.thirdPartyData.itmpRetrievals
          nonEmptyExpressionResult(
            StringResult(itmpRetrievals.flatMap(_.itmpAddress).flatMap(_.countryName).getOrElse(""))
          )
        case ChoicesRevealedField(fcId) => loop(FormCtx(fcId))
        case ChoiceLabel(fcId)          => evalChoiceLabel(evaluationContext, fcId, booleanExprResolver, recData)
        case _                          => unsupportedOperation("String")(expr)
      }
    }

    loop(typeInfo.expr)
  }

  private def computePageLink(forPageId: PageId, evaluationContext: EvaluationContext) = {
    val forModelPageId = forPageId.modelPageId
    evaluationContext.pageIdSectionNumberMap.get(forModelPageId) match {
      case Some(sectionNumber) =>
        uk.gov.hmrc.gform.gform.routes.FormController
          .formSection(evaluationContext.formTemplateId, evaluationContext.maybeAccessCode, sectionNumber)
          .url
      case None =>
        evaluationContext.pageIdSectionNumberMap.toList
          .sortBy(_._1.maybeIndex)(Ordering[Option[Int]].reverse)
          .find { case (modelPageId, _) =>
            modelPageId.baseId == forModelPageId.baseId
          }
          .fold("") { case (_, sectionNumber) =>
            uk.gov.hmrc.gform.gform.routes.FormController
              .formSection(evaluationContext.formTemplateId, evaluationContext.maybeAccessCode, sectionNumber)
              .url
          }
    }
  }

  private def evalDateString(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext
  ): ExpressionResult = {

    def loop(expr: Expr): ExpressionResult = expr match {
      case ctx @ FormCtx(_) => evalDateExpr(recData, evaluationContext, this, booleanExprResolver)(DateFormCtxVar(ctx))
      case IfElse(cond, field1: Expr, field2: Expr) =>
        if (booleanExprResolver.resolve(cond)) loop(field1) else loop(field2)
      case Else(field1: Expr, field2: Expr) => loop(field1) orElse loop(field2)
      case DateCtx(dateExpr)                => evalDateExpr(recData, evaluationContext, this, booleanExprResolver)(dateExpr)
      case _                                => ExpressionResult.empty
    }

    loop(typeInfo.expr) orElse evalString(typeInfo, recData, booleanExprResolver, evaluationContext)
  }

  private def evalPeriod(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext
  ): ExpressionResult = {

    implicit val m = evaluationContext.messages

    def loop(expr: Expr): ExpressionResult = expr match {
      case Add(field1: Expr, field2: Expr) => loop(field1) + loop(field2)
      case IfElse(cond, field1: Expr, field2: Expr) =>
        if (booleanExprResolver.resolve(cond)) loop(field1) else loop(field2)
      case Else(field1: Expr, field2: Expr) => loop(field1) orElse loop(field2)
      case PeriodValue(value)               => PeriodResult(java.time.Period.parse(value))
      case Period(DateCtx(dateExpr1), DateCtx(dateExpr2)) =>
        periodBetween(recData, evaluationContext, booleanExprResolver)(dateExpr1, dateExpr2)
      case PeriodExt(Period(DateCtx(dateExpr1), DateCtx(dateExpr2)), prop) =>
        def doSum(mapper: PeriodResult => ExpressionResult): ExpressionResult =
          dateExpr1
            .maybeFormCtx(recData, evaluationContext, this, booleanExprResolver)
            .orElse(dateExpr2.maybeFormCtx(recData, evaluationContext, this, booleanExprResolver))
            .fold(ExpressionResult.empty) { formCtx =>
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
                    booleanExprResolver,
                    evaluationContext
                  )
                )
                .reduce(_ + _)
                .fold[ExpressionResult](identity)(identity)(identity)(identity)(identity)(identity)(identity)(identity)(
                  identity
                )(mapper)(identity)
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

  private def safeToInt(s: String): Option[Int] = Try(s.toInt).toOption

  private def evalTaxPeriod(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext
  ): ExpressionResult =
    typeInfo.expr match {
      case FormCtx(fcId) =>
        val maybeTaxPeriodResult = for {
          year  <- recData.variadicFormData.one(fcId.toAtomicFormComponentId(Date.year))
          month <- recData.variadicFormData.one(fcId.toAtomicFormComponentId(Date.month))
          y     <- safeToInt(year)
          m     <- safeToInt(month).flatMap(m => Try(java.time.Month.of(m)).toOption)
        } yield TaxPeriodResult(m, y)
        maybeTaxPeriodResult.getOrElse(ExpressionResult.empty)
      case _ => ExpressionResult.empty
    }

  private def periodBetween(
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext,
    booleanExprResolver: BooleanExprResolver
  )(dateExpr1: DateExpr, dateExpr2: DateExpr): ExpressionResult = {
    val dateResult1 = evalDateExpr(recData, evaluationContext, this, booleanExprResolver)(dateExpr1)
    val dateResult2 = evalDateExpr(recData, evaluationContext, this, booleanExprResolver)(dateExpr2)
    (dateResult1, dateResult2) match {
      case (DateResult(value1), DateResult(value2)) => PeriodResult(java.time.Period.between(value1, value2))
      case _                                        => ExpressionResult.empty
    }
  }

  private def countryLookup(
    value: String,
    column: String,
    lookupOptions: LocalisedLookupOptions,
    lang: LangADT
  ): ExpressionResult =
    lookupOptions.fold[ExpressionResult](Empty)(
      _.get(LookupLabel(value))
        .flatMap(li => LookupOptions.getLookupValue(li, column))
        .map(StringResult(_))
        .getOrElse(Empty)
    )(lang)

  def evalExprCurrent(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.Current],
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext
  ): ExpressionResult =
    evalExpr(typeInfo, recData.asInstanceOf[RecData[SourceOrigin.OutOfDate]], booleanExprResolver, evaluationContext)

  private def evalConcat(
    exprs: List[Expr],
    recData: RecData[SourceOrigin.OutOfDate],
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext
  ) = {
    val concatValue = exprs
      .map {
        case Constant(value) => value
        case expr =>
          val stringResult = evalExprAsString(expr, evaluationContext, booleanExprResolver, recData)

          expr match {
            case Typed(_, ExplicitExprType.Sterling(_)) =>
              TextFormatter.formatSterling(stringResult)
            case Typed(_, ExplicitExprType.Number(fractionalDigits, roundingMode)) =>
              TextFormatter.formatNumberWithPrecise(stringResult, fractionalDigits, roundingMode)
            case _ => stringResult
          }
      }
      .mkString("")

    StringResult(concatValue)
  }

  private def evalExprAsString(
    expr: Expr,
    evaluationContext: EvaluationContext,
    booleanExprResolver: BooleanExprResolver,
    recData: RecData[SourceOrigin.OutOfDate]
  ): String = {
    val typeInfo: TypeInfo = expr match {
      case Add(_, _) | Multiply(_, _) | Subtraction(_, _) | Divide(_, _) =>
        TypeInfo(expr, StaticTypeData(ExprType.number, Some(Number())))
      case DateCtx(_) => TypeInfo(expr, StaticTypeData(ExprType.dateString, None))
      case IsNumberConstant(_) | PeriodExt(_, _) | UserCtx(UserField.Enrolment(_, _, Some(UserFieldFunc.Count))) |
          Size(_, _) | CsvCountryCountCheck(_, _, _) =>
        TypeInfo(expr, StaticTypeData(ExprType.number, Some(Number())))
      case DataRetrieveCtx(id, attribute) if evaluationContext.dataRetrieveAll.isInteger(id, attribute) =>
        TypeInfo(expr, StaticTypeData(ExprType.number, Some(Number())))
      case DataRetrieveCount(_) =>
        TypeInfo(expr, StaticTypeData(ExprType.number, Some(Number())))
      case Period(_, _) | PeriodValue(_) => TypeInfo(expr, StaticTypeData(ExprType.period, None))
      case Typed(_, tpe)                 => TypeInfo(expr, StaticTypeData.from(tpe))
      case DateFunction(_)               => TypeInfo(expr, StaticTypeData(ExprType.number, None))
      case AuthCtx(AuthInfo.ItmpAddress) => TypeInfo(expr, StaticTypeData(ExprType.address, None))
      case _                             => TypeInfo(expr, StaticTypeData(ExprType.string, None))
    }
    evalExpr(
      typeInfo,
      recData,
      booleanExprResolver,
      evaluationContext
    ).stringRepresentation(typeInfo, evaluationContext.messages)
  }

  private def evalChoiceLabel(
    evaluationContext: EvaluationContext,
    fcId: FormComponentId,
    booleanExprResolver: BooleanExprResolver,
    recData: RecData[SourceOrigin.OutOfDate]
  ) = {
    val maybeChoiceM: Option[Map[String, SmartString]] = evaluationContext.choiceLookup
      .get(fcId.baseComponentId)
      .map(_.toList.zipWithIndex.collect {
        case (OptionData.IndexBased(label, _, _, _), i) => i.toString -> label
        case (OptionData.ValueBased(label, _, _, _, OptionDataValue.StringBased(value)), _) =>
          value -> label
        case (OptionData.ValueBased(label, _, _, _, OptionDataValue.ExprBased(prefix, expr)), _) =>
          prefix + evalExprAsString(expr, evaluationContext, booleanExprResolver, recData) -> label
        case (OptionData.ValueBased(label, _, _, _, OptionDataValue.FormCtxBased(formCtx)), _) =>
          evalExprAsString(formCtx, evaluationContext, booleanExprResolver, recData) -> label
      }.toMap)

    val result = recData.variadicFormData
      .many(fcId.modelComponentId)
      .map(
        _.map(value => maybeChoiceM.fold("")(choiceM => choiceM(value).rawValue(evaluationContext.lang)))
          .mkString("")
      )
      .getOrElse("")

    StringResult(result)
  }

  def evalExpr(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext
  ): ExpressionResult =
    typeInfo.staticTypeData.exprType.fold { number =>
      evalNumber(typeInfo, recData, booleanExprResolver, evaluationContext)
    } { string =>
      evalString(typeInfo, recData, booleanExprResolver, evaluationContext)
    } { choiceSelection =>
      evalString(typeInfo, recData, booleanExprResolver, evaluationContext)
    } { dateString =>
      evalDateString(typeInfo, recData, booleanExprResolver, evaluationContext)
    } { addressString =>
      evalString(typeInfo, recData, booleanExprResolver, evaluationContext)
    } { period =>
      evalPeriod(typeInfo, recData, booleanExprResolver, evaluationContext)
    } { taxPeriod =>
      evalTaxPeriod(typeInfo, recData, booleanExprResolver, evaluationContext)
    } { illegal =>
      ExpressionResult.invalid("[evalTyped] Illegal expression " + typeInfo.expr)
    }
}

object EvaluationResults {
  val empty = EvaluationResults(Map.empty, RecData.empty, RepeatedComponentsDetails.empty)

  def one(expr: Expr, result: ExpressionResult): EvaluationResults = empty.+(expr, result)

  def unapply(
    a: EvaluationResults
  ): Option[(Map[Expr, ExpressionResult], RecData[SourceOrigin.Current], RepeatedComponentsDetails)] =
    Some((a.exprMap, a.recData, a.repeatedComponentsDetails))

  implicit val monoidEvaluationResults: Monoid[EvaluationResults] = new Monoid[EvaluationResults] {
    def empty = EvaluationResults.empty
    def combine(l: EvaluationResults, r: EvaluationResults): EvaluationResults = (l, r) match {
      case (
            EvaluationResults(em1, rd1, RepeatedComponentsDetails(m1)),
            EvaluationResults(em2, rd2, RepeatedComponentsDetails(m2))
          ) =>
        EvaluationResults(
          em1 ++ em2,
          RecData.fromData(rd1.variadicFormData ++ rd2.variadicFormData),
          RepeatedComponentsDetails(m1 ++ m2)
        )
      case _ => throw new Exception("Invalid expression results for combine")
    }
  }
}
