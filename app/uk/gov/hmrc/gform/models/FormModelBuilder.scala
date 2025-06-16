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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.{ Functor, MonadError }
import play.api.i18n.Messages
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.{ AuthCache, CacheData }
import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.eval._
import uk.gov.hmrc.gform.gform.{ BooleanExprUpdater, FormComponentUpdater, PageUpdater }
import uk.gov.hmrc.gform.graph.{ RecData, Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelRenderPageOptics, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic.AddToListPage.TerminalPageKind
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{ Instant, LocalDate, ZoneId }
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import scala.util.matching.Regex

object FormModelBuilder {
  def fromCache[E, F[_]: Functor](
    cache: AuthCache,
    cacheData: CacheData,
    recalculation: Recalculation[F, E],
    componentIdToFileId: FormComponentIdToFileIdMapping,
    lookupRegistry: LookupRegistry,
    taskIdTaskStatus: TaskIdTaskStatusMapping
  )(implicit
    hc: HeaderCarrier,
    me: MonadError[F, E]
  ): FormModelBuilder[E, F] =
    new FormModelBuilder(
      cache.retrievals,
      cache.formTemplate,
      cacheData.thirdPartyData,
      cacheData.envelopeId,
      cache.accessCode,
      recalculation,
      componentIdToFileId,
      lookupRegistry,
      taskIdTaskStatus
    )

  def evalRemoveItemIf[T <: PageMode](
    removeItemIf: RemoveItemIf,
    recalculationResult: RecalculationResult,
    recData: RecData[SourceOrigin.Current],
    formModel: FormModel[T]
  ): Boolean = evalBooleanExpr[T](removeItemIf.booleanExpr, recalculationResult, recData, formModel, None)

  def evalIncludeIf[T <: PageMode](
    includeIf: IncludeIf,
    recalculationResult: RecalculationResult,
    recData: RecData[SourceOrigin.Current],
    formModel: FormModel[T],
    phase: Option[FormPhase]
  ): Boolean = evalBooleanExpr[T](includeIf.booleanExpr, recalculationResult, recData, formModel, phase)

  private def evalBooleanExpr[T <: PageMode](
    booleanExpr: BooleanExpr,
    recalculationResult: RecalculationResult,
    recData: RecData[SourceOrigin.Current],
    formModel: FormModel[T],
    phase: Option[FormPhase]
  ): Boolean = {

    def booleanExprResolver = BooleanExprResolver(loop)

    def compare(expr1: Expr, expr2: Expr, f: (ExpressionResult, ExpressionResult) => Boolean): Boolean = {
      val typeInfo1 = formModel.toFirstOperandTypeInfo(expr1)
      val typeInfo2 = formModel.toFirstOperandTypeInfo(expr2)
      val r = recalculationResult.evaluationResults
        .evalExprCurrent(typeInfo1, recData, booleanExprResolver, recalculationResult.evaluationContext)
        .applyTypeInfo(typeInfo1)
      val s = recalculationResult.evaluationResults
        .evalExprCurrent(typeInfo2, recData, booleanExprResolver, recalculationResult.evaluationContext)
        .applyTypeInfo(typeInfo2)

      f(r, s)
    }

    def compareDate(
      dateExprLHS: uk.gov.hmrc.gform.sharedmodel.formtemplate.DateExpr,
      dateExprRHS: uk.gov.hmrc.gform.sharedmodel.formtemplate.DateExpr,
      f: (DateResult, DateResult) => Boolean
    ): Boolean = {
      val evalFunc: uk.gov.hmrc.gform.sharedmodel.formtemplate.DateExpr => Option[DateResult] =
        DateExprEval
          .eval(
            formModel,
            recData.asInstanceOf[RecData[OutOfDate]],
            recalculationResult.evaluationContext,
            booleanExprResolver,
            recalculationResult.evaluationResults
          )
      val exprResultLHS = evalFunc(dateExprLHS)
      val exprResultRHS = evalFunc(dateExprRHS)
      (exprResultLHS, exprResultRHS) match {
        case (Some(left), Some(right)) => f(left, right)
        case _                         => false
      }
    }

    def matchRegex(expr: Expr, regex: Regex): Boolean = {
      val typeInfo1 = formModel.toFirstOperandTypeInfo(expr)
      val expressionResult = recalculationResult.evaluationResults
        .evalExprCurrent(typeInfo1, recData, booleanExprResolver, recalculationResult.evaluationContext)
        .applyTypeInfo(typeInfo1)

      expressionResult.matchRegex(regex)
    }

    def loop(booleanExpr: BooleanExpr): Boolean = booleanExpr match {
      case Equals(field1, field2)              => compare(field1, field2, _ identical _)
      case GreaterThan(field1, field2)         => compare(field1, field2, _ > _)
      case DateAfter(field1, field2)           => compareDate(field1, field2, _ after _)
      case GreaterThanOrEquals(field1, field2) => compare(field1, field2, _ >= _)
      case LessThan(field1, field2)            => compare(field1, field2, _ < _)
      case DateBefore(field1, field2)          => compareDate(field1, field2, _ before _)
      case LessThanOrEquals(field1, field2)    => compare(field1, field2, _ <= _)
      case Not(invertedExpr)                   => !loop(invertedExpr)
      case Or(expr1, expr2)                    => val e1 = loop(expr1); val e2 = loop(expr2); e1 | e2
      case And(expr1, expr2)                   => val e1 = loop(expr1); val e2 = loop(expr2); e1 & e2
      case IsTrue                              => true
      case IsFalse                             => false
      case Contains(field1, field2)            => compare(field1, field2, _ contains _)
      case in @ In(_, _)                       => BooleanExprEval.evalInExpr(in, formModel, recalculationResult, booleanExprResolver, recData)
      case h @ HasAnswer(_, _) =>
        BooleanExprEval.evalHasAnswer(
          h,
          formModel,
          recalculationResult.evaluationResults,
          recalculationResult.evaluationContext,
          booleanExprResolver,
          recData
        )
      case DuplicateExists(fieldList)      => BooleanExprEval.evalDuplicateExpr(fieldList, recData)
      case MatchRegex(expr, regex)         => matchRegex(expr, regex)
      case FormPhase(value)                => phase.fold(false)(_.value == value)
      case First(FormCtx(formComponentId)) => BooleanExprEval.evalFirstExpr(formComponentId)
      case IsLogin(value)                  => BooleanExprEval.evalIsLoginExpr(value, recalculationResult.evaluationContext.retrievals)
    }

    loop(booleanExpr)

  }

  private def toCurrentData(
    modelComponentId: ModelComponentId,
    expressionResult: ExpressionResult,
    typeInfo: TypeInfo
  )(implicit messages: Messages): VariadicFormData[SourceOrigin.Current] =
    expressionResult match {
      case ExpressionResult.Empty      => VariadicFormData.one[SourceOrigin.Current](modelComponentId, "")
      case ExpressionResult.Hidden     => VariadicFormData.empty[SourceOrigin.Current]
      case ExpressionResult.Invalid(_) => VariadicFormData.empty[SourceOrigin.Current]
      case ExpressionResult.NumberResult(bigDecimal) =>
        VariadicFormData.one[SourceOrigin.Current](modelComponentId, bigDecimal.toString)
      case ExpressionResult.StringResult(value) => VariadicFormData.one[SourceOrigin.Current](modelComponentId, value)
      case ExpressionResult.OptionResult(value) =>
        VariadicFormData.many[SourceOrigin.Current](modelComponentId, value.map(_.toString))
      case d @ ExpressionResult.DateResult(_) =>
        VariadicFormData.one[SourceOrigin.Current](modelComponentId, d.asString)
      case d @ ExpressionResult.TaxPeriodResult(_, _) =>
        VariadicFormData.one[SourceOrigin.Current](modelComponentId, d.asString)
      case p @ ExpressionResult.PeriodResult(_) =>
        VariadicFormData.one[SourceOrigin.Current](modelComponentId, p.asString)
      case a @ ExpressionResult.AddressResult(_) =>
        VariadicFormData.one[SourceOrigin.Current](modelComponentId, a.stringRepresentation(typeInfo, messages))
      case ExpressionResult.ListResult(list) =>
        list.foldLeft(VariadicFormData.empty[SourceOrigin.Current]) { case (acc, result) =>
          acc ++ toCurrentData(modelComponentId, result, typeInfo)
        }
    }

}

class FormModelBuilder[E, F[_]: Functor](
  retrievals: MaterialisedRetrievals,
  formTemplate: FormTemplate,
  thirdPartyData: ThirdPartyData,
  envelopeId: EnvelopeId,
  maybeAccessCode: Option[AccessCode],
  recalculation: Recalculation[F, E],
  componentIdToFileId: FormComponentIdToFileIdMapping,
  lookupRegistry: LookupRegistry,
  taskIdTaskStatus: TaskIdTaskStatusMapping
)(implicit
  hc: HeaderCarrier,
  me: MonadError[F, E]
) {

  private def toRecalculationResults(
    data: VariadicFormData[SourceOrigin.OutOfDate],
    formModel: FormModel[Interim],
    formPhase: Option[FormPhase],
    lang: LangADT,
    messages: Messages,
    formStartDate: Instant,
    currentSection: Option[SectionOrSummary]
  ): F[RecalculationResult] = {
    val modelComponentId: Map[ModelComponentId, List[(FileComponentId, VariadicValue.One)]] =
      formModel.allMultiFileIds.map { modelComponentId =>
        modelComponentId -> data.filesOfMultiFileComponent(modelComponentId)
      }.toMap
    val evaluationContext =
      EvaluationContext(
        formTemplate._id,
        SubmissionRef(envelopeId),
        maybeAccessCode,
        retrievals,
        thirdPartyData,
        formTemplate.authConfig,
        hc,
        formPhase,
        FileIdsWithMapping(formModel.allFileIds, formModel.allMultiFileIds, componentIdToFileId),
        modelComponentId,
        formModel.dateLookup,
        formModel.addressLookup,
        formModel.overseasAddressLookup,
        formModel.postcodeLookup,
        formModel.pageIdSectionNumberMap,
        lang,
        messages,
        formModel.allIndexedComponentIds,
        formModel.taxPeriodDate,
        FileSizeLimit(formTemplate.fileSizeLimit.getOrElse(FileSizeLimit.defaultFileLimitSize)),
        formModel.dataRetrieveAll,
        formModel.hideChoicesSelected,
        formModel.choiceLookup,
        formModel.addToListIds,
        lookupRegistry,
        formModel.lookupRegister,
        formModel.constraints,
        taskIdTaskStatus,
        LocalDate.ofInstant(formStartDate, ZoneId.of("Europe/London")),
        currentSection
      )

    recalculation
      .recalculateFormDataNew(
        data,
        formModel,
        formTemplate,
        retrievals,
        thirdPartyData,
        evaluationContext,
        messages
      )
  }

  def dependencyGraphValidation[U <: SectionSelectorType: SectionSelector]: FormModel[DependencyGraphVerification] =
    expand(VariadicFormData.empty[SourceOrigin.OutOfDate])

  def renderPageModel[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    phase: Option[FormPhase],
    formModelInterim: Option[FormModel[Interim]] = None
  )(implicit messages: Messages, lang: LangADT): FormModelOptics[D] = {

    implicit val fmvo = formModelVisibilityOptics

    val data: VariadicFormData[SourceOrigin.Current] = formModelVisibilityOptics.recData.variadicFormData
    val dataOutOfDate = data.asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]]
    val formModel: FormModel[DataExpanded] = expand(dataOutOfDate)
    val formModelVisibility: FormModel[Visibility] =
      getVisibilityModel(
        formModel,
        formModelVisibilityOptics,
        phase,
        formModelInterim.getOrElse(formModel).asInstanceOf[FormModel[Interim]]
      )

    val formModelVisibilityOpticsFinal = new FormModelVisibilityOptics[D](
      formModelVisibility,
      formModelVisibilityOptics.recData,
      formModelVisibilityOptics.recalculationResult
    )

    val formModelRenderPageOptics = FormModelRenderPageOptics[D](
      formModel,
      formModelVisibilityOptics.recData
    )

    FormModelOptics[D](formModelRenderPageOptics, formModelVisibilityOpticsFinal)
  }

  private def getVisibilityModel[D <: DataOrigin](
    formModel: FormModel[DataExpanded],
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    phase: Option[FormPhase],
    formModelInterim: FormModel[Interim]
  )(implicit messages: Messages, lang: LangADT): FormModel[Visibility] = {
    val data: VariadicFormData[SourceOrigin.Current] = formModelVisibilityOptics.recData.variadicFormData

    def onDemandPageIncludeIf(includeIf: IncludeIf) = {
      val modelComponentId: Map[ModelComponentId, List[(FileComponentId, VariadicValue.One)]] =
        formModel.allMultiFileIds.map { modelComponentId =>
          modelComponentId -> data.filesOfMultiFileComponent(modelComponentId)
        }.toMap

      val evaluationContext =
        EvaluationContext(
          formTemplate._id,
          SubmissionRef(envelopeId),
          maybeAccessCode,
          retrievals,
          thirdPartyData,
          formTemplate.authConfig,
          hc,
          phase,
          FileIdsWithMapping(formModel.allFileIds, formModel.allMultiFileIds, componentIdToFileId),
          modelComponentId,
          formModel.dateLookup,
          formModel.addressLookup,
          formModel.overseasAddressLookup,
          formModel.postcodeLookup,
          formModel.pageIdSectionNumberMap,
          lang,
          messages,
          formModel.allIndexedComponentIds,
          formModel.taxPeriodDate,
          FileSizeLimit(formTemplate.fileSizeLimit.getOrElse(FileSizeLimit.defaultFileLimitSize)),
          formModel.dataRetrieveAll,
          formModel.hideChoicesSelected,
          formModel.choiceLookup,
          formModel.addToListIds,
          lookupRegistry,
          formModel.lookupRegister,
          formModel.constraints,
          taskIdTaskStatus,
          None
        )

      val exprs = includeIf.booleanExpr.allExpressions

      val baseFcLookup = formModelInterim.allFormComponentIds
        .map(fcId => fcId.baseComponentId -> fcId)
        .foldLeft(mutable.Map.empty[BaseComponentId, List[FormComponentId]]) { case (acc, (baseId, fcId)) =>
          acc.addOne(
            baseId -> (acc.getOrElse(baseId, List()) :+ fcId)
          )
        }

      //    println("base fc keys: " + baseFcLookup.keys)

      val atlComponents = formModelInterim.addToListIds.map(_.formComponentId)
      //    println("atl components: " + atlComponents)

      val standaloneSumsFcIds = formModel.standaloneSumInfo.sums.flatMap(_.allFormComponentIds())

      val formComponents =
        (exprs.flatMap(_.allFormComponentIds()) ++ atlComponents ++ standaloneSumsFcIds)
          .map(fcId => formModelInterim.fcLookup.get(fcId) -> fcId)
          .flatMap {
            case (Some(fc), fcId) => List(fc)
            case (None, fcId) =>
              baseFcLookup.get(fcId.baseComponentId).toList.flatten.flatMap(formModel.fcLookup.get)
          }

      val er = formModelVisibilityOptics.evaluationResults
      val recalc = recalculation
        .recalculateFromGraph(
          formModel = formModelInterim,
          formTemplate = formTemplate,
          retrievals = retrievals,
          evaluationContext = evaluationContext,
          messages = messages,
          graph = DependencyGraph.graphFrom(
            formModel,
            AllFormTemplateExpressions(formTemplate),
            formComponents,
            exprs
          ),
          exprMapStart = er.exprMap,
          formDataMapStart = er.recData.variadicFormData.data,
          booleanExprCacheStart = formModelVisibilityOptics.booleanExprCache.mapping
        )

      val newEr = recalc match {
        case recalc: Future[RecalculationResult] =>
          Await.result(
            recalc,
            Duration.Inf
          )
        case recalc: cats.Id[RecalculationResult] => recalc.asInstanceOf[RecalculationResult]
        case _                                    => throw new RuntimeException("Unknown functor type")
      }

      FormModelBuilder.evalIncludeIf(includeIf, newEr, newEr.evaluationResults.recData, formModel, phase)
    }

    FormComponentVisibilityFilter(formModelVisibilityOptics, phase)
      .stripHiddenFormComponents(formModel)
      .filter { pageModel =>
        pageModel.getIncludeIf.fold(true) { includeIf =>
          FormModelBuilder.evalIncludeIf(
            includeIf,
            formModelVisibilityOptics.recalculationResult,
            formModelVisibilityOptics.recData,
            formModelVisibilityOptics.formModel,
            phase
          )
        } && pageModel.getNotRequiredIf.fold(true) { includeIf =>
          !FormModelBuilder.evalIncludeIf(
            includeIf,
            formModelVisibilityOptics.recalculationResult,
            formModelVisibilityOptics.recData,
            formModelVisibilityOptics.formModel,
            phase
          )
        }
      }
      .map[Visibility] { singleton: Singleton[DataExpanded] =>
        val updatedFields = singleton.page.fields.flatMap {
          case fc @ IsRevealingChoice(rc) => fc.copy(`type` = RevealingChoice.slice(fc.id)(data)(rc)) :: Nil
          case otherwise                  => otherwise :: Nil
        }
        singleton.copy(page = singleton.page.copy(fields = updatedFields))
      } { checkYourAnswers: CheckYourAnswers[DataExpanded] =>
        checkYourAnswers.asInstanceOf[CheckYourAnswers[Visibility]]
      } { repeater: Repeater[DataExpanded] =>
        repeater.asInstanceOf[Repeater[Visibility]]
      }
      .copy(onDemandIncludeIf = Some(onDemandPageIncludeIf))
  }

  def visibilityModel[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData[SourceOrigin.OutOfDate],
    phase: Option[FormPhase],
    formStartDate: Instant,
    currentSection: Option[SectionOrSummary] = None
  )(implicit messages: Messages, lang: LangADT): F[FormModelVisibilityOptics[D]] = {
    val formModel: FormModel[Interim] = expand(data)

    val recalculationResultF: F[RecalculationResult] =
      toRecalculationResults(data, formModel, phase, lang, messages, formStartDate, currentSection)

    recalculationResultF.map { recalculationResult =>
      buildFormModelVisibilityOptics(
        data,
        formModel,
        recalculationResult,
        phase
      )
    }
  }

  private def buildFormModelVisibilityOptics[U <: SectionSelectorType: SectionSelector, D <: DataOrigin](
    data: VariadicFormData[OutOfDate],
    formModel: FormModel[Interim],
    recalculationResult: RecalculationResult,
    phase: Option[FormPhase]
  )(implicit messages: Messages): FormModelVisibilityOptics[D] = {
    val evaluationResults = recalculationResult.evaluationResults
    val dataOld = RecData(data).asInstanceOf[RecData[SourceOrigin.Current]]
    val visibilityFormModel: FormModel[Visibility] = formModel.filter[Visibility] { pageModel =>
      pageModel.getIncludeIf.fold(true) { includeIf =>
        FormModelBuilder.evalIncludeIf(
          includeIf,
          recalculationResult,
          dataOld,
          formModel,
          phase
        )
      }
    }

    val visibleTypedExprs: List[(FormComponentId, TypeInfo)] = visibilityFormModel.allFormComponents.collect {
      case fc @ HasValueExpr(expr) if !fc.editable => (fc.id, visibilityFormModel.explicitTypedExpr(expr, fc.id))
    }

    val booleanExprResolver = BooleanExprResolver(booleanExpr =>
      FormModelBuilder.evalIncludeIf(
        IncludeIf(booleanExpr),
        recalculationResult,
        dataOld,
        visibilityFormModel,
        phase
      )
    )

    val visibleVariadicData: VariadicFormData[SourceOrigin.Current] =
      visibleTypedExprs.foldMap { case (fcId, typeInfo) =>
        val expressionResult =
          evaluationResults
            .evalExpr(typeInfo, RecData(data), booleanExprResolver, recalculationResult.evaluationContext)
            .applyTypeInfo(typeInfo)

        FormModelBuilder.toCurrentData(fcId.modelComponentId, expressionResult, typeInfo)
      }

    val currentData = data ++ visibleVariadicData

    val recData: RecData[SourceOrigin.Current] = RecData.empty.copy(variadicFormData = currentData)

    FormModelVisibilityOptics[D](visibilityFormModel, recData, recalculationResult)
  }

  def expand[T <: PageMode: FormModelExpander, U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): FormModel[T] = {
    val basicFm: FormModel[T] = basic(data)
    mkFormModel(basicFm, data)
  }

  private def mkCheckYourAnswers[T <: PageMode](
    c: CheckYourAnswersPage,
    s: Section.AddToList,
    index: Int
  ): CheckYourAnswers[T] = {
    // dummy FormComponentId so that CheckYourAnswers page model works as expected when computing visited indexes
    val fc = new FormComponentUpdater(
      s.addAnotherQuestion.copy(
        id = s.addAnotherQuestion.id.withSuffix("CYA"),
        validIf = c.removeItemIf.map(removeItemIf => ValidIf(Not(removeItemIf.booleanExpr)))
      ),
      index,
      s.allIds
    ).updatedWithId.copy(mandatory = false, derived = true, submissible = false)

    val expandedFields = c.fields.map(_.map(fc => new FormComponentUpdater(fc, index, s.allIds).updatedWithId))

    CheckYourAnswers[T](
      s.pageId.withIndex(index).withSuffix("CYA"),
      c.title.map(_.expand(index, s.allIds)),
      c.caption.map(_.expand(index, s.allIds)),
      c.updateTitle.expand(index, s.allIds),
      c.noPIITitle.map(_.expand(index, s.allIds)),
      c.noPIIUpdateTitle.map(_.expand(index, s.allIds)),
      c.header.map(_.expand(index, s.allIds)),
      c.footer.map(_.expand(index, s.allIds)),
      c.continueLabel.map(_.expand(index, s.allIds)),
      fc,
      index,
      c.presentationHint,
      c.removeItemIf.map(c => RemoveItemIf(BooleanExprUpdater(c.booleanExpr, index, s.allIds))),
      expandedFields,
      c.displayWidth,
      c.keyDisplayWidth
    )
  }

  private def mkDeclaration[T <: PageMode](
    d: DeclarationSection,
    s: Section.AddToList,
    index: Int
  ): Singleton[T] = {
    val expandedFields = d.fields.map(fc => new FormComponentUpdater(fc, index, s.allIds).updatedWithId)

    Singleton(
      Page(
        title = d.title.expand(index, s.allIds),
        id = Some(s.pageId.withIndex(index).withSuffix("DEC")),
        noPIITitle = d.noPIITitle.map(_.expand(index, s.allIds)),
        description = d.description.map(_.expand(index, s.allIds)),
        shortName = d.shortName.map(_.expand(index, s.allIds)),
        caption = d.caption.map(_.expand(index, s.allIds)),
        includeIf = d.includeIf.map(i => IncludeIf(BooleanExprUpdater(i.booleanExpr, index, s.allIds))),
        fields = expandedFields,
        continueLabel = d.continueLabel.map(_.expand(index, s.allIds)),
        continueIf = None,
        instruction = None,
        presentationHint = None,
        dataRetrieve = None,
        confirmation = None,
        redirects = None,
        hideSaveAndComeBackButton = Some(true),
        removeItemIf = None,
        displayWidth = None,
        notRequiredIf = None
      )
    )
  }

  private def mkRepeater[T <: PageMode](s: Section.AddToList, index: Int): Repeater[T] = {
    val expand: SmartString => SmartString = _.expand(index, s.allIds)
    val fc = new FormComponentUpdater(s.addAnotherQuestion, index, s.allIds).updatedWithId

    val expandedFields = s.fields.map(_.map(fc => new FormComponentUpdater(fc, index, s.allIds).updatedWithId))

    def expandAtlDescription(atlDescription: AtlDescription) =
      atlDescription match {
        case ssb: AtlDescription.SmartStringBased => ssb.copy(value = expand(ssb.value))
        case kvb: AtlDescription.KeyValueBased    => kvb.copy(key = expand(kvb.key), value = expand(kvb.value))
      }

    def expandAtlDescriptionTotal(
      descriptionTotal: Option[AtlDescription.KeyValueBased]
    ): Option[AtlDescription.KeyValueBased] =
      descriptionTotal match {
        case Some(kvBased) => Some(kvBased.copy(key = expand(kvBased.key), value = expand(kvBased.value)))
        case None          => None
      }

    Repeater[T](
      expand(s.title),
      s.caption.map(expand),
      s.pageId.withIndex(index),
      s.noPIITitle.map(expand),
      expandAtlDescription(s.description),
      expand(s.summaryDescription),
      expand(s.shortName),
      expand(s.summaryName),
      s.includeIf,
      fc,
      index,
      s.instruction,
      expandedFields,
      s.repeatsUntil.map(c => IncludeIf(BooleanExprUpdater(c.booleanExpr, index, s.allIds))),
      s.repeatsWhile.map(c => IncludeIf(BooleanExprUpdater(c.booleanExpr, index, s.allIds))),
      expandAtlDescriptionTotal(s.descriptionTotal),
      s.notRequiredIf.map(c => IncludeIf(BooleanExprUpdater(c.booleanExpr, index, s.allIds))),
      s.displayWidth
    )
  }

  private def mkSingleton(page: Page[Basic], index: Int): Section.AddToList => Page[Basic] =
    source => PageUpdater(page, index, source.allIds)

  private def mergeIncludeIfs[T <: PageMode](includeIf: IncludeIf, page: Page[T]): Page[T] = page.copy(
    includeIf = Some(page.includeIf.fold(includeIf)(inIf => IncludeIf(And(inIf.booleanExpr, includeIf.booleanExpr))))
  )

  private def basicDefaultPage[T <: PageMode: FormModelExpander](
    s: Section.AddToList,
    templateSectionIndex: TemplateSectionIndex,
    maybeCoordinates: Option[Coordinates],
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): Option[SingletonWithNumber[T]] = {
    val addToListPages: Option[Page[Basic]] = s.defaultPage

    addToListPages.map { page =>
      val page2: Page[Basic] = mkSingleton(page, 1)(s).copy(notRequiredIf = s.notRequiredIf)
      val page3: Page[T] = implicitly[FormModelExpander[T]].lift(page2, data)
      val sectionNumber = mkSectionNumber(
        SectionNumber.Classic.AddToListPage.DefaultPage(templateSectionIndex),
        maybeCoordinates
      )
      SingletonWithNumber[T](Singleton(page3), sectionNumber)
    }
  }

  private def basicAddToList[T <: PageMode: FormModelExpander](
    defaultPage: Option[SingletonWithNumber[T]],
    s: Section.AddToList,
    templateSectionIndex: TemplateSectionIndex,
    maybeCoordinates: Option[Coordinates],
    iterationIndex: Int,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): Option[Bracket.AddToListIteration[T]] = {
    val singletons: List[SingletonWithNumber[T]] = {
      val addToListPages: NonEmptyList[Page[Basic]] = s.pages

      addToListPages.zipWithIndex.map { case (page, pageIndex) =>
        val page1: Page[Basic] = s.includeIf
          .fold(page)(includeIf => mergeIncludeIfs(includeIf, page))
          .copy(notRequiredIf = s.notRequiredIf)
        val page2: Page[Basic] = mkSingleton(page1, iterationIndex)(s)
        val page3: Page[T] = implicitly[FormModelExpander[T]].lift(page2, data)
        val sectionNumber = mkSectionNumber(
          SectionNumber.Classic.AddToListPage.Page(templateSectionIndex, iterationIndex, pageIndex),
          maybeCoordinates
        )
        SingletonWithNumber[T](Singleton(page3), sectionNumber)
      }.toList
    }

    val repeater: Repeater[T] = mkRepeater(s, iterationIndex)

    val checkYourAnswers: Option[CheckYourAnswersWithNumber[T]] = s.cyaPage.map(c =>
      CheckYourAnswersWithNumber(
        mkCheckYourAnswers(c, s, iterationIndex),
        mkSectionNumber(
          SectionNumber.Classic.AddToListPage
            .TerminalPage(templateSectionIndex, iterationIndex, TerminalPageKind.CyaPage),
          maybeCoordinates
        )
      )
    )

    val declaration: Option[SingletonWithNumber[T]] = s.declarationSection.map(d =>
      SingletonWithNumber(
        mkDeclaration(d, s, iterationIndex),
        mkSectionNumber(
          SectionNumber.Classic.AddToListPage
            .TerminalPage(templateSectionIndex, iterationIndex, TerminalPageKind.DeclarationPage),
          maybeCoordinates
        )
      )
    )

    NonEmptyList
      .fromList(singletons)
      .map(
        Bracket.AddToListIteration(
          defaultPage,
          _,
          checkYourAnswers,
          declaration,
          RepeaterWithNumber(
            repeater,
            mkSectionNumber(
              SectionNumber.Classic.AddToListPage
                .TerminalPage(templateSectionIndex, iterationIndex, TerminalPageKind.RepeaterPage),
              maybeCoordinates
            )
          )
        )
      )
  }

  private def mkSectionNumber(
    sn: SectionNumber.Classic,
    coordinates: Option[Coordinates]
  ): SectionNumber = coordinates.fold[SectionNumber](sn)(coordinates => SectionNumber.TaskList(coordinates, sn))

  private def basic[T <: PageMode, U <: SectionSelectorType](
    data: VariadicFormData[SourceOrigin.OutOfDate]
  )(implicit formModelExpander: FormModelExpander[T], sectionIncluder: SectionSelector[U]): FormModel[T] = {

    val allSections: AllSections = sectionIncluder.getSections(formTemplate)

    val staticTypeInfo: StaticTypeInfo =
      allSections.sections.foldLeft(StaticTypeInfo.empty)(_ ++ _.section.staticTypeInfo)

    val revealingChoiceInfo: RevealingChoiceInfo =
      allSections.sections.foldLeft(RevealingChoiceInfo.empty)(_ ++ _.section.revealingChoiceInfo)

    val brackets: BracketPlainCoordinated[T] = allSections.mapSection { maybeCoordinates =>
      {
        case IndexedSection.SectionNoIndex(s) =>
          val page = formModelExpander.lift(s.page, data)
          val sectionNumber =
            mkSectionNumber(SectionNumber.classicFixed, maybeCoordinates)
          Some(Bracket.NonRepeatingPage(SingletonWithNumber[T](Singleton(page), sectionNumber), s))
        case IndexedSection.SectionIndex(s: Section.NonRepeatingPage, index) =>
          val page = formModelExpander.lift(s.page, data)
          val sectionNumber = mkSectionNumber(SectionNumber.Classic.NormalPage(index), maybeCoordinates)
          Some(Bracket.NonRepeatingPage(SingletonWithNumber[T](Singleton(page), sectionNumber), s))
        case IndexedSection.SectionIndex(s: Section.RepeatingPage, index) =>
          formModelExpander.liftRepeating(s, index, data)
        case IndexedSection.SectionIndex(s: Section.AddToList, index) =>
          val defaultPage: Option[SingletonWithNumber[T]] = basicDefaultPage(s, index, maybeCoordinates, data)
          basicAddToList(defaultPage, s, index, maybeCoordinates, 1, data).map(atl =>
            Bracket.AddToList(NonEmptyList.one(atl), s)
          )
      }
    }

    val sumInfo: SumInfo = allSections.sections.foldLeft(SumInfo.empty)(_ ++ _.section.sumInfo)

    FormModel.fromPages(brackets, staticTypeInfo, revealingChoiceInfo, sumInfo, formTemplate.dataRetrieve)
  }

  private def repeaterIsYes(
    modelComponentId: ModelComponentId,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): Boolean = {
    val nextOne: Option[Seq[String]] = data.many(modelComponentId)
    val next = nextOne.toSeq.flatten
    next.contains("0")
  }

  private def answeredAddToListIterations[T <: PageMode: FormModelExpander](
    iteration: Bracket.AddToListIteration[T],
    data: VariadicFormData[SourceOrigin.OutOfDate],
    source: Section.AddToList
  ): NonEmptyList[Bracket.AddToListIteration[T]] = {
    def loop(
      repeater: RepeaterWithNumber[T],
      acc: NonEmptyList[Bracket.AddToListIteration[T]]
    ): NonEmptyList[Bracket.AddToListIteration[T]] =
      if (repeaterIsYes(repeater.repeater.addAnotherQuestion.modelComponentId, data)) {
        val templateSectionIndex: TemplateSectionIndex = repeater.sectionNumber.templateSectionIndex
        val maybeCoordinates: Option[Coordinates] = repeater.sectionNumber.maybeCoordinates
        val maybeBracket =
          basicAddToList(
            None, // next iteration has no default page
            source,
            templateSectionIndex,
            maybeCoordinates,
            repeater.repeater.index + 1,
            data
          ) // Add next iteration
        maybeBracket
          .map { bracket =>
            loop(bracket.repeater, acc ::: NonEmptyList.one(bracket))
          }
          .getOrElse(acc)
      } else {
        acc
      }
    loop(iteration.repeater, NonEmptyList.one(iteration))
  }

  private def mkFormModel[T <: PageMode: FormModelExpander](
    formModel: FormModel[T],
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): FormModel[T] =
    formModel.flatMapRepeater {
      case (NonEmptyList(iteration, Nil), source) => answeredAddToListIterations(iteration, data, source)
      case (iterations, _)                        => iterations
    }
}
