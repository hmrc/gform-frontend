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

package uk.gov.hmrc.gform.models

import cats.{ Functor, MonadError }
import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.foldable._
import cats.syntax.functor._
import scala.language.higherKinds
import uk.gov.hmrc.gform.controllers.{ AuthCache, CacheData }
import uk.gov.hmrc.gform.eval.{ EvaluationContext, EvaluationResults, ExpressionResult, TypedExpr }
import uk.gov.hmrc.gform.gform.{ FormComponentUpdater, PageUpdater }
import uk.gov.hmrc.gform.graph.{ RecData, Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelRenderPageOptics, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, BooleanExprCache, SmartString, SourceOrigin, SubmissionRef, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormModelOptics, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.http.HeaderCarrier

object FormModelBuilder {
  def fromCache[E, F[_]: Functor](
    cache: AuthCache,
    cacheData: CacheData,
    recalculation: Recalculation[F, E]
  )(
    implicit
    hc: HeaderCarrier,
    me: MonadError[F, E]
  ): FormModelBuilder[E, F] =
    new FormModelBuilder(
      cache.retrievals,
      cache.formTemplate,
      cacheData.thirdPartyData,
      cacheData.envelopeId,
      cache.accessCode,
      recalculation
    )
}

class FormModelBuilder[E, F[_]: Functor](
  retrievals: MaterialisedRetrievals,
  formTemplate: FormTemplate,
  thirdPartyData: ThirdPartyData,
  envelopeId: EnvelopeId,
  maybeAccessCode: Option[AccessCode],
  recalculation: Recalculation[F, E]
)(
  implicit
  hc: HeaderCarrier,
  me: MonadError[F, E]
) {

  private def toEvaluationResults(
    data: VariadicFormData[SourceOrigin.OutOfDate],
    formModel: FormModel[Interim]
  ): F[RecalculationResult] = {
    val typedExpressionLookup: Map[FormComponentId, TypedExpr] = formModel.allFormComponents.collect {
      case fc @ HasExpr(expr) => fc.id -> formModel.explicitTypedExpr(expr, fc.id)
    }.toMap

    val evaluationContext =
      new EvaluationContext(
        formTemplate._id,
        SubmissionRef(envelopeId),
        typedExpressionLookup,
        maybeAccessCode,
        retrievals,
        thirdPartyData,
        formTemplate.authConfig,
        hc)

    recalculation
      .recalculateFormDataNew(
        data,
        formModel,
        formTemplate,
        retrievals,
        thirdPartyData,
        maybeAccessCode,
        envelopeId,
        evaluationContext)
  }

  private def toCurrentData(
    modelComponentId: ModelComponentId,
    expressionResult: ExpressionResult
  ): VariadicFormData[SourceOrigin.Current] =
    expressionResult match {
      case ExpressionResult.Empty      => VariadicFormData.one[SourceOrigin.Current](modelComponentId, "")
      case ExpressionResult.Hidden     => VariadicFormData.empty[SourceOrigin.Current]
      case ExpressionResult.Invalid(_) => VariadicFormData.empty[SourceOrigin.Current]
      case ExpressionResult.NumberResult(bigDecimal) =>
        VariadicFormData.one[SourceOrigin.Current](modelComponentId, bigDecimal.toString)
      case ExpressionResult.IntResult(integer) =>
        VariadicFormData.one[SourceOrigin.Current](modelComponentId, integer.toString)
      case ExpressionResult.StringResult(value) => VariadicFormData.one[SourceOrigin.Current](modelComponentId, value)
      case ExpressionResult.OptionResult(value) =>
        VariadicFormData.many[SourceOrigin.Current](modelComponentId, value.map(_.toString))
    }

  def dependencyGraphValidation[U <: SectionSelectorType: SectionSelector]: FormModel[DependencyGraphVerification] =
    expand(VariadicFormData.empty[SourceOrigin.OutOfDate])

  def renderPageModel[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): FormModelOptics[D] = {

    implicit val fmvo = formModelVisibilityOptics

    val data: VariadicFormData[SourceOrigin.Current] = formModelVisibilityOptics.recData.variadicFormData
    val dataOutOfDate = data.asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]]
    val formModel: FormModel[DataExpanded] = expand(dataOutOfDate)
    val formModelVisibility: FormModel[Visibility] = visibilityModel(formModel, formModelVisibilityOptics)

    val formModelVisibilityOpticsFinal = new FormModelVisibilityOptics[D](
      formModelVisibility,
      formModelVisibilityOptics.recData,
      formModelVisibilityOptics.evaluationResults,
      formModelVisibilityOptics.graphData,
      formModelVisibilityOptics.booleanExprCache
    )

    val formModelRenderPageOptics = FormModelRenderPageOptics[D](
      formModel,
      formModelVisibilityOptics.recData
    )

    FormModelOptics[D](formModelRenderPageOptics, formModelVisibilityOpticsFinal)
  }

  private def visibilityModel[D <: DataOrigin](
    formModel: FormModel[DataExpanded],
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): FormModel[Visibility] = {
    val evaluationResults = formModelVisibilityOptics.evaluationResults
    val booleanExprCache = formModelVisibilityOptics.booleanExprCache
    val data: VariadicFormData[SourceOrigin.Current] = formModelVisibilityOptics.recData.variadicFormData
    formModel
      .filter { pageModel =>
        pageModel.getIncludeIf.fold(true) { includeIf =>
          evalIncludeIf(includeIf, evaluationResults, formModelVisibilityOptics.formModel, booleanExprCache)
        }
      }
      .map { pageModel: PageModel[DataExpanded] =>
        pageModel.fold[PageModel[DataExpanded]] { singleton =>
          val updatedFields = singleton.page.fields.flatMap {
            case fc @ IsRevealingChoice(rc) =>
              fc.copy(`type` = RevealingChoice.slice(fc.id)(data)(rc)) :: Nil
            case otherwise => otherwise :: Nil
          }
          singleton.copy(page = singleton.page.copy(fields = updatedFields))
        }(identity)
      }
      .asInstanceOf[FormModel[Visibility]]
  }

  def visibilityModel[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): F[FormModelVisibilityOptics[D]] = {
    val formModel: FormModel[Interim] = expand(data)

    toEvaluationResults(data, formModel).map { recalculationResult =>
      val evaluationResults = recalculationResult.evaluationResults
      val graphData = recalculationResult.graphData
      val booleanExprCache = recalculationResult.booleanExprCache
      val visibilityFormModel: FormModel[Visibility] = formModel.filter[Visibility] { pageModel =>
        pageModel.getIncludeIf.fold(true) { includeIf =>
          evalIncludeIf(includeIf, evaluationResults, formModel, booleanExprCache)
        }
      }

      val visibleTypedExprs: List[(FormComponentId, TypedExpr)] = visibilityFormModel.allFormComponents.collect {
        case fc @ HasValueExpr(expr) => (fc.id, visibilityFormModel.explicitTypedExpr(expr, fc.id))
      }

      val visibleVariadicData: VariadicFormData[SourceOrigin.Current] =
        visibleTypedExprs.foldMap {
          case (fcId, typedExpr) =>
            evaluationResults.get(typedExpr).fold(VariadicFormData.empty[SourceOrigin.Current]) { expressionResult =>
              toCurrentData(fcId.modelComponentId, expressionResult)
            }
        }

      val currentData = data ++ visibleVariadicData

      val recData: RecData[SourceOrigin.Current] = RecData.empty.copy(variadicFormData = currentData)

      new FormModelVisibilityOptics(visibilityFormModel, recData, evaluationResults, graphData, booleanExprCache)
    }

  }

  private def evalIncludeIf[T <: PageMode](
    includeIf: IncludeIf,
    evaluationResults: EvaluationResults,
    formModel: FormModel[T],
    booleanExprCache: BooleanExprCache
  ): Boolean = {
    def compare(expr1: Expr, expr2: Expr, f: (ExpressionResult, ExpressionResult) => Boolean) = {
      val typedExpr1 = formModel.toTypedExpr(expr1)
      val typedExpr2 = formModel.toTypedExpr(expr2)
      val maybeBoolean = for {
        r <- evaluationResults.get(typedExpr1)
        s <- evaluationResults.get(typedExpr2)
      } yield f(r, s)

      maybeBoolean.getOrElse(false)

    }

    def loop(booleanExpr: BooleanExpr): Boolean = booleanExpr match {
      case Equals(field1, field2)              => compare(field1, field2, _ identical _)
      case NotEquals(field1, field2)           => compare(field1, field2, _ =!= _)
      case GreaterThan(field1, field2)         => compare(field1, field2, _ > _)
      case GreaterThanOrEquals(field1, field2) => compare(field1, field2, _ >= _)
      case LessThan(field1, field2)            => compare(field1, field2, _ < _)
      case LessThanOrEquals(field1, field2)    => compare(field1, field2, _ <= _)
      case Not(invertedExpr)                   => !loop(invertedExpr)
      case Or(expr1, expr2)                    => val e1 = loop(expr1); val e2 = loop(expr2); e1 | e2
      case And(expr1, expr2)                   => val e1 = loop(expr1); val e2 = loop(expr2); e1 & e2
      case IsTrue                              => true
      case IsFalse                             => false
      case Contains(field1, field2)            => compare(field1, field2, _ contains _)
      case In(expr, dataSource) =>
        val maybeBoolean = for {
          expressionResult <- evaluationResults.get(formModel.toTypedExpr(expr))
          res              <- booleanExprCache.get(dataSource, expressionResult.stringRepresentation)
        } yield res
        maybeBoolean.getOrElse(false)
    }

    loop(includeIf.booleanExpr)

  }

  // 1. FormModel[DataExpanded, SourceOrigin.OutOfDate] - this model contains all pages and expanded
  //      groups and repeated sections are expanded based on data (not based on repeatsMax ie. expression)

  // 2. Remove revealingChoice fields which are not selected.
  //    FormModel[DataExpanded, SourceOrigin.OutOfDate] => FormModel[DataExpanded with RevealingChoiceVisibleOnly, SourceOrigin.OutOfDate]

  // 3. Create dependency graph based on model from step 2.
  //     a) For every graphLayer adjust model such that it will not contain hidden section
  //    FormModel[DataExpanded with RevealingChoiceVisibleOnly, SourceOrigin.OutOfDate] => FormModel[Visibility]

  private def expand[T <: PageMode: FormModelExpander, U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): FormModel[T] = {
    val basicFm: FormModel[T] = basic(data)
    mkFormModel(basicFm, data)
  }

  private def mkRepeater[T <: PageMode](s: Section.AddToList, index: Int): Repeater[T] = {
    val fc = new FormComponentUpdater(s.addAnotherQuestion, index, s.allIds).updatedWithId
    Repeater[T](s.title, s.description, s.shortName, s.includeIf, fc, index, s)
  }

  private def mkSingleton(page: Page[Basic], index: Int): Section.AddToList => Page[Basic] =
    source => PageUpdater(page, index, source.allIds)

  private def mergeIncludeIfs[T <: PageMode](includeIf: IncludeIf, page: Page[T]): Page[T] = page.copy(
    includeIf = Some(page.includeIf.fold(includeIf)(inIf => IncludeIf(And(inIf.booleanExpr, includeIf.booleanExpr))))
  )

  private def basicAddToList[T <: PageMode: FormModelExpander](
    s: Section.AddToList,
    index: Int,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): List[PageModel[T]] = {
    val qq: List[PageModel[T]] =
      s.pages.map { page =>
        val page1: Page[Basic] = s.includeIf.fold(page)(includeIf => mergeIncludeIfs(includeIf, page))
        val page2: Page[Basic] = mkSingleton(page1, index)(s)
        val page3: Page[T] = implicitly[FormModelExpander[T]].lift(page2, data)
        Singleton[T](page3, s)
      }.toList

    val repeater: PageModel[T] = mkRepeater(s, index)

    qq :+ repeater
  }

  private def basic[T <: PageMode, U <: SectionSelectorType](
    data: VariadicFormData[SourceOrigin.OutOfDate]
  )(implicit formModelExpander: FormModelExpander[T], sectionIncluder: SectionSelector[U]): FormModel[T] = {

    val allSections: List[Section] = sectionIncluder.getSections(formTemplate)

    val pages = allSections
      .flatMap {
        case s: Section.NonRepeatingPage => List(Singleton[T](formModelExpander.lift(s.page, data), s))
        case s: Section.RepeatingPage    => formModelExpander.liftRepeating(s, data)
        case s: Section.AddToList        => basicAddToList(s, 1, data)
      }

    FormModel.fromPages(pages)

  }

  private def mkFormModel[T <: PageMode: FormModelExpander](
    formModel: FormModel[T],
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): FormModel[T] =
    formModel.flatMap {
      case Singleton(page, source) => List(Singleton[T](page, source))
      case Repeater(title, description, shortName, includeIf, addAnotherQuestionFc, index, source) =>
        val expand: SmartString => SmartString = _.expand(index, source.allIds)
        val exTitle = expand(title)
        val exShortName = expand(shortName)
        val exDescription = expand(description)
        val repeater =
          Repeater[T](exTitle, exDescription, exShortName, includeIf, addAnotherQuestionFc, index, source)
        val nextOne: Option[Seq[String]] = data.many(addAnotherQuestionFc.modelComponentId)
        val next = nextOne.toSeq.flatten

        val rest = if (next.contains("0")) {
          val addToListFormModel: FormModel[T] = FormModel.fromPages(basicAddToList(source, index + 1, data))
          mkFormModel(addToListFormModel, data).pages
        } else {
          Nil
        }
        repeater :: rest
    }

}
