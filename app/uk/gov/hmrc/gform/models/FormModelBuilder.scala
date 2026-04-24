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
import play.api.i18n.Messages
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.{ AuthCache, CacheData }
import uk.gov.hmrc.gform.gform.{ BooleanExprUpdater, FormComponentUpdater, PageUpdater }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ FormModelRenderPageOptics, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.recalculation.{ DependencyGraph, EvaluationContext, EvaluationStatus, FreeCalculator, Metadata, MongoUserData, Recalculator }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic.AddToListPage.TerminalPageKind
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import java.time.{ LocalDate, ZoneId }

object FormModelBuilder {
  def fromCache(
    cache: AuthCache,
    cacheData: CacheData,
    componentIdToFileId: FormComponentIdToFileIdMapping,
    taskIdTaskStatus: TaskIdTaskStatusMapping
  ): FormModelBuilder =
    new FormModelBuilder(
      cache.retrievals,
      cache.formTemplate,
      Metadata.from(cache.formTemplate),
      cacheData.thirdPartyData,
      cacheData.envelopeId,
      cache.accessCode,
      componentIdToFileId,
      cache.lookupRegistry,
      taskIdTaskStatus
    )
}

class FormModelBuilder(
  retrievals: MaterialisedRetrievals,
  formTemplate: FormTemplate,
  metadata: Metadata,
  thirdPartyData: ThirdPartyData,
  envelopeId: EnvelopeId,
  maybeAccessCode: Option[AccessCode],
  componentIdToFileId: FormComponentIdToFileIdMapping,
  lookupRegistry: LookupRegistry,
  taskIdTaskStatus: TaskIdTaskStatusMapping
) {

  def visibilityModel[U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData,
    phase: Option[FormPhase],
    form: Form
  )(implicit lang: LangADT, messages: Messages): FormModelOptics = {

    val (formModel, freeCalculator, graph): (FormModel, FreeCalculator, DependencyGraph) = fastModel(data, form, phase)

    val formModelVisibilityOptics: FormModelVisibilityOptics =
      buildFormModelVisibilityOptics(formModel, freeCalculator, graph)
    val formModelRenderPageOptics = new FormModelRenderPageOptics(formModel)

    new FormModelOptics(formModelRenderPageOptics, formModelVisibilityOptics)
  }

  // This also removes unselected or hidden options of choices
  private def hideFormComponents(
    formComponents: List[FormComponent],
    freeCalculator: FreeCalculator
  )(implicit messages: Messages): List[FormComponent] =
    formComponents
      .filter { formComponent =>
        val maybeEvaluationStatus: Option[EvaluationStatus] =
          freeCalculator.answerMap.get(formComponent.modelComponentId)
        maybeEvaluationStatus match {
          case Some(evaluationStatus) => evaluationStatus != EvaluationStatus.Hidden
          case None                   => formComponent.includeIf.fold(true)(includeIf => freeCalculator.evalIncludeIf(includeIf))
        }
      }
      .map {
        case fc @ IsChoice(choice) =>
          val choiceRes = choice.copy(
            options = choice.options.zipWithIndex.flatMap { case (choiceElement, index) =>
              val isChoiceHidden: Boolean =
                choiceElement.includeIf
                  .map(includeIf => !freeCalculator.evalIncludeIf(includeIf))
                  .getOrElse(false)

              if (isChoiceHidden) List.empty else List(choiceElement)
            }
          )
          // choiceRes is used in validation (but not on render time)
          fc.copy(`type` = choiceRes)
        case fc @ IsRevealingChoice(revealingChoice) =>
          val revealingChoiceRes = revealingChoice.copy(
            options = revealingChoice.options.zipWithIndex.flatMap { case (revealingChoiceElement, index) =>
              val isRevalingChoiceHidden: Boolean =
                revealingChoiceElement.choice.includeIf
                  .map(includeIf => !freeCalculator.evalIncludeIf(includeIf))
                  .getOrElse(false)

              val evaluationStatus: EvaluationStatus =
                freeCalculator.answerMapWithFallback.toStringResultOrOptionResult(fc.modelComponentId)

              val choiceValue: String = revealingChoiceElement.choice match {
                case OptionData.IndexBased(_, _, _, _, _)                                        => index.toString
                case OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(value), _, _) => value
                case OptionData.ValueBased(_, _, _, _, OptionDataValue.ExprBased(expr), _, _) =>
                  freeCalculator.evalExpr(expr).stringRepresentation(messages)
              }

              val isSelected = evaluationStatus.optionRepresentation.fold(false)(_.contains(choiceValue))

              if (isRevalingChoiceHidden || !isSelected) {
                List.empty[RevealingChoiceElement]
              } else {
                List(
                  revealingChoiceElement.copy(revealingFields =
                    revealingChoiceElement.revealingFields.filter(revealingField =>
                      !freeCalculator.answerMap.get(revealingField.modelComponentId).contains(EvaluationStatus.Hidden)
                    )
                  )
                )
              }
            }
          )

          fc.copy(`type` = revealingChoiceRes)
        case other => other
      }

  private def hideSingletonWithNumber(
    singleton: SingletonWithNumber,
    freeCalculator: FreeCalculator
  )(implicit messages: Messages): Option[SingletonWithNumber] = {
    val isVisiblePage =
      singleton.singleton.page.includeIf
        .fold(true)(includeIf => freeCalculator.evalIncludeIf(includeIf))
    if (isVisiblePage) {
      val visibleFields = hideFormComponents(singleton.singleton.page.fields, freeCalculator)
      Some(
        singleton.copy(
          singleton = singleton.singleton.copy(
            page = singleton.singleton.page.copy(
              fields = visibleFields
            )
          )
        )
      )
    } else {
      None
    }
  }

  private def hideCheckYourAnswersWithNumber(
    checkYourAnswersWithNumber: CheckYourAnswersWithNumber,
    freeCalculator: FreeCalculator
  )(implicit messages: Messages): CheckYourAnswersWithNumber = {

    val visibleFields =
      hideFormComponents(checkYourAnswersWithNumber.checkYourAnswers.fields.toList.flatMap(_.toList), freeCalculator)

    val visibleFieldsNel = NonEmptyList.fromList(visibleFields)

    checkYourAnswersWithNumber.copy(
      checkYourAnswers = checkYourAnswersWithNumber.checkYourAnswers.copy(
        fields = visibleFieldsNel
      )
    )
  }

  private def hideInvisiblePages(
    brackets: NonEmptyList[Bracket],
    freeCalculator: FreeCalculator
  )(implicit messages: Messages): List[Bracket] =
    brackets.toList.flatMap { bracket =>
      bracket.fold[List[Bracket]] { nonRepeatingPage =>
        hideSingletonWithNumber(nonRepeatingPage.singleton, freeCalculator).map { singleton =>
          nonRepeatingPage.copy(singleton = singleton)
        }.toList

      } { repeatingPage =>
        val singletons =
          repeatingPage.singletons.toList.flatMap(singleton => hideSingletonWithNumber(singleton, freeCalculator))

        NonEmptyList
          .fromList(singletons)
          .fold(List.empty[Bracket]) { singletonsNel =>
            List(repeatingPage.copy(singletons = singletonsNel))
          }
      } { addToList =>
        addToList.includeIf match {
          case Some(includeIf) if !freeCalculator.evalIncludeIf(includeIf) => List.empty
          case _ =>
            val iterations: List[Bracket.AddToListIteration] = addToList.iterations.toList.flatMap { iteration =>
              val defaultPage =
                iteration.defaultPage.flatMap(singleton => hideSingletonWithNumber(singleton, freeCalculator))

              val declarationSection =
                iteration.declarationSection.flatMap(singleton => hideSingletonWithNumber(singleton, freeCalculator))

              val singletons =
                iteration.singletons.toList.flatMap(singleton => hideSingletonWithNumber(singleton, freeCalculator))

              val checkYourAnswers = iteration.checkYourAnswers.map { checkYourAnswers =>
                hideCheckYourAnswersWithNumber(checkYourAnswers, freeCalculator)
              }

              NonEmptyList
                .fromList(singletons)
                .map { singletonsNel =>
                  iteration.copy(
                    defaultPage = defaultPage,
                    singletons = singletonsNel,
                    checkYourAnswers = checkYourAnswers,
                    declarationSection = declarationSection
                  )
                }
            }

            NonEmptyList
              .fromList(iterations)
              .fold(List.empty[Bracket]) { iterationsNel =>
                List(
                  addToList.copy(
                    iterations = iterationsNel
                  )
                )
              }
        }
      }
    }

  private def buildFormModelVisibilityOptics[U <: SectionSelectorType: SectionSelector](
    formModel: FormModel,
    freeCalculator: FreeCalculator,
    graph: DependencyGraph
  )(implicit messages: Messages): FormModelVisibilityOptics = {

    val bracketsWithSectionNumbers = formModel.brackets.fold[Brackets] { classic =>
      val brackets: List[Bracket] = hideInvisiblePages(classic.brackets, freeCalculator)
      NonEmptyList.fromList(brackets) match {
        case None              => throw new RuntimeException("All pages are hidden")
        case Some(bracketsNel) => Brackets.Classic(bracketsNel)
      }
    } { taskList =>
      val brackets = taskList.brackets.map { case (coordinated, taskList) =>
        val visibilityTaskList = taskList match {
          case TaskModel.AllHidden() => TaskModel.AllHidden()
          case TaskModel.Editable(brackets) =>
            val bracketsVisiblity: List[Bracket] = hideInvisiblePages(brackets, freeCalculator)

            NonEmptyList.fromList(bracketsVisiblity).fold[TaskModel](TaskModel.AllHidden()) { brackets =>
              TaskModel.Editable(brackets)
            }
        }
        coordinated -> visibilityTaskList

      }
      Brackets.TaskList(brackets)
    }

    val visibilityFormModel: FormModel = new FormModel(bracketsWithSectionNumbers)

    new FormModelVisibilityOptics(visibilityFormModel, freeCalculator)
  }

  private def mkCheckYourAnswers(
    c: CheckYourAnswersPage,
    s: Section.AddToList,
    index: Int
  ): CheckYourAnswers = {

    val expandedFields =
      c.fields.map(_.map(fc => new FormComponentUpdater(fc, index, s.allIds, s.allDataRetriveIds).updatedWithId))

    CheckYourAnswers(
      s.pageId.withIndex(index),
      c.title.map(_.expand(index, s.allIds)),
      c.caption.map(_.expand(index, s.allIds)),
      c.updateTitle.expand(index, s.allIds),
      c.noPIITitle.map(_.expand(index, s.allIds)),
      c.noPIIUpdateTitle.map(_.expand(index, s.allIds)),
      c.header.map(_.expand(index, s.allIds)),
      c.footer.map(_.expand(index, s.allIds)),
      c.continueLabel.map(_.expand(index, s.allIds)),
      index,
      c.presentationHint,
      c.removeItemIf.map(c => RemoveItemIf(BooleanExprUpdater(c.booleanExpr, index, s.allIds))),
      expandedFields,
      c.displayWidth,
      c.keyDisplayWidth
    )
  }

  private def mkRepeater(s: Section.AddToList, index: Int): Repeater = {
    val expand: SmartString => SmartString = _.expand(index, s.allIds)
    val fc = new FormComponentUpdater(s.addAnotherQuestion, index, s.allIds, s.allDataRetriveIds).updatedWithId

    val expandedFields =
      s.fields.map(_.map(fc => new FormComponentUpdater(fc, index, s.allIds, s.allDataRetriveIds).updatedWithId))

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

    Repeater(
      expand(s.title),
      s.caption.map(expand),
      s.pageId.withIndex(index),
      s.noPIITitle.map(expand),
      expandAtlDescription(s.description),
      expand(s.summaryDescription),
      expand(s.shortName),
      expand(s.summaryName),
      fc,
      index,
      s.instruction,
      expandedFields,
      s.repeatsUntil.map(c => IncludeIf(BooleanExprUpdater(c.booleanExpr, index, s.allIds))),
      s.repeatsWhile.map(c => IncludeIf(BooleanExprUpdater(c.booleanExpr, index, s.allIds))),
      expandAtlDescriptionTotal(s.descriptionTotal),
      s.notRequiredIf.map(c => IncludeIf(BooleanExprUpdater(c.booleanExpr, index, s.allIds))),
      s.displayWidth,
      s.removePageContent.map(expand)
    )
  }

  private def mkSectionNumber(
    sn: SectionNumber.Classic,
    coordinates: Option[Coordinates]
  ): SectionNumber = coordinates.fold[SectionNumber](sn)(coordinates => SectionNumber.TaskList(coordinates, sn))

  def fastModel[U <: SectionSelectorType](
    variadiFormData: VariadicFormData,
    form: Form,
    phase: Option[FormPhase]
  )(implicit
    lang: LangADT,
    messages: Messages,
    sectionIncluder: SectionSelector[U]
  ): (FormModel, FreeCalculator, DependencyGraph) = {
    val mongoUserData = new MongoUserData(variadiFormData)

    val multiFilesData: Map[ModelComponentId, List[(FileComponentId, VariadicValue.One)]] =
      metadata.allMultiFileUploads
        .map { baseComponentId =>
          variadiFormData.filesOfMultiFileComponent(baseComponentId)
        }
        .foldLeft(Map.empty[ModelComponentId, List[(FileComponentId, VariadicValue.One)]])(_.concat(_))

    val evaluationContext = EvaluationContext(
      formTemplateId = form.formTemplateId,
      envelopeId = form.envelopeId,
      customSubmissionRef = formTemplate.customSubmissionRef,
      fileSizeLimit = FileSizeLimit(formTemplate.fileSizeLimit.getOrElse(FileSizeLimit.defaultFileLimitSize)),
      maybeAccessCode = maybeAccessCode,
      authConfig = formTemplate.authConfig,
      retrievals = retrievals,
      thirdPartyData = form.thirdPartyData,
      taskIdTaskStatus = taskIdTaskStatus,
      formStartDate = LocalDate.ofInstant(form.startDate, ZoneId.of("Europe/London")),
      lookupRegistry = lookupRegistry,
      lookupRegister = metadata.lookupRegister,
      componentIdToFileId = form.componentIdToFileId,
      multiFilesData = multiFilesData,
      formPhase = phase,
      lang = lang
    )
    val visitIndex = form.visitsIndex
    val recalculator: Recalculator =
      Recalculator.from(
        formTemplate,
        metadata,
        mongoUserData,
        visitIndex,
        evaluationContext
      )

    val freeCalculator: FreeCalculator = recalculator.recalculate()

    // recalculator.graph.pretty()

    val allSections: AllSections = sectionIncluder.getSections(formTemplate)

    val brackets: Brackets = allSections.mapSection { maybeCoordinates =>
      {
        case IndexedSection.SectionNoIndex(s) =>
          val sectionNumber =
            mkSectionNumber(SectionNumber.classicFixed, maybeCoordinates)
          Some(Bracket.NonRepeatingPage(SingletonWithNumber(Singleton(s.page), sectionNumber), s))
        case IndexedSection.SectionIndex(s: Section.NonRepeatingPage, index) =>
          val sectionNumber = mkSectionNumber(SectionNumber.Classic.NormalPage(index), maybeCoordinates)
          Some(
            Bracket
              .NonRepeatingPage(SingletonWithNumber(Singleton.expand(s.page, freeCalculator), sectionNumber), s)
          )
        case IndexedSection.SectionIndex(s: Section.RepeatingPage, index) =>
          freeCalculator.evalExpr(s.repeats).evaluationStatus match {
            case EvaluationStatus.NumberResult(repeats) =>
              val repeatingPages = (1 to repeats.toInt).map { iterationIndex =>
                val pageUpdated = PageUpdater(s.page, iterationIndex, s.allIds, s.page.allDataRetriveIds)
                SingletonWithNumber(
                  Singleton(pageUpdated),
                  mkSectionNumber(SectionNumber.Classic.RepeatedPage(index, iterationIndex - 1), maybeCoordinates)
                )
              }
              NonEmptyList.fromList(repeatingPages.toList).map { repeatingPagesNel =>
                Bracket.RepeatingPage(repeatingPagesNel, s)
              }
            case EvaluationStatus.Empty | EvaluationStatus.Hidden => Option.empty[Bracket]
            case other =>
              throw new Exception(
                s"Failed to create a repeatingPage. Expected NumberResult, Empty or Hidden but got: $other"
              )
          }

        case IndexedSection.SectionIndex(s: Section.AddToList, templateSectionIndex) =>
          val baseComponentId = s.addAnotherQuestion.id.baseComponentId

          // Find out how many times this baseComponentId appears in the data.
          val numberOfIterations: Int =
            mongoUserData.lookup.forBaseComponentId(baseComponentId).count {
              case (_, VariadicValue.Many(Seq("0"))) => true
              case (_, VariadicValue.Many(_))        => false
              case (_, one) =>
                throw new Exception(
                  s"AddToList repeater question answer must be Many, but One received: $one"
                )
            }

          val maybeIterations = (0 to numberOfIterations).map { index =>
            val iterationIndex = index + 1
            val defaultPage: Option[SingletonWithNumber] =
              s.defaultPage
                .filter(_ => iterationIndex == 1) // Only first iteration can have default page
                .map { dp =>
                  val dpSectionNumber = mkSectionNumber(
                    SectionNumber.Classic.AddToListPage.DefaultPage(templateSectionIndex),
                    maybeCoordinates
                  )
                  SingletonWithNumber(Singleton(PageUpdater(dp, 1, s.allIds, dp.allDataRetriveIds)), dpSectionNumber)
                }
            val checkYourAnswers: Option[CheckYourAnswersWithNumber] = s.cyaPage.map { cyaPage =>
              val checkYourAnswers = mkCheckYourAnswers(cyaPage, s, iterationIndex)

              val cyaSectionNumber: SectionNumber = mkSectionNumber(
                SectionNumber.Classic.AddToListPage
                  .TerminalPage(templateSectionIndex, iterationIndex, TerminalPageKind.CyaPage),
                maybeCoordinates
              )

              CheckYourAnswersWithNumber(checkYourAnswers, cyaSectionNumber)
            }

            val declarationSection = s.declarationSection.map { declarationSection =>
              val pageUpdated = PageUpdater(declarationSection.toPage, iterationIndex, s.allIds, s.allDataRetriveIds)

              val sectionNumber = mkSectionNumber(
                SectionNumber.Classic.AddToListPage
                  .TerminalPage(templateSectionIndex, iterationIndex, TerminalPageKind.DeclarationPage),
                maybeCoordinates
              )
              SingletonWithNumber(Singleton(pageUpdated), sectionNumber)
            }

            val singletons: NonEmptyList[SingletonWithNumber] = s.pages.zipWithIndex.map { case (page, pageIndex) =>
              val pageUpdated = PageUpdater(page, iterationIndex, s.allIds, page.allDataRetriveIds)

              val sectionNumber = mkSectionNumber(
                SectionNumber.Classic.AddToListPage.Page(templateSectionIndex, iterationIndex, pageIndex),
                maybeCoordinates
              )
              SingletonWithNumber(Singleton.expand(pageUpdated, freeCalculator), sectionNumber)
            }

            val repeater: Repeater = mkRepeater(s, iterationIndex)

            Bracket.AddToListIteration(
              defaultPage = defaultPage,
              singletons = singletons,
              checkYourAnswers = checkYourAnswers,
              declarationSection = declarationSection,
              repeater = RepeaterWithNumber(
                repeater,
                mkSectionNumber(
                  SectionNumber.Classic.AddToListPage
                    .TerminalPage(templateSectionIndex, iterationIndex, TerminalPageKind.RepeaterPage),
                  maybeCoordinates
                )
              )
            )
          }

          NonEmptyList
            .fromList(maybeIterations.toList)
            .fold(
              Option.empty[Bracket]
            )(iterations => Some(Bracket.AddToList(s.includeIf, iterations, s)))
      }
    }

    val formModel = new FormModel(brackets)

    (formModel, freeCalculator.withFormModelMetadata(formModel.metadata), recalculator.graph)
  }
}
