/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.models.{ Bracket, Brackets, CheckYourAnswers, CheckYourAnswersWithNumber, FormModel, PageModel, Repeater, RepeaterWithNumber, SingletonWithNumber, TaskModel }
import uk.gov.hmrc.gform.sharedmodel.{ LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AtlDescription, CheckYourAnswersPage, Choice, ComponentType, DeclarationSection, ExitPage, FormComponent, FormTemplate, Group, InformationMessage, Instruction, MiniSummaryList, MiniSummaryListValue, MiniSummaryRow, MultiFileUpload, OptionData, Page, PdfCxt, PostcodeLookup, RevealingChoice, RevealingChoiceElement, Section, SummarySection, TableComp, TableHeadCell, TableValue, TableValueRow, Task, TaskSection, Text }

object SmartStringCollector {

  def collectSmartStrings(formModel: FormModel): List[(SmartString, Boolean)] =
    smartStringForBracketsWithSectionNumber(formModel.brackets)

  def collectSmartStrings(formTemplate: FormTemplate): List[(SmartString, Boolean)] =
    formTemplate.formKind.fold(classic => classic.sections.flatMap(smartStringForSection))(taskList =>
      taskList.sections.toList.flatMap(smartStringForTaskSection)
    ) ++
      formTemplate.exitPages.toList.flatMap(_.toList.flatMap(smartStringForExitPage))

  private def smartStringForBracketsWithSectionNumber(
    bracketsWithSectionNumber: Brackets
  ): List[(SmartString, Boolean)] =
    bracketsWithSectionNumber.fold(classic => classic.brackets.toList.flatMap(smartStringForBracket))(taskList =>
      taskList.brackets.toList.flatMap { case (_, taskModel) => smartStringForTaskModel(taskModel) }
    )

  private def smartStringForTaskModel(taskModel: TaskModel): List[(SmartString, Boolean)] =
    taskModel.fold(allHidden => List.empty[(SmartString, Boolean)])(editable =>
      editable.brackets.toList.flatMap(smartStringForBracket)
    )

  private def smartStringForBracket(bracket: Bracket): List[(SmartString, Boolean)] =
    bracket.fold(nonRepeatingPage => smartStringForSingletonWithNumber(nonRepeatingPage.singleton))(repeatingPage =>
      repeatingPage.singletons.toList.flatMap(smartStringForSingletonWithNumber)
    )(addToList => addToList.iterations.toList.flatMap(smartStringForIteration))

  private def smartStringForIteration(addToListIteration: Bracket.AddToListIteration): List[(SmartString, Boolean)] =
    addToListIteration.defaultPage.toList.flatMap(smartStringForSingletonWithNumber) ++
      addToListIteration.singletons.toList.flatMap(smartStringForSingletonWithNumber) ++
      addToListIteration.checkYourAnswers.toList.flatMap(smartStringForCheckYourAnswersWithNumber) ++
      addToListIteration.declarationSection.toList.flatMap(smartStringForSingletonWithNumber) ++
      smartStringForRepeaterWithNumber(addToListIteration.repeater)

  private def smartStringForRepeaterWithNumber(repeaterWithNumber: RepeaterWithNumber): List[(SmartString, Boolean)] =
    smartStringForPageModel(repeaterWithNumber.repeater)

  private def smartStringForRepeater(repeater: Repeater): List[(SmartString, Boolean)] =
    List(
      Some(repeater.expandedTitle              -> false),
      repeater.expandedCaption.map(_           -> false),
      repeater.expandedNoPIITitle.map(_        -> false),
      Some(repeater.expandedSummaryDescription -> false),
      Some(repeater.expandedShortName          -> false),
      Some(repeater.expandedSummaryName        -> false),
      repeater.expandedRemovePageContent.map(_ -> false)
    ).flatten ++
      smartStringForAtlDescription(repeater.expandedDescription) ++
      smartStringForFormComponent(repeater.addAnotherQuestion) ++
      repeater.fields.toList.flatMap(_.toList.flatMap(smartStringForFormComponent)) ++
      repeater.expandedDescriptionTotal.toList.flatMap(smartStringForAtlDescription)

  private def smartStringForCheckYourAnswersWithNumber(
    checkYourAnswersWithNumber: CheckYourAnswersWithNumber
  ): List[(SmartString, Boolean)] =
    smartStringForPageModel(checkYourAnswersWithNumber.checkYourAnswers)

  private def smartStringForCheckYourAnswers(checkYourAnswers: CheckYourAnswers): List[(SmartString, Boolean)] =
    List(
      checkYourAnswers.expandedTitle.map(_            -> false),
      checkYourAnswers.expandedCaption.map(_          -> false),
      Some(checkYourAnswers.expandedUpdateTitle       -> false),
      checkYourAnswers.expandedNoPIITitle.map(_       -> false),
      checkYourAnswers.expandedNoPIIUpdateTitle.map(_ -> false),
      checkYourAnswers.expandedHeader.map(_           -> false),
      checkYourAnswers.expandedFooter.map(_           -> false),
      checkYourAnswers.expandedContinueLabel.map(_    -> false)
    ).flatten ++
      checkYourAnswers.fields.toList.flatMap(_.toList.flatMap(smartStringForFormComponent))

  private def smartStringForSingletonWithNumber(
    singletonWithNumber: SingletonWithNumber
  ): List[(SmartString, Boolean)] =
    smartStringForPageModel(singletonWithNumber.singleton)

  private def smartStringForPageModel(pageModel: PageModel): List[(SmartString, Boolean)] =
    pageModel.fold(singleton => smartStringForPage(singleton.page))(checkYourAnswers =>
      smartStringForCheckYourAnswers(checkYourAnswers)
    )(repeater => smartStringForRepeater(repeater))

  private def smartStringForExitPage(exitPage: ExitPage): List[(SmartString, Boolean)] =
    List(exitPage.label -> false, exitPage.exitMessage -> false)

  private def smartStringForTaskSection(taskSection: TaskSection): List[(SmartString, Boolean)] =
    taskSection.title -> false :: taskSection.tasks.toList.flatMap(smartStringForTask)

  private def smartStringForTask(task: Task): List[(SmartString, Boolean)] =
    List(
      Some(task.title    -> false),
      task.caption.map(_ -> false)
    ).flatten ++
      task.sections.toList.flatMap(smartStringForSection) ++
      task.summarySection.toList.flatMap(smartStringForSummarySection) ++
      task.declarationSection.toList.flatMap(smartStringForDeclarationSection)

  private def smartStringForSummarySection(summarySection: SummarySection): List[(SmartString, Boolean)] =
    List(
      Some(summarySection.title          -> false),
      summarySection.caption.map(_       -> false),
      Some(summarySection.header         -> false),
      Some(summarySection.footer         -> false),
      summarySection.continueLabel.map(_ -> false)
    ).flatten ++
      summarySection.fields.toList.flatMap(_.toList.flatMap(smartStringForFormComponent)) ++
      summarySection.pdf.toList.flatMap(smartStringForPdfCtx)

  private def smartStringForPdfCtx(pdfCxt: PdfCxt): List[(SmartString, Boolean)] =
    List(
      pdfCxt.header.map(_ -> true),
      pdfCxt.footer.map(_ -> true)
    ).flatten

  private def smartStringForSection(section: Section): List[(SmartString, Boolean)] =
    section.fold(nonRepeatingPage => smartStringForPage(nonRepeatingPage.page))(repeatingPage =>
      smartStringForPage(repeatingPage.page)
    )(addToList =>
      List(
        Some(addToList.title                  -> false),
        addToList.caption.map(_               -> false),
        addToList.noPIITitle.map(_            -> false),
        Some(addToList.summaryDescription     -> false),
        Some(addToList.shortName              -> false),
        Some(addToList.summaryName            -> false),
        addToList.repeaterContinueLabel.map(_ -> false),
        addToList.infoMessage.map(_           -> false),
        addToList.errorMessage.map(_          -> false),
        addToList.removePageContent.map(_     -> false)
      ).flatten ++
        addToList.pages.toList.flatMap(smartStringForPage) ++
        smartStringForFormComponent(addToList.addAnotherQuestion) ++
        smartStringForAtlDescription(addToList.description) ++
        addToList.descriptionTotal.toList.flatMap(smartStringForAtlDescription) ++
        addToList.instruction.toList.flatMap(smartStringForInstruction) ++
        addToList.fields.toList.flatMap(_.toList.flatMap(smartStringForFormComponent)) ++
        addToList.defaultPage.toList.flatMap(smartStringForPage) ++
        addToList.cyaPage.toList.flatMap(smartStringCheckYourAnswersPage) ++
        addToList.declarationSection.toList.flatMap(smartStringForDeclarationSection)
    )

  private def smartStringForDeclarationSection(declarationSection: DeclarationSection): List[(SmartString, Boolean)] =
    List(
      Some(declarationSection.title          -> false),
      declarationSection.caption.map(_       -> false),
      declarationSection.noPIITitle.map(_    -> false),
      declarationSection.description.map(_   -> false),
      declarationSection.shortName.map(_     -> false),
      declarationSection.continueLabel.map(_ -> false),
      declarationSection.caption.map(_       -> false)
    ).flatten ++ declarationSection.fields.flatMap(smartStringForFormComponent)

  private def smartStringCheckYourAnswersPage(
    checkYourAnswersPage: CheckYourAnswersPage
  ): List[(SmartString, Boolean)] =
    List(
      checkYourAnswersPage.title.map(_            -> false),
      checkYourAnswersPage.caption.map(_          -> false),
      Some(checkYourAnswersPage.updateTitle       -> false),
      checkYourAnswersPage.noPIITitle.map(_       -> false),
      checkYourAnswersPage.noPIIUpdateTitle.map(_ -> false),
      checkYourAnswersPage.header.map(_           -> false),
      checkYourAnswersPage.footer.map(_           -> false),
      checkYourAnswersPage.continueLabel.map(_    -> false)
    ).flatten ++ checkYourAnswersPage.fields.toList.flatMap(_.toList.flatMap(smartStringForFormComponent))

  private def smartStringForInstruction(instruction: Instruction): List[(SmartString, Boolean)] =
    instruction.name.map(_ -> false).toList

  private def smartStringForAtlDescription(atlDescription: AtlDescription): List[(SmartString, Boolean)] =
    atlDescription match {
      case s: AtlDescription.SmartStringBased => List(s.value -> true)
      case k: AtlDescription.KeyValueBased    => List(k.key -> false, k.value -> true)
    }

  private def smartStringForOptionData(optionData: OptionData): List[(SmartString, Boolean)] =
    optionData match {
      case i: OptionData.IndexBased =>
        List(Some(i.label -> false), i.hint.map(_ -> false), i.summaryValue.map(_ -> false)).flatten
      case v: OptionData.ValueBased =>
        List(
          Some(v.label         -> false),
          v.hint.map(_         -> false),
          v.summaryValue.map(_ -> false),
          v.keyWord.map(_      -> false)
        ).flatten
    }

  private def smartStringForRevealingChoiceElement(
    revealingChoiceElement: RevealingChoiceElement
  ): List[(SmartString, Boolean)] =
    smartStringForOptionData(revealingChoiceElement.choice) ++
      revealingChoiceElement.revealingFields.flatMap(smartStringForFormComponent) ++
      revealingChoiceElement.hint.map(_ -> false).toList

  private def smartStringForMiniSummaryRow(miniSummaryRow: MiniSummaryRow): List[(SmartString, Boolean)] =
    miniSummaryRow match {
      case v: MiniSummaryRow.ValueRow =>
        val valueString = v.value match {
          case MiniSummaryListValue.AnyExpr(expr) =>
            val localised: LocalisedString = toLocalisedString("{0}")
            List(SmartString(localised, List(expr)) -> false)
          case MiniSummaryListValue.Reference(formCtx) => List.empty[(SmartString, Boolean)]
        }
        v.key.map(_ -> false).toList ++ valueString
      case s: MiniSummaryRow.SmartStringRow => List(Some(s.value -> false), s.key.map(_ -> false)).flatten
      case h: MiniSummaryRow.HeaderRow      => List(h.header -> false)
      case a: MiniSummaryRow.ATLRow         => a.rows.flatMap(smartStringForMiniSummaryRow)
    }

  private def smartStringForTableHeadCell(tableHeadCell: TableHeadCell): List[(SmartString, Boolean)] = List(
    tableHeadCell.label -> false
  )

  private def smartStringForTableValue(tableValue: TableValue): List[(SmartString, Boolean)] = List(
    tableValue.value -> false
  )

  private def smartStringForTableValueRow(tableValueRow: TableValueRow): List[(SmartString, Boolean)] =
    tableValueRow.values.flatMap(smartStringForTableValue)

  private def smartStringForComponentType(ct: ComponentType): List[(SmartString, Boolean)] =
    ct match {
      case i: InformationMessage => List(Some(i.infoText -> true), i.summaryValue.map(_ -> false)).flatten
      case t: Text               => List(t.prefix.map(_ -> false), t.suffix.map(_ -> false)).flatten
      case p: PostcodeLookup =>
        List(
          p.chooseAddressLabel.map(_  -> false),
          p.confirmAddressLabel.map(_ -> false),
          p.enterAddressLabel.map(_   -> false)
        ).flatten
      case c: Choice =>
        c.options.toList.flatMap(smartStringForOptionData) ++
          c.hints.toList.flatMap(_.map(_ -> false).toList) ++
          c.optionHelpText.toList.flatMap(_.map(_ -> true).toList) ++
          List(c.dividerText -> false)
      case r: RevealingChoice =>
        r.options.flatMap(smartStringForRevealingChoiceElement)
      case g: Group =>
        g.fields.flatMap(smartStringForFormComponent) ++
          List(g.repeatLabel.map(_ -> false), g.repeatAddAnotherText.map(_ -> false)).flatten
      case m: MultiFileUpload =>
        List(m.hint.map(_ -> false), m.uploadAnotherLabel.map(_ -> false), m.continueText.map(_ -> false)).flatten
      case m: MiniSummaryList => m.rows.flatMap(smartStringForMiniSummaryRow)
      case t: TableComp =>
        t.header.flatMap(smartStringForTableHeadCell) ++
          t.rows.flatMap(smartStringForTableValueRow) ++
          List(t.summaryValue -> false)
      case _ => List.empty[(SmartString, Boolean)]
    }

  private def smartStringForFormComponent(fc: FormComponent): List[(SmartString, Boolean)] =
    List(
      if (fc.isPageHeading) Option.empty[(SmartString, Boolean)] else Some(fc.label -> false),
      fc.helpText.map(_            -> true),
      fc.shortName.map(_           -> false),
      fc.errorMessage.map(_        -> false),
      fc.errorShortName.map(_      -> false),
      fc.errorShortNameStart.map(_ -> false),
      fc.errorExample.map(_        -> false)
    ).flatten ++ smartStringForComponentType(fc.`type`)

  private def smartStringForPage(page: Page): List[(SmartString, Boolean)] =
    List(
      Some(page.title          -> false),
      page.noPIITitle.map(_    -> false),
      page.description.map(_   -> false),
      page.shortName.map(_     -> false),
      page.caption.map(_       -> false),
      page.continueLabel.map(_ -> false)
    ).flatten ++ page.fields.flatMap(smartStringForFormComponent)

}
