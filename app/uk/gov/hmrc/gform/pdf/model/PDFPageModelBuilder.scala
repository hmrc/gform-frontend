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

package uk.gov.hmrc.gform.pdf.model

import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.pdf.model.PDFModel._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Section, SectionNumber }
import uk.gov.hmrc.gform.validation.ValidationResult

object PDFPageModelBuilder {

  def makeModel[T <: PDFType](
    formModelOptics: FormModelOptics,
    cache: AuthCacheWithForm,
    envelopeWithMapping: EnvelopeWithMapping,
    validationResult: ValidationResult,
    maybeFilterFieldIds: Option[List[BaseComponentId]]
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    pdfFunctions: PDFCustomRender[T]
  ): List[SummaryData] = {

    import pdfFunctions._

    def brackets: List[Bracket] =
      formModelOptics.formModelVisibilityOptics.formModel.brackets.fold(_.brackets)(_.allBrackets).toList
    val bracketsSorted: List[Bracket] = bracketOrdering.fold(brackets)(brackets.sorted(_))

    bracketsSorted.flatMap {
      _.fold { nonRepeatingPage =>
        buildFromSingleton(
          cache,
          envelopeWithMapping,
          nonRepeatingPage.singleton.singleton,
          nonRepeatingPage.singleton.sectionNumber,
          nonRepeatingPage.source,
          validationResult,
          formModelOptics.formModelVisibilityOptics,
          maybeFilterFieldIds
        ).toList: List[SummaryData]
      } { repeatingPage =>
        repeatingPage.singletons.toList.flatMap { case SingletonWithNumber(singleton, sectionNumber) =>
          buildFromSingleton(
            cache,
            envelopeWithMapping,
            singleton,
            sectionNumber,
            repeatingPage.source,
            validationResult,
            formModelOptics.formModelVisibilityOptics,
            maybeFilterFieldIds
          )
        }
      } { addToList =>
        val addToListTitle = addToList.source.summaryName.value()
        val addToListSummary: AddToListSummary =
          AddToListSummary(
            addToList.repeaters.last.title.value(),
            addToList.repeaters.map(r => r.expandedSummaryDescription.value()).toList
          )
        val addToListPageGroups: List[AddToListPageGroup] = addToList.iterations.toList.zipWithIndex.flatMap {
          case (iteration, index) =>
            val singletonsSorted =
              singletonWithNumberOrdering.fold(iteration.singletons.toList)(iteration.singletons.toList.sorted(_))
            val addToListPages: List[PageData] = singletonsSorted
              .flatMap { case SingletonWithNumber(singleton, sectionNumber) =>
                buildFromSingleton(
                  cache,
                  envelopeWithMapping,
                  singleton,
                  sectionNumber,
                  addToList.source,
                  validationResult,
                  formModelOptics.formModelVisibilityOptics,
                  maybeFilterFieldIds
                )
              }
            val addToListIterationTitle = iteration.repeater.repeater.expandedShortName.value()
            if (addToListPages.isEmpty) {
              None
            } else {
              Some(
                AddToListPageGroup(
                  addToListIterationTitle,
                  addToListPages,
                  addToList.source.addAnotherQuestion.id.value + index
                )
              )
            }
        }

        if (addToListPageGroups.isEmpty)
          List.empty
        else
          List(
            AddToListData(
              addToListTitle,
              addToListSummary,
              addToListPageGroups,
              addToList.source.addAnotherQuestion.id.value
            )
          )
      }
    }
  }

  private def buildFromSingleton[T <: PDFType](
    cache: AuthCacheWithForm,
    envelopeWithMapping: EnvelopeWithMapping,
    singleton: Singleton,
    sectionNumber: SectionNumber,
    source: Section,
    validationResult: ValidationResult,
    formModelVisibilityOptics: FormModelVisibilityOptics,
    maybeFilterFieldIds: Option[List[BaseComponentId]]
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    pdfFunctions: PDFCustomRender[T]
  ): Option[PageData] = {

    import pdfFunctions._

    val filteredFields = doFilter(singleton.page.fields, formModelVisibilityOptics.freeCalculator).filter(fc =>
      maybeFilterFieldIds.fold(true)(_.contains(fc.id.baseComponentId))
    )
    val pageFields: List[PageField] = formComponentOrdering
      .fold(filteredFields)(filteredFields.sorted(_))
      .flatMap(fc =>
        PDFPageFieldBuilder
          .build(fc, cache, sectionNumber, validationResult, envelopeWithMapping, formModelVisibilityOptics)
      )

    if (pageFields.isEmpty) {
      None
    } else {
      val maybePresentationHint = source.fold { _ =>
        singleton.page.presentationHint
      } { _ =>
        singleton.page.presentationHint
      } { addToList =>
        addToList.presentationHint
          .orElse(singleton.page.presentationHint)
      }
      val pageTitle = getPageTitle(singleton.page, maybePresentationHint)
      Some(PageData(pageTitle, pageFields, sectionNumber.value))
    }
  }
}
