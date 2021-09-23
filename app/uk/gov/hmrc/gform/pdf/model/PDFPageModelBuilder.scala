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

package uk.gov.hmrc.gform.pdf.model

import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, _ }
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Section, SectionNumber }
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.gform.pdf.model.PDFModel._

object PDFPageModelBuilder {

  def makeModel[D <: DataOrigin, T <: PDFType](
    formModelOptics: FormModelOptics[D],
    cache: AuthCacheWithForm,
    envelopeWithMapping: EnvelopeWithMapping,
    validationResult: ValidationResult
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    pdfFunctions: PDFCustomRender[T]
  ): List[SummaryData] = {

    import pdfFunctions._

    val brackets = formModelOptics.formModelVisibilityOptics.formModel.brackets.brackets.toList
    val bracketsSorted: List[Bracket[Visibility]] = bracketOrdering.fold(brackets)(brackets.sorted(_))

    bracketsSorted.flatMap {
      _.fold { nonRepeatingPage =>
        buildFromSingleton(
          cache,
          envelopeWithMapping,
          nonRepeatingPage.singleton,
          nonRepeatingPage.sectionNumber,
          nonRepeatingPage.source,
          validationResult
        ).toList: List[SummaryData]
      } { repeatingPage =>
        repeatingPage.singletons.toList.flatMap { case SingletonWithNumber(singleton, sectionNumber) =>
          buildFromSingleton(
            cache,
            envelopeWithMapping,
            singleton,
            sectionNumber,
            repeatingPage.source,
            validationResult
          )
        }
      } { addToList =>
        val addToListTitle = addToList.source.summaryName.value()
        val addToListSummary: AddToListSummary =
          AddToListSummary(
            addToList.repeaters.last.title.value(),
            addToList.repeaters.map(_.expandedDescription.value()).toList
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
                  validationResult
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

  private def buildFromSingleton[A <: PageMode, T <: PDFType](
    cache: AuthCacheWithForm,
    envelopeWithMapping: EnvelopeWithMapping,
    singleton: Singleton[A],
    sectionNumber: SectionNumber,
    source: Section,
    validationResult: ValidationResult
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    pdfFunctions: PDFCustomRender[T]
  ): Option[PageData] = {

    import pdfFunctions._

    val filteredFields = doFilter(singleton.page.fields)
    val pageFields: List[PageField] = formComponentOrdering
      .fold(filteredFields)(filteredFields.sorted(_))
      .map(fc => PDFPageFieldBuilder.build(fc, cache, sectionNumber, validationResult, envelopeWithMapping))
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
      Option(PageData(pageTitle, pageFields, sectionNumber.value.toString))
    }
  }
}
