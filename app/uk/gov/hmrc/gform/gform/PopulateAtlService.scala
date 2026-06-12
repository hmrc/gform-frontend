/*
 * Copyright 2026 HM Revenue & Customs
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

import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieveResult, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, Section, SectionNumber, TemplateSectionIndex }
import uk.gov.hmrc.gform.sharedmodel.PopulateATL

case class PopulateAtlData(
  addToList: Section.AddToList,
  atlCount: Int,
  atlSectionNumber: SectionNumber,
  fields: Seq[FormField],
  baseIds2: Set[BaseComponentId]
)

object PopulateAtlService {
  def getPopulateAtlData(
    populateATL: PopulateATL,
    dataRetrieveResult: DataRetrieveResult,
    formModelVisibilityOptics: FormModelVisibilityOptics
  )(implicit messages: Messages): PopulateAtlData = {
    val addToList: Section.AddToList = formModelVisibilityOptics.formModel.addToListBrackets
      .collectFirst { case atl if atl.source.id.formComponentId == populateATL.id.formComponentId => atl.source }
      .getOrElse(throw new RuntimeException(s"Could not find an ATL with id ${populateATL.id}"))

    val defaultPageFieldsValues: List[FormField] = addToList.defaultPage.toList
      .flatMap(page => page.fields)
      .map(fc => FormField(fc.id.modelComponentId.expandWithPrefix(1), ""))

    val addAnotherQuestionBaseComponentId = addToList.addAnotherQuestion.baseComponentId

    val sectionNumber: SectionNumber =
      formModelVisibilityOptics.formModel.sectionNumberLookup(addToList.addAnotherQuestion.id.withIndex(1))

    val fields: List[FormField] = populateATL.mapping.toList.flatMap { case (atlComponentName, expr) =>
      val atlValues: List[String] = formModelVisibilityOptics.freeCalculator.evalExpr(expr).listRepresentation
      val bcId = atlComponentName.baseComponentId

      atlValues.zipWithIndex
        .map { case (value, i) =>
          val modelComponentId = ModelComponentId.pure(IndexedComponentId.indexed(bcId, i + 1))
          formModelVisibilityOptics.freeCalculator.addAnswer(modelComponentId, VariadicValue.One(value)) // Side effect
          FormField(modelComponentId, value)
        }

    }

    val atlCount = fields.flatMap(_.id.maybeIndex).max

    val addAnotherQuestionValues: List[FormField] = (1 until atlCount).toList.map { i =>
      val modelComponentId = ModelComponentId.pure(IndexedComponentId.indexed(addAnotherQuestionBaseComponentId, i))
      formModelVisibilityOptics.freeCalculator.addAnswer(modelComponentId, VariadicValue.Many(Seq("0"))) // Side effect
      FormField(modelComponentId, "0")
    }

    val mappingKeys: Set[BaseComponentId] = populateATL.mapping.keys.map(_.baseComponentId).toSet
    val baseIds: Set[BaseComponentId] = mappingKeys ++ Set(addAnotherQuestionBaseComponentId)

    PopulateAtlData(
      addToList,
      atlCount,
      sectionNumber,
      defaultPageFieldsValues ++ addAnotherQuestionValues ++ fields,
      baseIds
    )

  }

  def updateCache(
    authCacheWithForm: AuthCacheWithForm,
    populateAtlDatas: List[PopulateAtlData],
    visitsIndex: VisitIndex,
    maybeCoordinates: Option[Coordinates],
    variadicFormData: VariadicFormData
  )(implicit
    lang: LangADT,
    messages: Messages
  ): (AuthCacheWithForm, Option[FormModelOptics]) =
    if (populateAtlDatas.isEmpty) {
      (authCacheWithForm, Option.empty[FormModelOptics])
    } else {
      val populateAtlFields = populateAtlDatas.flatMap(_.fields)
      val populateAtlFormData = FormData(populateAtlFields.toList)
      val oldPopulateAtlFieldsToClean = populateAtlDatas.flatMap(_.baseIds2).toSet
      val oldData = authCacheWithForm.variadicFormData
      val oldDataWithoutPopulateAtl = oldPopulateAtlFieldsToClean.foldLeft(oldData) { case (acc, bcId) =>
        acc.forBaseComponentId(bcId).foldLeft(acc) { case (acc, (mcId, value)) =>
          acc - mcId
        }
      }

      val formDataU: FormData = oldDataWithoutPopulateAtl.toFormData ++ populateAtlFormData

      val newVisitIndicesClassic: List[SectionNumber.Classic] = populateAtlDatas.flatMap { populateAtlData =>
        val atl = populateAtlData.addToList
        val templateSectionIndex = populateAtlData.atlSectionNumber.templateSectionIndex

        (1 to populateAtlData.atlCount).toList.flatMap { iterationIndex =>
          val defaultPage: List[SectionNumber.Classic] =
            if (
              atl.defaultPage.isDefined && iterationIndex == 1 && visitsIndex.existsSectionIndex(templateSectionIndex)
            ) {
              SectionNumber.Classic.AddToListPage.DefaultPage(templateSectionIndex) :: Nil
            } else Nil

          val cyaPages: List[SectionNumber.Classic] = if (atl.cyaPage.isDefined) {
            val kind = SectionNumber.Classic.AddToListPage.TerminalPageKind.CyaPage
            SectionNumber.Classic.AddToListPage.TerminalPage(templateSectionIndex, iterationIndex, kind) :: Nil
          } else Nil

          val pages: List[SectionNumber.Classic] = (0 to atl.pages.size).toList.map { pageIndex =>
            SectionNumber.Classic.AddToListPage
              .Page(populateAtlData.atlSectionNumber.templateSectionIndex, iterationIndex, pageIndex)
          }

          val repeaterPages: List[SectionNumber.Classic] = {
            val kind = SectionNumber.Classic.AddToListPage.TerminalPageKind.RepeaterPage
            SectionNumber.Classic.AddToListPage.TerminalPage(templateSectionIndex, iterationIndex, kind) :: Nil
          }

          (defaultPage ++ pages ++ cyaPages ++ repeaterPages)
        }
      }

      val templateSectionIndices: List[TemplateSectionIndex] = populateAtlDatas.map { populateAtlData =>
        populateAtlData.atlSectionNumber.templateSectionIndex
      }

      val visitsIndexCleaned =
        templateSectionIndices.foldLeft(visitsIndex)((acc, sectionIndex) => acc.unvisitAllSectionIndex(sectionIndex))

      val newVisitIndices: List[SectionNumber] = maybeCoordinates.fold[List[SectionNumber]](newVisitIndicesClassic) {
        coordinates =>
          newVisitIndicesClassic.map(classic => SectionNumber.TaskList(coordinates, classic))
      }

      val visitsIndexUpd = newVisitIndices.foldLeft(visitsIndexCleaned)(_.visit(_))

      val authCacheWithFormUpd = authCacheWithForm.copy(
        form = authCacheWithForm.form
          .copy(
            formData = formDataU,
            visitsIndex = visitsIndexUpd
          )
      )

      val updFormModelOptics = FormModelOptics
        .mkFormModelOptics[SectionSelectorType.Normal](
          variadicFormData,
          authCacheWithFormUpd
        )
      (authCacheWithFormUpd, Some(updFormModelOptics))
    }

}
