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

import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.models.SectionSelector
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DataRetrieveCtx, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.{ PopulateATL, RetrieveDataType, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.http.HeaderCarrier

case class PopulateAtlData(fields: Seq[FormField], baseIds: Set[BaseComponentId])

object PopulateAtlService {
  def getPopulateAtlData(
    populateATL: PopulateATL,
    formModelVisibilityOptics: FormModelVisibilityOptics[_]
  ): (PopulateAtlData, VariadicFormData[SourceOrigin.Current]) = {
    val seq = populateATL.mapping.toSeq

    val fields = seq.map { case (atlComponentName, expr) =>
      val bcId = atlComponentName.baseComponentId

      val atlValues = expr match {
        case DataRetrieveCtx(dataRetrieveId, attribute) =>
          val dataRetrieveData =
            formModelVisibilityOptics.recalculationResult.evaluationContext.thirdPartyData.dataRetrieve.get
              .getOrElse(
                dataRetrieveId,
                throw new RuntimeException("Could not retrieve dataRetrieve data for populateATL")
              )
          dataRetrieveData.data match {
            case RetrieveDataType.ObjectType(data) => Seq(data(attribute))
            case RetrieveDataType.ListType(data)   => data.map(attrToValue => attrToValue(attribute))
          }
        case _ => throw new RuntimeException(s"$expr did not match for populateATL evaluation")
      }

      val atlId = populateATL.id.formComponentId.withIndex(1)
      val addAnotherQuestionFormComponent = formModelVisibilityOptics.formModel.fcLookup(atlId)
      val defaultPageBcs = formModelVisibilityOptics.formModel.addToListBrackets
        .collectFirst { case atl if atl.source.id.formComponentId == populateATL.id.formComponentId => atl.source }
        .getOrElse(throw new RuntimeException(s"Could not find an ATL with id ${populateATL.id}"))
        .defaultPage
        .toList
        .flatMap(page => page.fields)
        .map(_.id.baseComponentId)

      val defaultPageFormFields = defaultPageBcs.map { bc =>
        FormField(
          ModelComponentId.pure(IndexedComponentId.indexed(bc, 1)),
          ""
        )
      }

      val addAnotherQuestionBaseComponentId = addAnotherQuestionFormComponent.baseComponentId

      val atlsToPopulateCount = atlValues.length
      val defaultAndAAQFormFields = (1 until atlsToPopulateCount).foldLeft(
        defaultPageFormFields
      ) { case (acc, i) =>
        acc :+ FormField(
          ModelComponentId.pure(IndexedComponentId.indexed(addAnotherQuestionBaseComponentId, i)),
          "0"
        )
      }
      (
        defaultAndAAQFormFields ++ atlValues.zipWithIndex
          .map { case (value, i) =>
            FormField(
              ModelComponentId.pure(IndexedComponentId.indexed(bcId, i + 1)),
              value
            )
          },
        Set(bcId, addAnotherQuestionBaseComponentId)
      )

    }

    val populateAtlData = fields.foldLeft(PopulateAtlData(Seq(), Set())) {
      case (PopulateAtlData(accFields, seq), (fields, baseIds)) =>
        PopulateAtlData(accFields ++ fields, baseIds ++ seq)
    }

    val updatedVariadicFormData = populateAtlData.fields
      .foldLeft(formModelVisibilityOptics.recData.variadicFormData) { case (acc, field) =>
        acc.addMany(field.id -> Seq(field.value))
      }

    populateAtlData -> updatedVariadicFormData
  }

  def updateCache[T <: DataOrigin](
    authCacheWithForm: AuthCacheWithForm,
    populateAtlData: Seq[PopulateAtlData],
    visOptics: FormModelVisibilityOptics[T],
    visitsIndex: VisitIndex
  )(implicit hc: HeaderCarrier): AuthCacheWithForm = {
    val populateAtlFields = populateAtlData.flatMap(_.fields)
    val populateAtlFormData = FormData(populateAtlFields.toList)
    val oldPopulateAtlFieldsToClean = populateAtlData.flatMap(_.baseIds).toSet
    val oldData = authCacheWithForm.variadicFormData(SectionSelector.normal, implicitly)
    val oldDataWithoutPopulateAtl = oldPopulateAtlFieldsToClean.foldLeft(oldData) { case (acc, bcId) =>
      acc.forBaseComponentId(bcId).foldLeft(acc) { case (acc, (mcId, value)) =>
        acc.-(mcId)
      }
    }

    val formDataU = oldDataWithoutPopulateAtl.toFormData ++ populateAtlFormData

    val populateAtlDataWithTemplateIndex = populateAtlData.map { populateAtlData =>
      val fm = visOptics.formModel
      populateAtlData.fields
        .flatMap { case FormField(mcId, value) =>
          fm.sectionNumberLookup.get(mcId.toFormComponentId).map(_.templateSectionIndex)
        }
        .headOption
        .head
    }

    val visitedPopulateAtlPagesVisitsIndex =
      populateAtlDataWithTemplateIndex.foldLeft(visitsIndex) { case (acc, atlSection) =>
        val fm = visOptics.formModel
        fm.addToListSectionNumbers.foldLeft(acc) { case (visitIndexAcc, sectionNumber) =>
          sectionNumber match {
            case classic: SectionNumber.Classic if classic.sectionIndex == atlSection =>
              visitIndexAcc.visit(classic)
            case SectionNumber.TaskList(coordinates, classic) if classic.sectionIndex == atlSection =>
              visitIndexAcc.visit(classic)
            case _ => visitIndexAcc
          }
        }
      }

    authCacheWithForm.copy(
      form = authCacheWithForm.form
        .copy(
          formData = formDataU,
          visitsIndex = visitedPopulateAtlPagesVisitsIndex
        )
    )
  }

}
