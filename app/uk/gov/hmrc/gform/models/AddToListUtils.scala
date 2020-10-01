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

import cats.instances.int._
import cats.syntax.eq._
import cats.syntax.option._
import scala.util.Try
import uk.gov.hmrc.gform.gform.ExprUpdater
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, Expr, FormComponentId, Section }

object AddToListUtils {

  def removeRecord(
    processData: ProcessData,
    idx: Int,
    addToListId: AddToListId
  ): VariadicFormData[SourceOrigin.Current] = {
    val allAddToListPages: List[PageModel[DataExpanded]] =
      processData.formModel.pages.filter(_.isAddToList(addToListId))

    val (
      toBeRemoved: List[PageModel[DataExpanded]],
      toKeep: List[PageModel[DataExpanded]]
    ) =
      allAddToListPages.partition(pageModel =>
        pageModel.fold(s => s.page.fields.forall(_.modelComponentId.hasExpansionPrefix(idx)))(_.index === idx))

    val toBeReindex: List[PageModel[DataExpanded]] = toKeep.filter(
      pageModel =>
        pageModel
          .fold(s => s.page.fields.forall(_.modelComponentId.maybeIndex.exists(_ > idx)))(_.index > idx))

    val variadicFormData = processData.formModelOptics.pageOpticsData

    val toReindex: Set[ModelComponentId] = toBeReindex.flatMap(_.allModelComponentIds).toSet
    val variadicFormDataToModify = variadicFormData.subset(toReindex)

    val variadicFormDataToModified = variadicFormDataToModify.mapKeys { variadicValueId =>
      variadicValueId.decrement
    }

    val toBeRemovedIds: List[ModelComponentId] = toBeRemoved.flatMap(_.allModelComponentIds)

    val res = variadicFormData -- toBeRemovedIds -- variadicFormDataToModify ++ variadicFormDataToModified

    val prefixes: Set[Int] = res.keySet.flatMap(_.maybeIndex)

    val maxPrefix = prefixes.max

    val lastAddAnotherQuestionId = addToListId.formComponentId.modelComponentId.expandWithPrefix(maxPrefix)

    res - lastAddAnotherQuestionId

  }

}
