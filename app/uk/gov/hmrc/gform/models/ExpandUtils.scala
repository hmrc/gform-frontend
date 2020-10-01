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
import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.foldable._
import scala.util.Try
import uk.gov.hmrc.gform.gform.{ ExprUpdater, FormComponentUpdater }
import uk.gov.hmrc.gform.lookup.LookupExtractors
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId, MultiValueId }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object ExpandUtils {

  private val NumericPrefix = "^(\\d+)_.*".r

  private val NumericPrefixWithId = "^(\\d+)_(.*)".r

  // format: off
  private val AtomicOnly     =        "^([^_]*)-(.*)$".r
  private val ExpandedAtomic = "^(\\d+)_([^-]*)-(.*)$".r
  private val ExpandedOnly   = "^(\\d+)_([^-]*)$".r
  // format: on

  // format: off
  def toModelComponentId(string: String): ModelComponentId = string match {
    case AtomicOnly(base, atom) =>            ModelComponentId.atomic(IndexedComponentId.pure(BaseComponentId(base)), Atom(atom))
    case ExpandedAtomic(index, base, atom) => ModelComponentId.atomic(IndexedComponentId.indexed(BaseComponentId(base), index.toInt), Atom(atom))
    case ExpandedOnly(index, base) =>         ModelComponentId.pure(IndexedComponentId.indexed(BaseComponentId(base), index.toInt))
    case _ =>                                 ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId(string)))
  }
  // format: on

  def modelComponentIdFromFormComponentId(formComponentId: FormComponentId): ModelComponentId =
    toModelComponentId(formComponentId.value)

  def expandSmartString(smartString: SmartString, index: Int, ids: List[FormComponentId]): SmartString = {
    val interpolations: List[Expr] = smartString.interpolations.map(expr => ExprUpdater(expr, index, ids))
    smartString.copy(interpolations = interpolations).replace("$n", index.toString)
  }

  def expandGroup[S <: SourceOrigin](fc: FormComponent, group: Group, data: VariadicFormData[S]): List[FormComponent] =
    (1 to group.repeatsMax.getOrElse(1)).toList.flatMap { index =>
      val allIds = fc.id :: group.fields.map(_.id)
      val fcUpdated = new FormComponentUpdater(fc, index, allIds).updatedWithId
      val exp = fc.modelComponentId.expandWithPrefix(index)
      val toExpand: Boolean = data.contains(exp)
      if (toExpand || index === 1) List(fcUpdated) else Nil
    }

  def submittedFCs[D <: DataOrigin](
    formModelOptics: FormModelOptics[D],
    formComponents: List[FormComponent]
  ): List[FormComponent] = {
    val atomicFcIds: Set[ModelComponentId] = formModelOptics.pageOpticsData.keySet

    formComponents.filter { fc =>
      fc.multiValueId.atomsModelComponentIds.forall(atomicFcIds)
    }
  }

  def getAlwaysEmptyHidden(
    pageModel: PageModel[DataExpanded],
    lookupExtractors: LookupExtractors
  ): List[FormComponent] =
    pageModel.allFormComponents.filter {
      case IsChoice(_)                       => false // true
      case IsGroup(_)                        => true
      case lookupExtractors.IsRadioLookup(_) => true
      case _                                 => false
    }

  def findFormComponent(targetFcId: FormComponentId, formModel: FormModel[DataExpanded]): Option[FormComponent] =
    formModel.allFormComponents.find(_.id === targetFcId)

  private def addPrefixToString(n: Int, str: String): String =
    str match {
      case NumericPrefix(_) => str
      case _                => n + "_" + str
    }

  def addPrefix(n: Int, targetFcId: FormComponentId): FormComponentId =
    FormComponentId(addPrefixToString(n, targetFcId.value))

}
