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

package uk.gov.hmrc.gform.models.optics

import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel, PageModel, Visibility }
import uk.gov.hmrc.gform.models.ids.{ IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, SectionNumber }
import uk.gov.hmrc.gform.testonly.RevealingChoiceLookup

case class FormModelRenderPageOptics[D <: DataOrigin](
  formModel: FormModel[DataExpanded],
  recData: RecData[SourceOrigin.Current]
) {
  def allFormComponents: List[FormComponent] = formModel.allFormComponents

  def allFormComponentIds: List[FormComponentId] = allFormComponents.map(_.id)

  def allFormComponentsExceptFromPage(pageModel: PageModel[DataExpanded]): List[FormComponent] =
    formModel.allFormComponentsExceptFromPage(pageModel)

  def find(modelComponentId: ModelComponentId): Option[FormComponent] = formModel.find(modelComponentId)

  def findBigger(indexedComponentId: IndexedComponentId): List[FormComponent] = formModel.findBigger(indexedComponentId)

  val rcLookup: RevealingChoiceLookup = RevealingChoiceLookup(formModel)

  def toFormField(modelComponentId: ModelComponentId): FormField =
    recData.variadicFormData.toFormField(modelComponentId)

}
