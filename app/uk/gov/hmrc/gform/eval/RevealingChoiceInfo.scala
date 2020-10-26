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

package uk.gov.hmrc.gform.eval

import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

/**
  * This is static information derived from form template.
  * It contains mapping from child of revealing choice to its parent
  * together with index it lies at.
  */
case class RevealingChoiceInfo(lookup: Map[BaseComponentId, RevealingChoiceData]) extends AnyVal {

  def ++(revealedChoiceInfo: RevealingChoiceInfo): RevealingChoiceInfo =
    RevealingChoiceInfo(lookup ++ revealedChoiceInfo.lookup)

  def isHiddenByParentId(
    formComponentId: FormComponentId,
    variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]
  ): Option[Boolean] = {
    val parentData = lookup.get(formComponentId.baseComponentId)
    parentData.map { revealingChoiceData =>
      val maybeIndex: Option[Int] = formComponentId.modelComponentId.maybeIndex
      val pureModelComponentId: ModelComponentId =
        ModelComponentId.pure(IndexedComponentId.pure(revealingChoiceData.baseComponentId))
      val modelComponentId: ModelComponentId =
        maybeIndex.fold(pureModelComponentId)(pureModelComponentId.expandWithPrefix)

      val answers: Seq[String] = variadicFormData.many(modelComponentId).toSeq.flatten

      !answers.contains(revealingChoiceData.index.toString)
    }
  }
}

object RevealingChoiceInfo {
  val empty: RevealingChoiceInfo = RevealingChoiceInfo(Map.empty[BaseComponentId, RevealingChoiceData])
}

case class RevealingChoiceData(
  index: Int, // This is index of RevealingChoiceElement inside of RevealingChoice
  baseComponentId: BaseComponentId // This is baseComponentId of RevealingChoice component
)
