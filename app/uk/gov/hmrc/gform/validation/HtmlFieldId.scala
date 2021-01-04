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

package uk.gov.hmrc.gform.validation

import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

sealed trait HtmlFieldId extends Product with Serializable {
  def fold[B](f: HtmlFieldId.Pure => B)(g: HtmlFieldId.Indexed => B): B = this match {
    case t: HtmlFieldId.Pure    => f(t)
    case t: HtmlFieldId.Indexed => g(t)
  }

  def toHtmlId: String =
    fold(
      pure => pure.modelComponentId.toMongoIdentifier
    )(
      indexed => indexed.formComponentId.value + indexed.index
    )
}

object HtmlFieldId {
  def pure(modelComponentId: ModelComponentId): HtmlFieldId = Pure(modelComponentId)
  def indexed(formComponentId: FormComponentId, index: Int): HtmlFieldId = Indexed(formComponentId, index)
  // This is used for fields with atomic ids, see getOptionalCurrentValue
  case class Pure(modelComponentId: ModelComponentId) extends HtmlFieldId
  // This is used for choice, to determine what field is checked and which field is not, see getOptionalCurrentValue
  case class Indexed(formComponentId: FormComponentId, index: Int) extends HtmlFieldId
}
