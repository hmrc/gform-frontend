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

package uk.gov.hmrc.gform.models

import play.api.mvc.Result
import uk.gov.hmrc.gform.models.ids.ModelPageId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Confirmation, SectionNumber }

sealed trait ConfirmationPage

object ConfirmationPage {
  final case class Confirmee(confirmedBySectionNumber: SectionNumber) extends ConfirmationPage
  final case class Confirmator(confirmation: Confirmation) extends ConfirmationPage {
    val modelPageId: ModelPageId = confirmation.pageId.modelPageId
  }
  case object Not extends ConfirmationPage

  def fromConfirmation(confirmation: Confirmation): ConfirmationPage = Confirmator(confirmation)
}

sealed trait ConfirmationAction

object ConfirmationAction {
  case class NotConfirmed(redirect: Result) extends ConfirmationAction
  case class UpdateConfirmation(f: ProcessData => ProcessData) extends ConfirmationAction

  val noop: ConfirmationAction = ConfirmationAction.UpdateConfirmation(identity)
}
