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

package uk.gov.hmrc.gform.controllers

import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AddToListId

sealed trait Direction

case object SaveAndContinue extends Direction
case object Back extends Direction
case object SaveAndExit extends Direction
case object Exit extends Direction
case object SummaryContinue extends Direction
case object Continue extends Direction
case object DeclarationContinue extends Direction
case class AddGroup(modelComponentId: ModelComponentId) extends Direction
case class RemoveGroup(modelComponentId: ModelComponentId) extends Direction
case class EditAddToList(idx: Int, addToListId: AddToListId) extends Direction
