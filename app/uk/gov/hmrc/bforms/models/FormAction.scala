/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.models

sealed trait FormAction

object FormAction {
  def fromAction(action: List[String], page: Page): Either[String, FormAction] = {
    val onLastPage = page.curr == page.next

    (action, onLastPage) match {
      case ("Save" :: Nil, _) => Right(SaveAndExit)
      case ("Continue" :: Nil, true) => Right(SaveAndSummary)
      case ("Continue" :: Nil, false) => Right(SaveAndContinue)
      case _ => Left("Cannot determite action")
    }
  }
}

case object SaveAndContinue extends FormAction
case object SaveAndExit extends FormAction
case object SaveAndSummary extends FormAction
