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

package uk.gov.hmrc.gform.models.form

import uk.gov.hmrc.gform.models.Page

sealed trait FormAction

object FormAction {
  def determineAction(action: List[String], nextPage: Option[Page]): Either[String, FormAction] = {

    (action, nextPage) match {
      case ("Save" :: Nil, _) => Right(SaveAndExit)
      case ("Continue" :: Nil, None) => Right(SaveAndSummary)
      case ("Continue" :: Nil, Some(nextToRender)) => Right(SaveAndContinue(nextToRender))
      case _ => Left("Cannot determine action")
    }
  }
}

case class SaveAndContinue(nextPage: Page) extends FormAction
case object SaveAndExit extends FormAction
case object SaveAndSummary extends FormAction
