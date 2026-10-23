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

import munit.FunSuite
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

class ConfirmationServiceSpec extends FunSuite {

  test("language switch refreshes only existing confirmation expression snapshots") {
    val confirmedQuestion = FormComponentId("confirmedQuestion")
    val unconfirmedQuestion = FormComponentId("unconfirmedQuestion")

    val existingConfirmations = Some(Map(confirmedQuestion -> List("1 January 2000")))
    val currentConfirmations = Map(
      confirmedQuestion   -> List("1 Ionawr 2000"),
      unconfirmedQuestion -> List("unconfirmed value")
    )

    val result =
      ConfirmationService.updateConfirmationsForLanguageSwitch(existingConfirmations, currentConfirmations)

    assertEquals(result, Some(Map(confirmedQuestion -> List("1 Ionawr 2000"))))
  }

  test("language switch leaves absent confirmation snapshots absent") {
    val currentConfirmations = Map(FormComponentId("question") -> List("current value"))

    assertEquals(
      ConfirmationService.updateConfirmationsForLanguageSwitch(None, currentConfirmations),
      None
    )
  }
}
