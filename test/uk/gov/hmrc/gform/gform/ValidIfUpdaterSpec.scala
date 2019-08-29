/*
 * Copyright 2019 HM Revenue & Customs
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

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._

class ValidIfUpdaterSpec extends Spec {
  "ValidIfUpdater" should "update reference to group's field in ValidIf expression" in {
    val validIf = ValidIf(Equals(FormCtx("choice"), FormCtx("notInGroup")))

    val group = mkGroup(2, List(mkFormComponent("choice", Value)))

    val res = ValidIfUpdater(validIf, 11, group).updated

    val expected = ValidIf(Equals(FormCtx("11_choice"), FormCtx("notInGroup")))

    res shouldBe expected
  }

  it should "update reference to section's field in ValidIf expression" in {
    val validIf = ValidIf(Equals(FormCtx("choice"), FormCtx("notInSection")))

    val section = mkSection(List(mkFormComponent("choice", Value)))

    val res = ValidIfUpdater(validIf, 11, section).updated

    val expected = ValidIf(Equals(FormCtx("11_choice"), FormCtx("notInSection")))

    res shouldBe expected
  }
}
