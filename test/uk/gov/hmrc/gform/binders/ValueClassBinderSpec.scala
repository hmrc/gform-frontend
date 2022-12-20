/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.binders

import org.scalatestplus.play.PlaySpec
import uk.gov.hmrc.gform.models.FastForward
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionOrSummary
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber

class ValueClassBinderSpec extends PlaySpec {

  "FastForward parameter binding and unbinding" should {

    "build a URL parameter for FastForward list" in {
      ValueClassBinder.fastForwardListBinder
        .unbind(
          "ff",
          List(
            FastForward.CYA(SectionOrSummary.FormSummary),
            FastForward.CYA(SectionOrSummary.TaskSummary),
            FastForward.CYA(SectionOrSummary.Section(SectionNumber.Classic(42))),
            FastForward.Yes
          )
        ) mustBe "ff=cyaf&ff=cyat&ff=cya42&ff=t"
    }

    "parse a URL parameter for FastForward list" in {
      ValueClassBinder.fastForwardListBinder
        .bind("ff", Map("ff" -> List("cyaf", "cyat", "cya42", "t"))) mustBe Some(
        Right(
          List(
            FastForward.CYA(SectionOrSummary.FormSummary),
            FastForward.CYA(SectionOrSummary.TaskSummary),
            FastForward.CYA(SectionOrSummary.Section(SectionNumber.Classic(42))),
            FastForward.Yes
          )
        )
      )
    }

  }

}
