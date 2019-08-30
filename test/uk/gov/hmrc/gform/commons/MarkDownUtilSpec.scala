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

package uk.gov.hmrc.gform.commons

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import play.twirl.api.Html
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.sharedmodel.LangADT

class MarkDownUtilSpec extends Spec with GeneratorDrivenPropertyChecks {
  implicit val l = LangADT.En
  "addTargetToLinks" should "add target link" in {

    val dataAndExpectations = Table(
      ("input", "output"),
      ("", ""),
      (
        "link [making a claim](https://www.gov.uk/government)",
        """<p>link <a href="https://www.gov.uk/government" target="_blank">making a claim</a></p>""")
    )

    forAll(dataAndExpectations) { (input, expected) =>
      val res = MarkDownUtil.markDownParser(toLocalisedString(input))

      res should be(Html(expected))

    }
  }
}
