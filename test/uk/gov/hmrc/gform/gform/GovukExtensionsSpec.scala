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

package uk.gov.hmrc.gform.gform

import org.scalatest.{ FlatSpec, Matchers }
import play.twirl.api.Html

class GovukExtensionsSpec extends FlatSpec with Matchers {
  "insertUnit" should "insert unit to the html gnerated by govukInput" in {
    val rawHtml =
      """|<div class="govuk-form-group">
         |  <label class="govuk-label" for="totWeight1a">
         |   Total Weight of Cigars
         |  </label>
         |  <span id="totWeight1a-hint" class="govuk-hint">
         |    Enter weight of cigars in KG eg 2.75KG
         |  </span>
         |  <input class="govuk-input govuk-input--width-4" id="totWeight1a" name="totWeight1a" type="text" aria-describedby="totWeight1a-hint">
         |</div>
         |""".stripMargin

    val expected =
      """|<div class="govuk-form-group">
         |  <label class="govuk-label" for="totWeight1a">
         |   Total Weight of Cigars
         |  </label>
         |  <span id="totWeight1a-hint" class="govuk-hint">
         |    Enter weight of cigars in KG eg 2.75KG
         |  </span>
         |  <input class="govuk-input govuk-input--width-4" id="totWeight1a" name="totWeight1a" type="text" aria-describedby="totWeight1a-hint">
         |<span class="gform-unit">Kg</span></div>
         |""".stripMargin

    val doc = GovukExtensions.insertUnit(Html(rawHtml), "Kg")
    doc.outputSettings().prettyPrint(false);
    doc.body().html() shouldBe expected
  }
}
