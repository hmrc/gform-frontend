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

package uk.gov.hmrc.gform.models

import uk.gov.hmrc.gform.Spec

class ObligationValidatorTest extends Spec with ObligationValidatorTestFixture {

  // format: off
  val taxPeriodValidationTable = Table(
    ("description", "data", "cachedObligations", "taxResponse", "output"),
    ("obligations match and selected still available",                      cachedData,                                      obligations, taxResponse,                                                DoNotGoBackToTaxPeriodSelection),
    ("more obligations are now available in DES",                           cachedData,                                      obligations, taxResponse.withObligations(moreObligationsAvailable),      GoBackToTaxPeriodSelection),
    ("less obligations are available but selected still available",         cachedData,                                      obligations, taxResponse.withObligations(lessObligationsAvailable),      DoNotGoBackToTaxPeriodSelection),
    ("less obligations are available in DES and selected is not available", cachedData.updated(formComponentId, Seq("XXX")), obligations, taxResponse.withObligations(lessObligationsAvailable),      GoBackToTaxPeriodSelection),
    ("obligations size match but are different in DES",                     cachedData,                                      obligations, taxResponse.withObligations(differentObligationsAvailable), GoBackToTaxPeriodSelection)
  )
  // format: on

  forAll(taxPeriodValidationTable) { (description, data, storedObligations, taxResponse, output) =>
    it should s"check if still valid when $description" in new ObligationValidator {

      validate(data, storedObligations, taxResponse) shouldBe output
    }
  }
}
