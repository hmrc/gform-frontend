/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

/* import uk.gov.hmrc.gform.Spec
 * import FormDataRecalculated._
 * import uk.gov.hmrc.gform.graph.RecData
 * import uk.gov.hmrc.gform.sharedmodel.{ IdNumberValue, RecalculatedTaxPeriodKey, VariadicFormData }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate._
 * import uk.gov.hmrc.gform.sharedmodel.graph.GraphNode
 *
 * class FormDataRecalculatedTest extends Spec {
 *
 *   it should "clear recalculated tax period" in {
 *     val taxPeriod = HmrcTaxPeriod(IdType("id"), FormCtx("ctx"), RegimeType("AAA"))
 *     val commonId = FormComponentId("2")
 *     val periodKey = RecalculatedTaxPeriodKey(commonId, taxPeriod)
 *     val unclearedData: VariadicFormData = VariadicFormData.ones(FormComponentId("1") -> "one", commonId -> "two")
 *     val idNumberValue = IdNumberValue("777")
 *
 *     val formDataRecalculated =
 *       FormDataRecalculated(Set[GraphNode](), RecData(unclearedData, Map(periodKey -> idNumberValue)))
 *
 *     val expectedRecData = RecData(
 *       VariadicFormData.ones(FormComponentId("1")        -> "one"),
 *       Map(RecalculatedTaxPeriodKey(commonId, taxPeriod) -> idNumberValue))
 *
 *     clearTaxResponses(formDataRecalculated) should be(formDataRecalculated.copy(recData = expectedRecData))
 *   }
 * } */
