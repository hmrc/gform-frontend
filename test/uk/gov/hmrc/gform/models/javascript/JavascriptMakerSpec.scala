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

package uk.gov.hmrc.gform.models.javascript

import cats.data.NonEmptyList
import cats.implicits._
import uk.gov.hmrc.gform.GraphSpec
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.Role
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.graph.{ GraphException, Recalculation }
import uk.gov.hmrc.gform.models.{ ProcessDataService, TaxPeriodStateChecker }
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier

class JavascriptMakerSpec extends Spec with GraphSpec {

  type EitherEffect[A] = Either[Exception, A]

  val recalculation: Recalculation[EitherEffect, Exception] =
    new Recalculation[EitherEffect, Exception](booleanExprEval, (s: GraphException) => new Exception(s.reportProblem))
  new Recalculation[EitherEffect, GraphException](booleanExprEval, (s: GraphException) => s)

  val taxPeriodStateChecker: TaxPeriodStateChecker[EitherEffect, Exception] =
    new TaxPeriodStateChecker[EitherEffect, Exception] {
      def error = new Exception("Obligation retrieval failed")
    }

  val processDataService = new ProcessDataService(recalculation, taxPeriodStateChecker)

  private def javascript(formTemplate: FormTemplate, sectionNumber: SectionNumber): String = {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val data: VariadicFormData = VariadicFormData.empty
    val cache: AuthCacheWithForm =
      AuthCacheWithForm(ExampleData.materialisedRetrievals, ExampleData.form, formTemplate, Role.Customer)
    val result: EitherEffect[(FormDataRecalculated, List[Section])] =
      processDataService.recalculateDataAndSections(data, cache)

    val dynamicSections
      : List[Section] = result.right.get._2 // What a shame to do this unsafe '.get', but it is ok, since this code is not under the test.

    JavascriptMaker.generateJs(sectionNumber, dynamicSections, cache.formTemplate)
  }

  "Generated javascript" should "handle revealing choice" in {

    val formTemplate: FormTemplate = mkFormTemplate(
      mkSection(
        mkFormComponent("standaloneAmount", Value),
        mkFormComponent(
          "lossesChoice",
          RevealingChoice(
            NonEmptyList.of(
              RevealingChoiceElement(
                toSmartString("Yes"),
                List(
                  mkFormComponent("lossAmount", Value)
                ),
                false),
              RevealingChoiceElement(
                toSmartString("Option 2"),
                List(
                  mkFormComponent("amount12", Value),
                  mkFormComponent("amount22", Value)
                ),
                false
              )
            ),
            true
          )
        ),
        mkFormComponent(
          "totalAmount",
          Add(Add(Add(FormCtx("lossAmount"), FormCtx("amount12")), FormCtx("amount22")), FormCtx("standaloneAmount"))
        ).copy(presentationHint = Some(List(TotalValue)))
      )
    )

    val snippets = List(
      """var result = BigNumber(add(add(add(getValue("lossAmount", 0, isHiddenlossAmount), getValue("amount12", 0, isHiddenamount12)), getValue("amount22", 0, isHiddenamount22)), getValue("standaloneAmount", 0, isHiddenstandaloneAmount))).decimalPlaces(numberOfDecimalPlaces, roundingMode);""",
      """|document.getElementsByName("lossesChoice").forEach(function(element, index) {
         |  element.addEventListener("change",computetotalAmount);
         |});""",
      """|var isHiddenamount22 = function () {
         |  return document.getElementById("amount22").parentNode.parentNode.parentNode.classList.contains("govuk-checkboxes__conditional--hidden");
         |};""",
      """|var isHiddenamount12 = function () {
         |  return document.getElementById("amount12").parentNode.parentNode.parentNode.classList.contains("govuk-checkboxes__conditional--hidden");
         |};""",
      """|var isHiddenlossAmount = function () {
         |  return document.getElementById("lossAmount").parentNode.parentNode.parentNode.classList.contains("govuk-checkboxes__conditional--hidden");
         |};""",
      """|var isHiddentotalAmount = function () {
         |  return false;
         |};""",
      """|var isHiddenstandaloneAmount = function () {
         |  return false;
         |};"""
    ).map(_.stripMargin)

    val js = javascript(formTemplate, SectionNumber(0))

    snippets.foreach(snippet => js should include(snippet))

  }

  it should "render isHiddenX function for element on previous section" in {
    val formTemplate: FormTemplate = mkFormTemplate(
      mkSection(
        mkFormComponent("addedValue", Value)
      ),
      mkSection(
        mkFormComponent("addedValue2", Value),
        mkFormComponent(
          "totalAmount",
          Add(FormCtx("addedValue"), FormCtx("addedValue2"))
        ).copy(presentationHint = Some(List(TotalValue)))
      )
    )

    val snippets = List(
      """|var isHiddenaddedValue = function () {
         |  return false;
         |};"""
    ).map(_.stripMargin)

    val js = javascript(formTemplate, SectionNumber(1))

    snippets.foreach(snippet => js should include(snippet))
  }
}
