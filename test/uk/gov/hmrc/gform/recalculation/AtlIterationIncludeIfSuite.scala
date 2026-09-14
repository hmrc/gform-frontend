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

package uk.gov.hmrc.gform.recalculation

import cats.data.NonEmptyList
import munit.FunSuite
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.eval.{ ExprType, StaticTypeData, StaticTypeInfo }
import uk.gov.hmrc.gform.models.DataRetrieveAll
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieveId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

// Reproduces VAT652 (notification-errors-vat-returns): a sterling expression built from ATL fields
// drives two mutually exclusive component includeIfs on a page inside the same ATL.
class AtlIterationIncludeIfSuite extends FunSuite {

  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  private val box1 = FormComponentId("box1")
  private val box1corrected = FormComponentId("box1corrected")
  private val box4 = FormComponentId("box4")
  private val box4corrected = FormComponentId("box4corrected")

  private val atlFields = List(box1, box1corrected, box4, box4corrected)

  // ${(box1corrected - box1) + (box4 - box4corrected)}
  private val totalPayable: Expr =
    Add(
      Subtraction(FormCtx(box1corrected), FormCtx(box1)),
      Subtraction(FormCtx(box4), FormCtx(box4corrected))
    )

  // Iteration 1 totals +100, iteration 2 totals -50.
  private val answersByIteration: Map[BaseComponentId, List[BigDecimal]] = Map(
    box1corrected.baseComponentId -> List(100, 0),
    box1.baseComponentId          -> List(0, 0),
    box4.baseComponentId          -> List(0, 0),
    box4corrected.baseComponentId -> List(0, 50)
  )

  private val metadata: Metadata = new Metadata(
    lookup = atlFields.map(_.baseComponentId -> RefInfo.AddToListPage(TemplateSectionIndex(0))).toMap,
    groups = Set.empty[BaseComponentId],
    groupComponents = Set.empty[FormComponentId],
    atomicFields = Set.empty[BaseComponentId],
    atomicsLookup = Map.empty[FormComponentId, IndexedComponentId => NonEmptyList[ModelComponentId.Atomic]],
    componentTypeLookup = Map.empty[BaseComponentId, ComponentType],
    dataRetrieveAll = DataRetrieveAll.empty,
    staticTypeInfo = StaticTypeInfo(
      atlFields.map(_.baseComponentId -> StaticTypeData(ExprType.Number, None)).toMap
    ),
    lookupRegister = Map.empty[BaseComponentId, Register],
    addToListDataRetrieveIds = Set.empty[DataRetrieveId],
    addToListComponentIds = atlFields.map(_.baseComponentId).toSet,
    addToListComponentIds2 = Map.empty[AddToListId, Set[BaseComponentId]],
    hideChoicesSelected = Set.empty[BaseComponentId],
    choiceLookup = Map.empty[BaseComponentId, (FormComponent, Choice)],
    addToListIds = Set.empty[BaseComponentId],
    allFileUploads = Set.empty[BaseComponentId],
    allMultiFileUploads = Set.empty[BaseComponentId]
  )

  // Mirrors BootstrapCalculator positioned on a component of ATL iteration `currentIteration`.
  private def calculatorForIteration(currentIteration: Int): Calculator = {
    val dataBridge = new DataBridge {
      val name = "AtlIterationIncludeIfSuite"
      val valueValue: EvaluationStatus = EvaluationStatus.Empty

      def evalFormCtx(formComponentId: FormComponentId, behaviour: Behaviour): EvaluationStatus = {
        val perIteration = answersByIteration(formComponentId.baseComponentId)
        behaviour match {
          case Behaviour.Default =>
            EvaluationStatus.NumberResult(perIteration(currentIteration - 1))
          case _ =>
            EvaluationStatus.ListResult(
              perIteration.take(currentIteration).map(EvaluationStatus.NumberResult(_))
            )
        }
      }

      def maybeIndex(formComponentId: FormComponentId): Option[Int] = Some(currentIteration)
      def liftDataRetrieveId(id: DataRetrieveId): DataRetrieveId = id
      def insideAtl(formCtx: FormCtx): Boolean = true
      def outsideAtl(formCtx: FormCtx): Boolean = false
      def allModelComponentIds(modelComponentId: ModelComponentId): List[(ModelComponentId, EvaluationStatus)] = Nil
    }

    new RealCalculator(
      metadata,
      EvaluationContext.empty,
      FormModelMetadata.notAvailable,
      dataBridge,
      CacheBuster.static
    )
  }

  private val owesHmrc = GreaterThanOrEquals(totalPayable, Constant("0")) // "Amount you owe HMRC"
  private val hmrcOwes = LessThan(totalPayable, Constant("0")) // "Amount HMRC owes you"

  test("iteration totalling -50 shows only the 'HMRC owes you' field") {
    val calculator = calculatorForIteration(2)

    assertEquals(calculator.evalBooleanExpr(hmrcOwes), true)
    assertEquals(calculator.evalBooleanExpr(owesHmrc), false)
  }

  test("iteration totalling +100 shows only the 'you owe HMRC' field") {
    val calculator = calculatorForIteration(1)

    assertEquals(calculator.evalBooleanExpr(owesHmrc), true)
    assertEquals(calculator.evalBooleanExpr(hmrcOwes), false)
  }

  test("mutually exclusive includeIfs are never both visible in one iteration") {
    List(1, 2).foreach { iteration =>
      val calculator = calculatorForIteration(iteration)

      assertNotEquals(
        calculator.evalBooleanExpr(owesHmrc),
        calculator.evalBooleanExpr(hmrcOwes),
        s"both includeIfs evaluated the same way in iteration $iteration"
      )
    }
  }
}
