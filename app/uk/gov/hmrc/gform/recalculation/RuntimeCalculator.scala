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

import play.api.i18n.Messages
import cats.syntax.eq._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.DataRetrieveId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

// If pageA is in ATL it sees:
// ${1_pageA > 10} etc.
object RuntimeCalculator {

  def apply(
    metadata: Metadata,
    answerMapWithFallback: AnswerMapWithFallback,
    evaluationContext: EvaluationContext,
    formModelMetadata: FormModelMetadata
  )(implicit messages: Messages): Calculator = {
    def evalDate(fcId: FormComponentId): EvaluationStatus = {
      val day = answerMapWithFallback.expectNumberResult(fcId, Date.day)
      val month = answerMapWithFallback.expectNumberResult(fcId, Date.month)
      val year = answerMapWithFallback.expectNumberResult(fcId, Date.year)

      EvaluationStatus.evalDate(day, month, year, fcId.modelComponentId)
    }

    def evalCalendarDate(fcId: FormComponentId): EvaluationStatus = {

      val day = answerMapWithFallback.expectNumberResult(fcId, Date.day)
      val month = answerMapWithFallback.expectNumberResult(fcId, Date.month)

      EvaluationStatus.evalCalendarDate(day, month, fcId.modelComponentId)
    }

    def evalTaxPeriodDate(fcId: FormComponentId): EvaluationStatus = {
      val month = answerMapWithFallback.expectNumberResult(fcId, Date.month)
      val year = answerMapWithFallback.expectNumberResult(fcId, Date.year)

      EvaluationStatus.evalTaxPeriodDate(month, year, fcId.modelComponentId)
    }

    def evalAddress(fcId: FormComponentId): EvaluationStatus = {
      val addressAtoms: List[ModelComponentId.Atomic] =
        Address.fields(fcId.modelComponentId.indexedComponentId).filter(_.atom =!= Address.uk)
      evalAddressAtoms(addressAtoms)
    }

    def evalOverseasAddress(fcId: FormComponentId): EvaluationStatus = {
      val addressAtoms: List[ModelComponentId.Atomic] =
        OverseasAddress.fields(fcId.modelComponentId.indexedComponentId).toList
      evalAddressAtoms(addressAtoms)
    }

    def evalPostCodeLookup(fcId: FormComponentId): EvaluationStatus = {
      val postcodeLookup =
        answerMapWithFallback.toStringResultOrOptionResult(fcId.toAtomicFormComponentId(PostcodeLookup.postcode))
      EvaluationStatus.evalPostcodeLookup(fcId, postcodeLookup, evaluationContext)
    }

    def evalAddressAtoms(atoms: List[ModelComponentId.Atomic]): EvaluationStatus = {
      val statuses: List[(Atom, EvaluationStatus)] =
        atoms.map(atom => atom.atom -> answerMapWithFallback.toStringResultOrOptionResult(atom))

      val address = statuses.collect { case (atom, EvaluationStatus.StringResult(result)) => atom -> result }
      EvaluationStatus.AddressResult(address)
    }

    def evalMultiFile(fcId: FormComponentId): EvaluationStatus =
      EvaluationStatus.evalMultiFile(fcId.modelComponentId, evaluationContext)

    def evalFile(fcId: FormComponentId): EvaluationStatus = {
      val modelComponentId = fcId.modelComponentId

      answerMapWithFallback
        .toStringResultOrOptionResult(modelComponentId)
        .stripFileNamePrefix(evaluationContext, modelComponentId)
    }

    def evalGroup(fcId: FormComponentId): EvaluationStatus = {

      val maybeN = answerMapWithFallback.maxIndexOf(fcId.baseComponentId)

      val n = maybeN.getOrElse(1) // There is always at least 1 repeat

      EvaluationStatus.ListResult(List.fill(n)(EvaluationStatus.StringResult("")))
    }

    val dataBridge = new DataBridge {
      val name = "RuntimeCalculator"
      val valueValue: EvaluationStatus = EvaluationStatus.Empty
      def maybeIndex(formComponentId: FormComponentId): Option[Int] = formComponentId.modelComponentId.maybeIndex
      def liftDataRetrieveId(id: DataRetrieveId): DataRetrieveId = id
      def insideAtl(formCtx: FormCtx): Boolean = formCtx.formComponentId.modelComponentId.maybeIndex.isDefined
      def outsideAtl(formCtx: FormCtx): Boolean = !formCtx.formComponentId.modelComponentId.maybeIndex.isDefined
      def allModelComponentIds(modelComponentId: ModelComponentId): List[(ModelComponentId, EvaluationStatus)] =
        answerMapWithFallback.allModelComponentIds(modelComponentId)
      def evalFormCtx(formComponentId: FormComponentId, behaviour: Behaviour): EvaluationStatus = {
        val baseComponentId = formComponentId.baseComponentId
        val componentType = metadata.componentTypeLookup(baseComponentId)
        val refInfo = metadata.refInfo(baseComponentId)
        val computeList: Boolean = refInfo match {
          case RefInfo.AddToListPage(_) | RefInfo.TaskListAddToListPage(_, _) | RefInfo.RepeatingPage(_) |
              RefInfo.TaskListRepeatingPage(_, _) if !formComponentId.modelComponentId.maybeIndex.isDefined =>
            true
          case _ =>
            metadata.isGroupComponent(
              formComponentId
            ) || behaviour == Behaviour.LessThanCurrent || behaviour == Behaviour.All
        }

        val res = if (computeList) {

          val maybeMaxIndex = behaviour match {
            case Behaviour.LessThanCurrent if formComponentId.modelComponentId.maybeIndex.isDefined =>
              formComponentId.modelComponentId.maybeIndex
            case Behaviour.All => answerMapWithFallback.maxIndexOf(baseComponentId)
            case _ =>
              metadata.addToListIdFor(baseComponentId) match {
                case Some(addToListId) =>
                  List(addToListId.formComponentId.baseComponentId, baseComponentId)
                    .map(answerMapWithFallback.maxIndexOf)
                    .flatten
                    .maxOption
                case None =>
                  answerMapWithFallback.maxIndexOf(
                    baseComponentId
                  ) // TODO JoVl should this be tailored to repeating section too?
              }
          }

          val baseComponentIds =
            maybeMaxIndex.fold(List(if (refInfo.isRepeatedField()) formComponentId.withIndex(1) else formComponentId))(
              maxIndex => (1 to maxIndex).toList.map(index => formComponentId.noIndex.withIndex(index))
            )

          val results: List[EvaluationStatus] =
            componentType match {
              case _: Text | _: TextArea | _: Choice | _: RevealingChoice =>
                baseComponentIds.map(fcId => answerMapWithFallback.toStringResultOrOptionResult(fcId.modelComponentId))
              case _: Date            => baseComponentIds.map(fcId => evalDate(fcId))
              case CalendarDate       => baseComponentIds.map(fcId => evalCalendarDate(fcId))
              case TaxPeriodDate      => baseComponentIds.map(fcId => evalTaxPeriodDate(fcId))
              case _: Address         => baseComponentIds.map(fcId => evalAddress(fcId))
              case _: OverseasAddress => baseComponentIds.map(fcId => evalOverseasAddress(fcId))
              case _: PostcodeLookup  => baseComponentIds.map(fcId => evalPostCodeLookup(fcId))
              case _: FileUpload      => baseComponentIds.map(fcId => evalFile(fcId))
              case _: MultiFileUpload => baseComponentIds.map(fcId => evalMultiFile(fcId))
              case _: Group           => baseComponentIds.map(fcId => evalGroup(fcId))
              case _: HmrcTaxPeriod   => throw new Exception(s"HmrcTaxPeriod component is not supported in AddToList")
              case todo               => throw new Exception(s"TODO implemented support for LIST $todo")
            }
          EvaluationStatus.ListResult(results)
        } else {
          componentType match {
            case _: Text | _: TextArea | _: Choice | _: RevealingChoice | _: HmrcTaxPeriod =>
              answerMapWithFallback.toStringResultOrOptionResult(formComponentId.modelComponentId)
            case _: Date            => evalDate(formComponentId)
            case CalendarDate       => evalCalendarDate(formComponentId)
            case TaxPeriodDate      => evalTaxPeriodDate(formComponentId)
            case _: Address         => evalAddress(formComponentId)
            case _: OverseasAddress => evalOverseasAddress(formComponentId)
            case _: PostcodeLookup  => evalPostCodeLookup(formComponentId)
            case _: FileUpload      => evalFile(formComponentId)
            case _: MultiFileUpload => evalMultiFile(formComponentId)
            case _: Group           => evalGroup(formComponentId)
            case todo               => throw new Exception(s"TODO implemented support for SINGLE $todo")
          }
        }
        // println("HeRe 181181181 " + fcId)
        // println("      computeList: " + computeList)
        // println("              res: " + res)
        res
      }
    }

    new RealCalculator(metadata, evaluationContext, formModelMetadata, dataBridge)
  }
}
