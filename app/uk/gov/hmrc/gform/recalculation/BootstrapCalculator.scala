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
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.DataRetrieveId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

// This is used to determine if form component is hidden or not.
// If pageA is in ATL it sees:
// ${pageA > 10}
object BootstrapCalculator {

  def apply(
    runtime: Runtime,
    answerMap: AnswerMap,
    metadata: Metadata,
    evaluationContext: EvaluationContext,
    formModelMetadata: FormModelMetadata,
    lifter: Lifter
  )(implicit messages: Messages): Calculator = {
    def allIndexEvaluationStatuses(fcId: FormComponentId): List[EvaluationStatus] = {
      val allFcIds: Set[ModelComponentId] = runtime.toIndexedFormComponentIds(fcId)
      allFcIds.toList.map(fcId => answerMap(fcId))
    }

    def evalModelComponentId(rfcId: ModelComponentId): EvaluationStatus = {
      val baseComponentId = rfcId.baseComponentId
      if (metadata.isAtomic(baseComponentId)) {

        val componentType = metadata.componentTypeLookup(baseComponentId)

        componentType match {
          case _: Date =>
            val day = answerMap(rfcId.toAtomicFormComponentId(Date.day))
            val month = answerMap(rfcId.toAtomicFormComponentId(Date.month))
            val year = answerMap(rfcId.toAtomicFormComponentId(Date.year))

            EvaluationStatus.evalDate(day, month, year, rfcId)

          case CalendarDate =>
            val day = answerMap(rfcId.toAtomicFormComponentId(Date.day))
            val month = answerMap(rfcId.toAtomicFormComponentId(Date.month))

            EvaluationStatus.evalCalendarDate(day, month, rfcId)

          case TaxPeriodDate =>
            val month = answerMap(rfcId.toAtomicFormComponentId(Date.month))
            val year = answerMap(rfcId.toAtomicFormComponentId(Date.year))

            EvaluationStatus.evalTaxPeriodDate(month, year, rfcId)

          case _: Address =>
            val addressAtoms: List[ModelComponentId.Atomic] =
              Address.fields(rfcId.indexedComponentId).filter(_.atom != Address.uk)
            evalAddressAtoms(addressAtoms)

          case _: OverseasAddress =>
            val addressAtoms: List[ModelComponentId.Atomic] = OverseasAddress.fields(rfcId.indexedComponentId).toList
            evalAddressAtoms(addressAtoms)

          case _: PostcodeLookup =>
            val postcodeLookup = answerMap(rfcId.toAtomicFormComponentId(PostcodeLookup.postcode))
            EvaluationStatus.evalPostcodeLookup(rfcId.toFormComponentId, postcodeLookup, evaluationContext)
          case todo => throw new Exception(s"TODO implemented support for $todo")
        }
      } else if (metadata.allFileUploads(rfcId.baseComponentId)) {
        answerMap(rfcId).stripFileNamePrefix(evaluationContext, rfcId)
      } else if (metadata.allMultiFileUploads(rfcId.baseComponentId)) {
        EvaluationStatus.evalMultiFile(rfcId, evaluationContext)
      } else {
        answerMap(rfcId)
      }
    }

    def evalAddressAtoms(atoms: List[ModelComponentId.Atomic]): EvaluationStatus = {

      val statuses: List[(Atom, EvaluationStatus)] = atoms.map(atom => atom.atom -> answerMap(atom))

      val address = statuses.collect { case (atom, EvaluationStatus.StringResult(result)) => atom -> result }
      EvaluationStatus.AddressResult(address)
    }

    def lessThanOrEqualToCurrentIndexEvaluationStatuses(fcId: FormComponentId): List[EvaluationStatus] = {
      val allFcIds: Set[ModelComponentId] = runtime.toIndexedFormComponentIds(fcId)

      val maybeIndex: Option[Int] = lifter.modelComponentId.maybeIndex
      val lessThanOrEqualCurrentIndexFcIds0 = maybeIndex match {
        case None        => allFcIds
        case Some(index) => allFcIds.filter(_.maybeIndex.map(_ <= index).getOrElse(false))
      }

      lessThanOrEqualCurrentIndexFcIds0.toList.map { fcId =>
        answerMap(fcId)
      }
    }

    val dataBridge = new DataBridge {
      val name = "BootstrapCalculator"
      val valueValue = EvaluationStatus.Dirty
      val index = lifter.modelComponentId.maybeIndex
      def evalFormCtx(formComponentId: FormComponentId, behaviour: Behaviour): EvaluationStatus = {
        val rfcIds = lifter.toRuntimeValue(metadata, formComponentId)
        rfcIds match {
          case Left(rfcId)
              if behaviour == Behaviour.LessThanCurrent && metadata.isRepeatedField(
                formComponentId.baseComponentId
              ) ||
                metadata.isGroupComponent(formComponentId) =>
            EvaluationStatus.ListResult(lessThanOrEqualToCurrentIndexEvaluationStatuses(formComponentId))
          case Left(rfcId) if behaviour == Behaviour.All && metadata.isRepeatedField(formComponentId.baseComponentId) =>
            EvaluationStatus.ListResult(allIndexEvaluationStatuses(formComponentId))
          case Left(rfcId) => evalModelComponentId(rfcId)
          case Right(rfcIds) =>
            EvaluationStatus.ListResult(rfcIds.map(evalModelComponentId).toList)
        }
      }
      def maybeIndex(formComponentId: FormComponentId): Option[Int] = index
      def liftDataRetrieveId(id: DataRetrieveId): DataRetrieveId = index.fold(id)(id.withIndex)
      def insideAtl(formCtx: FormCtx): Boolean = index.isDefined
      def outsideAtl(formCtx: FormCtx): Boolean = !index.isDefined
      def allModelComponentIds(modelComponentId: ModelComponentId): List[(ModelComponentId, EvaluationStatus)] =
        answerMap.forBaseComponentId(modelComponentId.baseComponentId)
    }

    new RealCalculator(metadata, evaluationContext, formModelMetadata, dataBridge)
  }
}
