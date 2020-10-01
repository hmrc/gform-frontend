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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import cats.instances.int._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{ Monad, MonadError }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.BooleanExprCache
import uk.gov.hmrc.gform.sharedmodel.form.{ FormModelOptics, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.IsHmrcTaxPeriod
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.models.gform.ObligationsAction

import scala.util.Try

case class ProcessData(
  formModelOptics: FormModelOptics[DataOrigin.Browser],
  visitsIndex: VisitIndex,
  obligations: Obligations,
  booleanExprCache: BooleanExprCache
) {
  val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel
}

class ProcessDataService[F[_]: Monad](
  recalculation: Recalculation[F, Throwable],
  taxPeriodStateChecker: TaxPeriodStateChecker[F, Throwable]) {

  def hmrcTaxPeriodWithId(
    hmrcTaxPeriodWithEvaluatedIds: List[Option[HmrcTaxPeriodWithEvaluatedId]]
  ): Option[NonEmptyList[HmrcTaxPeriodWithEvaluatedId]] =
    hmrcTaxPeriodWithEvaluatedIds.flatten match {
      case x :: xs => Some(NonEmptyList(x, xs))
      case Nil     => None
    }

  def getProcessData[U <: SectionSelectorType: SectionSelector](
    dataRaw: VariadicFormData[SourceOrigin.OutOfDate],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    getAllTaxPeriods: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] => F[NonEmptyList[ServiceCallResponse[TaxResponse]]],
    obligationsAction: ObligationsAction
  )(
    implicit hc: HeaderCarrier,
    me: MonadError[F, Throwable],
    l: LangADT
  ): F[ProcessData] = {

    val hmrcTaxPeriodWithEvaluatedIds: List[Option[HmrcTaxPeriodWithEvaluatedId]] =
      formModelOptics.formModelVisibilityOptics.allFormComponents.collect {
        case fc @ IsHmrcTaxPeriod(hmrcTaxPeriod) =>
          val idNumber = formModelOptics.formModelVisibilityOptics.eval(hmrcTaxPeriod.idNumber)
          if (idNumber.isEmpty) {
            None
          } else
            Some(
              HmrcTaxPeriodWithEvaluatedId(
                RecalculatedTaxPeriodKey(fc.id, hmrcTaxPeriod),
                IdNumberValue(formModelOptics.formModelVisibilityOptics.eval(hmrcTaxPeriod.idNumber))))
      }

    val cachedObligations: Obligations = cache.form.thirdPartyData.obligations

    val mongoData = formModelOptics
    for {

      browserFormModelOptics <- FormModelOptics
                                 .mkFormModelOptics[DataOrigin.Browser, F, U](dataRaw, cache, recalculation)

      obligations <- taxPeriodStateChecker.callDesIfNeeded(
                      getAllTaxPeriods,
                      hmrcTaxPeriodWithId(hmrcTaxPeriodWithEvaluatedIds),
                      cachedObligations,
                      obligationsAction
                    )

    } yield {

      val dataUpd: FormModelOptics[DataOrigin.Browser] = new ObligationValidator {}
        .validateWithDes(browserFormModelOptics, cachedObligations, obligations)

      val newVisitIndex =
        VisitIndex.updateSectionVisits(
          browserFormModelOptics.formModelRenderPageOptics.formModel,
          mongoData.formModelRenderPageOptics.formModel,
          cache.form.visitsIndex)

      ProcessData(
        dataUpd,
        VisitIndex(newVisitIndex),
        obligations,
        browserFormModelOptics.formModelVisibilityOptics.booleanExprCache)
    }
  }
}
