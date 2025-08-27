/*
 * Copyright 2023 HM Revenue & Customs
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
import cats.syntax.functor._
import cats.{ Monad, MonadError }
import com.softwaremill.quicklens._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.models.gform.ObligationsAction
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ FormModelOptics, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Confirmation, FormComponentId, IsHmrcTaxPeriod }
import uk.gov.hmrc.http.HeaderCarrier

case class ProcessData(
  formModelOptics: FormModelOptics[DataOrigin.Browser],
  visitsIndex: VisitIndex,
  obligations: Obligations,
  booleanExprCache: BooleanExprCache,
  confirmations: Option[Map[FormComponentId, List[String]]]
) {
  val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel

  def removeConfirmation(confirmations: List[Confirmation]): ProcessData =
    this
      .modify(_.formModelOptics)
      .using(_.clearModelComponentIds(confirmations.map(_.question.id.modelComponentId)))
}

class ProcessDataService[F[_]: Monad](
  taxPeriodStateChecker: TaxPeriodStateChecker[F, Throwable]
) {

  def hmrcTaxPeriodWithId(
    hmrcTaxPeriodWithEvaluatedIds: List[Option[HmrcTaxPeriodWithEvaluatedId]]
  ): Option[NonEmptyList[HmrcTaxPeriodWithEvaluatedId]] =
    hmrcTaxPeriodWithEvaluatedIds.flatten match {
      case x :: xs => Some(NonEmptyList(x, xs))
      case Nil     => None
    }

  private def hmrcTaxPeriodWithEvaluatedIds(
    browserFormModelOptics: FormModelOptics[DataOrigin.Browser]
  )(implicit messages: Messages): List[Option[HmrcTaxPeriodWithEvaluatedId]] = {
    val fmvo = browserFormModelOptics.formModelVisibilityOptics
    fmvo.allFormComponents.collect { case fc @ IsHmrcTaxPeriod(hmrcTaxPeriod) =>
      val idNumber = fmvo.evalAndApplyTypeInfoFirst(hmrcTaxPeriod.idNumber).stringRepresentation
      if (idNumber.isEmpty) {
        None
      } else {
        Some(HmrcTaxPeriodWithEvaluatedId(RecalculatedTaxPeriodKey(fc.id, hmrcTaxPeriod), IdNumberValue(idNumber)))
      }
    }
  }

  def getProcessData[U <: SectionSelectorType: SectionSelector](
    dataRaw: VariadicFormData[SourceOrigin.OutOfDate],
    cache: AuthCacheWithForm,
    getAllTaxPeriods: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] => F[NonEmptyList[ServiceCallResponse[TaxResponse]]],
    obligationsAction: ObligationsAction
  )(implicit
    lang: LangADT,
    messages: Messages,
    hc: HeaderCarrier,
    me: MonadError[F, Throwable]
  ): F[ProcessData] = {

    val cachedObligations: Obligations = cache.form.thirdPartyData.obligations

    val browserFormModelOptics = FormModelOptics
      .mkFormModelOptics[DataOrigin.Browser, U](dataRaw, cache)
    taxPeriodStateChecker
      .callDesIfNeeded(
        getAllTaxPeriods,
        hmrcTaxPeriodWithId(hmrcTaxPeriodWithEvaluatedIds(browserFormModelOptics)),
        cachedObligations,
        obligationsAction
      )
      .map { obligations =>
        val dataUpd: FormModelOptics[DataOrigin.Browser] = new ObligationValidator {}
          .validateWithDes(browserFormModelOptics, cachedObligations, obligations)

        ProcessData(
          dataUpd,
          cache.form.visitsIndex,
          obligations,
          browserFormModelOptics.formModelVisibilityOptics.recalculationResult.graphDataCache.booleanExprCache,
          cache.form.thirdPartyData.confirmations.map(_.map { case (k, v) => k -> v })
        )
      }
  }
}
