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

import cats.instances.list._
import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.{ Obligation, TaxResponse }

sealed trait TaxSelectionNavigation
case object DoNotGoBackToTaxPeriodSelection extends TaxSelectionNavigation
case object GoBackToTaxPeriodSelection extends TaxSelectionNavigation

trait TaxSelectionNavigator {

  def taxSelectionNavigator(
    data: FormDataRecalculated,
    cachedObligation: Obligation,
    taxResponse: TaxResponse): TaxSelectionNavigation = {
    val obligationsMatch = obligationsMatchTaxResponseObligations(cachedObligation, taxResponse)
    val stillAvailable = selectedPeriodStillAvailable(data, taxResponse)

    (obligationsMatch, stillAvailable) match {
      case (GoBackToTaxPeriodSelection, DoNotGoBackToTaxPeriodSelection) =>
        goBackIf(desHasMore(cachedObligation, taxResponse.obligation))
      case (GoBackToTaxPeriodSelection, GoBackToTaxPeriodSelection) =>
        GoBackToTaxPeriodSelection
      case (DoNotGoBackToTaxPeriodSelection, _) =>
        DoNotGoBackToTaxPeriodSelection
    }
  }

  private def obligationsMatchTaxResponseObligations(
    cachedObligation: Obligation,
    taxResponse: TaxResponse): TaxSelectionNavigation =
    if (cachedObligation.obligations === taxResponse.obligation.obligations) DoNotGoBackToTaxPeriodSelection
    else GoBackToTaxPeriodSelection

  private def selectedPeriodStillAvailable(
    data: FormDataRecalculated,
    taxResponse: TaxResponse): TaxSelectionNavigation = {
    val desPeriods: List[String] = taxResponse.obligation.obligations.flatMap(_.obligationDetails).map(_.periodKey)
    data.data
      .one(taxResponse.id.recalculatedTaxPeriodKey.fcId)
      .fold[TaxSelectionNavigation](GoBackToTaxPeriodSelection) { period =>
        goBackIf(!desPeriods.contains(period))
      }
  }

  private def desHasMore(dataObligation: Obligation, desObligation: Obligation): Boolean =
    desObligation.obligations.flatMap(_.obligationDetails).size > dataObligation.obligations
      .flatMap(_.obligationDetails)
      .size

  private val goBackIf: Boolean => TaxSelectionNavigation =
    predicate => if (predicate) GoBackToTaxPeriodSelection else DoNotGoBackToTaxPeriodSelection

}
