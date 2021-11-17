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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.{ Obligations, RetrievedObligations, TaxResponse }

trait ObligationValidator extends TaxSelectionNavigator {

  def validateWithDes(
    formModelOptics: FormModelOptics[DataOrigin.Browser],
    cachedObligation: Obligations,
    desObligation: Obligations
  ): FormModelOptics[DataOrigin.Browser] =
    (cachedObligation, desObligation) match {
      case (RetrievedObligations(obligation), RetrievedObligations(responseObligation)) =>
        clearTaxResponses(formModelOptics, obligation, responseObligation)
      case _ => formModelOptics
    }

  private def clearTaxResponses(
    formModelOptics: FormModelOptics[DataOrigin.Browser],
    cachedObligation: NonEmptyList[TaxResponse],
    desObligation: NonEmptyList[TaxResponse]
  ): FormModelOptics[DataOrigin.Browser] = {
    val toRemove: List[ModelComponentId] = cachedObligation.toList
      .zip(desObligation.toList)
      .map { case (cached, taxResponse) =>
        (cached.obligation, taxResponse)
      }
      .map { case (obligation, taxResponse) =>
        val taxSelectionNavigation = taxSelectionNavigator(formModelOptics, obligation, taxResponse)

        taxSelectionNavigation match {
          case GoBackToTaxPeriodSelection      => Some(taxResponse.id.recalculatedTaxPeriodKey.fcId.modelComponentId)
          case DoNotGoBackToTaxPeriodSelection => None
        }
      }
      .flatten

    formModelOptics.clearModelComponentIds(toRemove)
  }
}
