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

package uk.gov.hmrc.gform.graph

/* import cats.syntax.functor._
 * import cats.syntax.applicative._
 * import cats.syntax.traverse._
 * import cats.instances.list._ */

import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DestinationWithCustomerId, Destinations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object CustomerIdRecalculation {

  def evaluateCustomerId[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): CustomerId =
    customerIdExpressions(cache.formTemplate.destinations)
      .map { expr =>
        CustomerId(formModelVisibilityOptics.eval(expr).take(32))
      }
      .filter(!_.isEmpty)
      .headOption
      .getOrElse(CustomerId.empty)

  private def customerIdExpressions(destinations: Destinations): List[Expr] = destinations match {
    case ds: Destinations.DestinationList =>
      ds.destinations.collect { case d: DestinationWithCustomerId => d.customerId }
    case _ =>
      Nil
  }
}
