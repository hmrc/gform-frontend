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

import cats.{ Functor, MonadError }
import cats.syntax.eq._
import cats.syntax.functor._
import com.softwaremill.quicklens._
import play.api.libs.json._
import uk.gov.hmrc.gform.controllers.{ AuthCache, AuthCacheWithForm, CacheData }
import uk.gov.hmrc.gform.eval.EvaluationResults
import uk.gov.hmrc.gform.graph.{ GraphData, Recalculation }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel, FormModelBuilder, SectionSelector, SectionSelectorType, Visibility }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelRenderPageOptics, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.http.HeaderCarrier

case class FormModelOptics[D <: DataOrigin](
  formModelRenderPageOptics: FormModelRenderPageOptics[D],
  formModelVisibilityOptics: FormModelVisibilityOptics[D]
) {
  val pageOpticsData: VariadicFormData[SourceOrigin.Current] = formModelRenderPageOptics.recData.variadicFormData

  def clearTaxResponses(
    modelComponentIds: List[ModelComponentId]
  ): FormModelOptics[D] =
    this
      .modify(_.formModelRenderPageOptics.recData.variadicFormData)
      .setTo(formModelRenderPageOptics.recData.cleared(modelComponentIds))
      .modify(_.formModelVisibilityOptics.recData.variadicFormData)
      .setTo(formModelVisibilityOptics.recData.cleared(modelComponentIds))
}

object FormModelOptics {

  def empty[D <: DataOrigin] = FormModelOptics[D](
    new FormModelRenderPageOptics(FormModel.empty[DataExpanded], RecData.empty),
    new FormModelVisibilityOptics(
      FormModel.empty[Visibility],
      RecData.empty,
      EvaluationResults.empty,
      GraphData.empty,
      BooleanExprCache.empty)
  )

  def mkFormModelOptics[D <: DataOrigin, F[_]: Functor, U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData[SourceOrigin.OutOfDate],
    cache: AuthCache,
    cacheData: CacheData,
    recalculation: Recalculation[F, Throwable]
  )(
    implicit
    hc: HeaderCarrier,
    me: MonadError[F, Throwable]
  ): F[FormModelOptics[D]] = {
    val formModelBuilder = FormModelBuilder.fromCache(cache, cacheData, recalculation)
    val formModelVisibilityOpticsF: F[FormModelVisibilityOptics[D]] = formModelBuilder.visibilityModel(data)
    formModelVisibilityOpticsF.map { formModelVisibilityOptics =>
      formModelBuilder.renderPageModel(formModelVisibilityOptics)
    }
  }

  def mkFormModelOptics[D <: DataOrigin, F[_]: Functor, U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData[SourceOrigin.OutOfDate],
    cache: AuthCacheWithForm,
    recalculation: Recalculation[F, Throwable]
  )(
    implicit
    hc: HeaderCarrier,
    me: MonadError[F, Throwable]
  ): F[FormModelOptics[D]] = mkFormModelOptics(data, cache, cache.toCacheData, recalculation)
}
