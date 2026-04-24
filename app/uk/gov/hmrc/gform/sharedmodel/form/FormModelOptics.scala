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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.{ AuthCache, AuthCacheWithForm, CacheData }
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.{ FormModelRenderPageOptics, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormPhase

final class FormModelOptics(
  val formModelRenderPageOptics: FormModelRenderPageOptics,
  val formModelVisibilityOptics: FormModelVisibilityOptics
) {
  val variadicFormData: VariadicFormData =
    formModelVisibilityOptics.freeCalculator.variadicFormData

  def clearModelComponentIds(
    modelComponentIds: List[ModelComponentId]
  ): FormModelOptics = new FormModelOptics(
    formModelRenderPageOptics,
    formModelVisibilityOptics.cleared(modelComponentIds)
  )

  val dataLookup: Map[BaseComponentId, List[VariadicValue]] =
    formModelVisibilityOptics.data.all
      .groupBy { case (modelComponentId, _) =>
        modelComponentId.baseComponentId
      }
      .map { case (k, v) => k -> v.map(_._2) }

}

object FormModelOptics {

  def mkFormModelOptics[U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData,
    cache: AuthCache,
    cacheData: CacheData,
    phase: Option[FormPhase],
    form: Form
  )(implicit
    lang: LangADT,
    messages: Messages
  ): FormModelOptics = {

    val formModelBuilder = FormModelBuilder.fromCache(cache, cacheData, form.componentIdToFileId, form.taskIdTaskStatus)
    formModelBuilder.visibilityModel(data, phase, form)

  }

  def mkFormModelOptics[U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData,
    cache: AuthCacheWithForm,
    phase: Option[FormPhase] = None
  )(implicit
    lang: LangADT,
    messages: Messages
  ): FormModelOptics =
    mkFormModelOptics[U](data, cache, cache.toCacheData, phase, cache.form)
}
