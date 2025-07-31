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

import cats.syntax.functor._
import cats.{ Functor, MonadError }
import com.softwaremill.quicklens._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.{ AuthCache, AuthCacheWithForm, AuthCacheWithoutForm, CacheData }
import uk.gov.hmrc.gform.eval.{ EvaluationContext, FileIdsWithMapping }
import uk.gov.hmrc.gform.graph.{ RecData, Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelRenderPageOptics, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EnrolmentSection, FileComponentId, FileSizeLimit, FormPhase }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{ Instant, LocalDate }

case class FormModelOptics[D <: DataOrigin](
  formModelRenderPageOptics: FormModelRenderPageOptics[D],
  formModelVisibilityOptics: FormModelVisibilityOptics[D]
) {
  val pageOpticsData: VariadicFormData[SourceOrigin.Current] = formModelRenderPageOptics.recData.variadicFormData

  def clearModelComponentIds(
    modelComponentIds: List[ModelComponentId]
  ): FormModelOptics[D] =
    this
      .modify(_.formModelRenderPageOptics.recData.variadicFormData)
      .setTo(formModelRenderPageOptics.recData.cleared(modelComponentIds))
      .modify(_.formModelVisibilityOptics.recData.variadicFormData)
      .setTo(formModelVisibilityOptics.recData.cleared(modelComponentIds))
}

object FormModelOptics {

  def fromEnrolmentSection[D <: DataOrigin](enrolmentSection: EnrolmentSection, cache: AuthCacheWithoutForm)(implicit
    lang: LangADT,
    messages: Messages,
    hc: HeaderCarrier
  ) = {
    val evaluationContext =
      EvaluationContext(
        cache.formTemplate._id,
        SubmissionRef(""),
        cache.accessCode,
        cache.retrievals,
        ThirdPartyData.empty,
        cache.formTemplate.authConfig,
        hc,
        Option.empty[FormPhase],
        FileIdsWithMapping.empty,
        Map.empty[ModelComponentId, List[(FileComponentId, VariadicValue.One)]],
        Map.empty,
        Set.empty[BaseComponentId],
        Set.empty[BaseComponentId],
        Set.empty[BaseComponentId],
        Map.empty,
        lang,
        messages,
        Map.empty,
        Set.empty[BaseComponentId],
        FileSizeLimit(cache.formTemplate.fileSizeLimit.getOrElse(FileSizeLimit.defaultFileLimitSize)),
        DataRetrieveAll.empty,
        Set.empty[ModelComponentId],
        Map.empty,
        Set.empty,
        cache.lookupRegistry,
        Map.empty,
        Map.empty,
        TaskIdTaskStatusMapping.empty,
        LocalDate.now()
      )
    FormModelOptics[D](
      FormModelRenderPageOptics(FormModel.fromEnrolmentSection[DataExpanded](enrolmentSection), RecData.empty),
      FormModelVisibilityOptics(
        FormModel.fromEnrolmentSection[Visibility](enrolmentSection),
        RecData.empty,
        RecalculationResult.empty(evaluationContext)
      )
    )
  }

  def mkFormModelOptics[D <: DataOrigin, F[_]: Functor, U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData[SourceOrigin.OutOfDate],
    cache: AuthCache,
    cacheData: CacheData,
    recalculation: Recalculation[F, Throwable],
    phase: Option[FormPhase],
    componentIdToFileId: FormComponentIdToFileIdMapping,
    taskIdTaskStatusMapping: TaskIdTaskStatusMapping,
    formStartDate: Instant
  )(implicit
    messages: Messages,
    lang: LangADT,
    hc: HeaderCarrier,
    me: MonadError[F, Throwable]
  ): F[FormModelOptics[D]] = {
    val formModelBuilder =
      FormModelBuilder.fromCache(
        cache,
        cacheData,
        recalculation,
        componentIdToFileId,
        cache.lookupRegistry,
        taskIdTaskStatusMapping
      )
    val formModelVisibilityOpticsF: F[FormModelVisibilityOptics[D]] =
      formModelBuilder.visibilityModel(data, phase, formStartDate)
    formModelVisibilityOpticsF.map { formModelVisibilityOptics =>
      formModelBuilder.renderPageModel(formModelVisibilityOptics, phase)
    }
  }

  def mkFormModelOptics[D <: DataOrigin, F[_]: Functor, U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData[SourceOrigin.OutOfDate],
    cache: AuthCacheWithForm,
    recalculation: Recalculation[F, Throwable],
    phase: Option[FormPhase] = None
  )(implicit
    messages: Messages,
    lang: LangADT,
    hc: HeaderCarrier,
    me: MonadError[F, Throwable]
  ): F[FormModelOptics[D]] =
    mkFormModelOptics(
      data,
      cache,
      cache.toCacheData,
      recalculation,
      phase,
      cache.form.componentIdToFileId,
      cache.form.taskIdTaskStatus,
      cache.form.startDate
    )
}
