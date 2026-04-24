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

import java.time.LocalDate
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals }
import uk.gov.hmrc.gform.controllers.AuthCacheWithoutForm
import uk.gov.hmrc.gform.lookup.{ LookupRegistry, LookupType }
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Anonymous, CustomSubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfig, FileComponentId, FormPhase, Register }
import uk.gov.hmrc.gform.sharedmodel.form.{ TaskIdTaskStatusMapping, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileSizeLimit, FormTemplateId }
import uk.gov.hmrc.http.SessionId

final case class EvaluationContext(
  formTemplateId: FormTemplateId,
  envelopeId: EnvelopeId,
  customSubmissionRef: Option[CustomSubmissionRef],
  fileSizeLimit: FileSizeLimit,
  maybeAccessCode: Option[AccessCode],
  authConfig: AuthConfig,
  retrievals: MaterialisedRetrievals,
  thirdPartyData: ThirdPartyData,
  taskIdTaskStatus: TaskIdTaskStatusMapping,
  formStartDate: LocalDate,
  lookupRegistry: LookupRegistry,
  lookupRegister: Map[BaseComponentId, Register],
  componentIdToFileId: FormComponentIdToFileIdMapping,
  multiFilesData: Map[ModelComponentId, List[(FileComponentId, VariadicValue.One)]],
  formPhase: Option[FormPhase],
  lang: LangADT
)

object EvaluationContext {

  def from(cache: AuthCacheWithoutForm): EvaluationContext =
    EvaluationContext(
      cache.formTemplate._id,
      EnvelopeId(""),
      Option.empty[CustomSubmissionRef],
      FileSizeLimit(cache.formTemplate.fileSizeLimit.getOrElse(FileSizeLimit.defaultFileLimitSize)),
      cache.accessCode,
      cache.formTemplate.authConfig,
      cache.retrievals,
      ThirdPartyData.empty,
      TaskIdTaskStatusMapping.empty,
      LocalDate.now(),
      cache.lookupRegistry,
      Map.empty[BaseComponentId, Register],
      FormComponentIdToFileIdMapping.empty,
      Map.empty[ModelComponentId, List[(FileComponentId, VariadicValue.One)]],
      Option.empty[FormPhase],
      LangADT.En
    )

  val empty: EvaluationContext =
    EvaluationContext(
      FormTemplateId(""),
      EnvelopeId(""),
      Option.empty[CustomSubmissionRef],
      FileSizeLimit(FileSizeLimit.defaultFileLimitSize),
      Option.empty[AccessCode],
      Anonymous,
      AnonymousRetrievals(SessionId("dummy")),
      ThirdPartyData.empty,
      TaskIdTaskStatusMapping.empty,
      LocalDate.now(),
      new LookupRegistry(Map.empty[Register, LookupType]),
      Map.empty[BaseComponentId, Register],
      FormComponentIdToFileIdMapping.empty,
      Map.empty[ModelComponentId, List[(FileComponentId, VariadicValue.One)]],
      Option.empty[FormPhase],
      LangADT.En
    )
}
