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

package uk.gov.hmrc.gform.eval

import cats.data.NonEmptyList
import play.api.i18n.Messages
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ Basic, DataRetrieveAll, PageModel }
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId, ModelPageId }
import uk.gov.hmrc.gform.sharedmodel.VariadicValue
import uk.gov.hmrc.gform.sharedmodel.form.{ TaskIdTaskStatusMapping, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, SubmissionRef }
import uk.gov.hmrc.http.HeaderCarrier

// Various information needed for Expr evaluation
final case class EvaluationContext(
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  maybeAccessCode: Option[AccessCode],
  retrievals: MaterialisedRetrievals,
  thirdPartyData: ThirdPartyData,
  authConfig: AuthConfig,
  headerCarrier: HeaderCarrier,
  formPhase: Option[FormPhase],
  fileIdsWithMapping: FileIdsWithMapping,
  multiFilesData: Map[ModelComponentId, List[(FileComponentId, VariadicValue.One)]],
  dateLookup: Map[ModelComponentId, DateValue],
  addressLookup: Set[BaseComponentId],
  overseasAddressLookup: Set[BaseComponentId],
  postcodeLookup: Set[BaseComponentId],
  pageIdSectionNumberMap: Map[ModelPageId, SectionNumber],
  lang: LangADT,
  messages: Messages,
  indexedComponentIds: Map[BaseComponentId, List[ModelComponentId]],
  taxPeriodYear: Set[BaseComponentId],
  fileSizeLimit: FileSizeLimit,
  dataRetrieveAll: DataRetrieveAll,
  hideChoicesSelected: Set[ModelComponentId], // ids of choices with "hideChoicesSelected": true
  choiceLookup: Map[ModelComponentId, NonEmptyList[OptionData]],
  addToListIds: Set[AddToListId],
  lookupRegistry: LookupRegistry,
  lookupRegister: Map[BaseComponentId, Register],
  constraints: Map[BaseComponentId, TextConstraint],
  taskIdTaskStatus: TaskIdTaskStatusMapping,
  currentSection: Option[SectionNumber] = None
)
