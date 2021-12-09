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

package uk.gov.hmrc.gform.eval

import play.api.i18n.Messages
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId, ModelPageId }
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
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
  dateLookup: Map[ModelComponentId, DateValue],
  addressLookup: Set[BaseComponentId],
  overseasAddressLookup: Set[BaseComponentId],
  pageIdSectionNumberMap: Map[ModelPageId, SectionNumber],
  lang: LangADT,
  messages: Messages,
  indexedComponentIds: List[ModelComponentId]
)
