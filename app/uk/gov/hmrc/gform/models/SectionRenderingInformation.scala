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

import play.api.mvc.Call
import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.config.FileExtension
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.LayoutDisplayWidth.LayoutDisplayWidth
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.upscan.UpscanData

final case class SectionRenderingInformation(
  formTemplateId: FormTemplateId,
  maybeAccessCode: Option[AccessCode],
  sectionNumber: SectionNumber,
  sectionHeader: SectionHeader,
  noPIITitle: String,
  snippets: List[Html],
  javascripts: String,
  envelopeId: EnvelopeId,
  formAction: Call,
  renderComeBackLater: Boolean,
  continueLabel: String,
  formMaxAttachmentSizeMB: Int,
  allowedFileTypes: AllowedFileTypes,
  restrictedFileExtensions: List[FileExtension],
  upscanData: Map[FormComponentId, UpscanData] = Map.empty,
  fileUploadMaxSize: Map[FormComponentId, Int] = Map.empty,
  displayWidth: Option[LayoutDisplayWidth] = None
)

final case class SectionHeader(
  sectionTitle: String,
  sectionDescription: Option[String],
  caption: Option[String] = None
)
