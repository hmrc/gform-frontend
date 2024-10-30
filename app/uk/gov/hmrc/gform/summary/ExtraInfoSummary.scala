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

package uk.gov.hmrc.gform.summary

import play.twirl.api.Html
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.gform.{ HasErrors, SummaryPagePurpose }
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, FormTemplate, LayoutDisplayWidth, SectionNumber }

final case class ExtraInfoSummary(
  formTemplate: FormTemplate,
  snippets: List[Html],
  maybeAccessCode: Option[AccessCode],
  lastSectionNumber: SectionNumber,
  renderComeBackLater: Boolean,
  continueLabelKey: String,
  frontendAppConfig: FrontendAppConfig,
  summaryPagePurpose: SummaryPagePurpose,
  title: String,
  caption: Option[String],
  header: Html,
  summaryDeclaration: Html,
  footer: Html,
  formDataFingerprint: String,
  displayWidth: LayoutDisplayWidth.LayoutDisplayWidth,
  pageLevelError: HasErrors,
  maybeCoordinates: Option[Coordinates],
  taskCompleted: Option[Boolean]
)
