/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.gform.gform.SummaryPagePurpose
import uk.gov.hmrc.gform.models.Coordinates
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, SectionNumber, SummaryDisplayWidth }
import uk.gov.hmrc.govukfrontend.views.viewmodels.notificationbanner.NotificationBanner

final case class ExtraInfoSummary(
  formTemplate: FormTemplate,
  snippets: List[Html],
  maybeAccessCode: Option[AccessCode],
  lastSectionNumber: SectionNumber,
  renderComeBackLater: Boolean,
  continueLabelKey: String,
  frontendAppConfig: FrontendAppConfig,
  summaryPagePurpose: SummaryPagePurpose,
  reviewerComments: Option[String],
  title: String,
  caption: Option[String],
  header: Html,
  summaryDeclaration: Html,
  footer: Html,
  formDataFingerprint: String,
  displayWidth: SummaryDisplayWidth.SummaryDisplayWidth,
  maybeCoordinates: Option[Coordinates],
  taskComplete: Boolean,
  notificationBanner: Option[NotificationBanner]
)