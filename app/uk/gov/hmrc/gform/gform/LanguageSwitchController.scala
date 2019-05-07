/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import play.api.Application
import play.api.i18n.{ Lang, MessagesApi }
import play.api.mvc.{ Action, AnyContent }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.i18n.LanguageController

class LanguageSwitchController(
  config: FrontendAppConfig,
  override implicit val messagesApi: MessagesApi,
  implicit val app: Application)
    extends LanguageController {

  override protected def fallbackURL: String = "/"

  override protected def languageMap: Map[String, Lang] = config.getAvailableLanguages

  def switchToLang: String => Action[AnyContent] = (lang: String) => switchToLanguage(lang)

}
