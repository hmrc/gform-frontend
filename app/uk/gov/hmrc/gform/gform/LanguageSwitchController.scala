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

import play.api.mvc.{ Action, AnyContent, Controller }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import play.api.i18n.{ I18nSupport, Lang, MessagesApi }
import uk.gov.hmrc.play.language.LanguageUtils

class LanguageSwitchController(config: FrontendAppConfig, implicit val messagesApi: MessagesApi)
    extends Controller with I18nSupport {
  protected def fallbackURL: String = "/"

  protected def languageMap: Map[String, Lang] = config.availableLanguages

  def switchToLang: String => Action[AnyContent] = (lang: String) => switchToLanguage(lang)

  def switchToLanguage(language: String): Action[AnyContent] = Action { implicit request =>
    val lang = languageMap.getOrElse(language, Lang("en"))
    val redirectURL = request.headers.get(REFERER).getOrElse(fallbackURL)
    Redirect(redirectURL).withLang(Lang.apply(lang.code)).flashing(LanguageUtils.FlashWithSwitchIndicator)
  }
}
