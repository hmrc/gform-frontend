/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel
import play.api.i18n.{ Lang, Langs, MessagesApi }
import play.api.libs.json._
import play.api.mvc.RequestHeader
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OFormatWithTemplateReadFallback
import uk.gov.hmrc.hmrcfrontend.views.viewmodels.language

sealed trait LangADT {
  def langADTToString: String = fold(_ => "en")(_ => "cy")

  def toLanguage: language.Language = fold[language.Language](_ => language.En)(_ => language.Cy)

  def fold[B](f: LangADT.En.type => B)(g: LangADT.Cy.type => B): B = this match {
    case l: LangADT.En.type => f(l)
    case l: LangADT.Cy.type => g(l)
  }
}

object LangADT {
  case object En extends LangADT
  case object Cy extends LangADT

  def langADTToString(langADT: LangADT): String = langADT.langADTToString

  def stringToLangADT(string: String): LangADT = string match {
    case "cy" => Cy
    case _    => En
  }

  def fromRequest(request: RequestHeader, langs: Langs)(implicit messagesApi: MessagesApi): LangADT = {
    val maybeLangFromCookie = request.cookies.get(messagesApi.langCookieName).flatMap(c => Lang.get(c.value))
    val lang: Lang = langs.preferred(maybeLangFromCookie.toSeq ++ request.acceptLanguages)
    stringToLangADT(lang.code)
  }

  private def convertToLang(jsValue: JsValue): JsResult[LangADT] = jsValue match {
    case JsString(message) =>
      message match {
        case "en" => JsSuccess(LangADT.En)
        case "cy" => JsSuccess(LangADT.Cy)
        case l    => JsError("Unsupported language " + l)
      }
    case _ => JsError("Expected Lang, got " + jsValue)
  }

  implicit val langADTReads: Reads[LangADT] = Reads.apply[LangADT](convertToLang)
  implicit val format: OFormat[LangADT] = OFormatWithTemplateReadFallback(langADTReads)

}
