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

package uk.gov.hmrc.gform.views

import play.api.mvc.Request
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.csp.WebchatClient
import uk.gov.hmrc.play.config.AssetsConfig
import uk.gov.hmrc.play.views.html.helpers.{ ErrorInline, ReportAProblemLink }
import uk.gov.hmrc.play.views.html.layouts.{ Article, Footer, FooterLinks, GTMSnippet, Head, HeaderNav, MainContent, MainContentHeader, OptimizelySnippet, ServiceInfo, Sidebar }
import views.html.layouts.GovUkTemplate

trait ViewHelpersAlgebra {
  def errorInline(errorKey: String, errorMessage: String, classes: Seq[String] = Seq.empty): HtmlFormat.Appendable

  def article(
    content: Html,
    includeGridWrapper: Boolean = false,
    articleClasses: Option[String] = None): HtmlFormat.Appendable

  def sidebar(sidebarLinks: Html, sidebarClass: Option[String] = None): HtmlFormat.Appendable

  def head(linkElem: Option[Html], headScripts: Option[Html]): HtmlFormat.Appendable

  def headerNav(
    navTitle: Option[String],
    navTitleLink: Option[play.api.mvc.Call],
    showBetaLink: Boolean,
    navLinks: Option[Html]): HtmlFormat.Appendable

  def footer(
    analyticsToken: scala.Option[scala.Predef.String],
    analyticsHost: scala.Predef.String,
    ssoUrl: scala.Option[scala.Predef.String],
    scriptElem: scala.Option[play.twirl.api.Html],
    gaCalls: scala.Option[(String, String) => Html],
    analyticsAnonymizeIp: scala.Boolean = false,
    analyticsAdditionalJs: scala.Option[play.twirl.api.Html] = None,
    allowQueryStringInAnalytics: scala.Boolean = false): HtmlFormat.Appendable

  def footerLinks(
    additionalLinks: scala.Option[play.twirl.api.Html],
    euExitLinks: scala.Option[play.twirl.api.Html] = None,
    accessibilityFooterUrl: scala.Option[scala.Predef.String] = None)(
    implicit messages: play.api.i18n.Messages): play.twirl.api.HtmlFormat.Appendable

  def govUkTemplate(title: scala.Option[scala.Predef.String], bodyClasses: scala.Option[scala.Predef.String])(
    head: play.twirl.api.Html,
    bodyEnd: play.twirl.api.Html,
    insideHeader: play.twirl.api.Html,
    afterHeader: play.twirl.api.Html,
    footerTop: play.twirl.api.Html,
    footerLinks: scala.Option[play.twirl.api.Html],
    nav: scala.Boolean)(content: play.twirl.api.Html)(
    implicit messages: play.api.i18n.Messages): play.twirl.api.HtmlFormat.Appendable

  def reportAProblemLink(ajaxFormPartialUrl: scala.Predef.String, nonJSFormPageUrl: scala.Predef.String)(
    implicit messages: play.api.i18n.Messages): play.twirl.api.HtmlFormat.Appendable

  def mainContent(
    article: play.twirl.api.Html,
    mainClass: scala.Option[scala.Predef.String],
    mainDataAttributes: scala.Option[play.twirl.api.Html],
    mainContentHeader: play.twirl.api.Html,
    serviceInfo: play.twirl.api.Html,
    sidebar: play.twirl.api.Html,
    getHelpForm: play.twirl.api.Html): play.twirl.api.HtmlFormat.Appendable

  def mainContentHeader(contentHeader: play.twirl.api.Html): play.twirl.api.HtmlFormat.Appendable

  def serviceInfo(
    betaBanner: play.twirl.api.Html,
    includeGridWrapper: scala.Boolean,
    serviceInfoContent: scala.Option[play.twirl.api.Html],
    includeHMRCBranding: scala.Boolean,
    setLang: scala.Predef.String): play.twirl.api.HtmlFormat.Appendable

  def webchatClickToChatScriptPartial(entryPoint: String, template: String)(implicit request: Request[_]): Html
}

class ViewHelpers(
  optimizelySnippet: OptimizelySnippet,
  assetsConfig: AssetsConfig,
  gtmSnippet: GTMSnippet,
  webChatClient: WebchatClient)
    extends ViewHelpersAlgebra {
  def errorInline(errorKey: String, errorMessage: String, classes: Seq[String] = Seq.empty): HtmlFormat.Appendable =
    new ErrorInline().apply(errorKey, errorMessage, classes)

  def article(
    content: Html,
    includeGridWrapper: Boolean = false,
    articleClasses: Option[String] = None): HtmlFormat.Appendable =
    new Article().apply(content, includeGridWrapper, articleClasses)

  def sidebar(sidebarLinks: Html, sidebarClass: Option[String] = None): HtmlFormat.Appendable =
    new Sidebar().apply(sidebarLinks, sidebarClass)

  def head(linkElem: Option[Html], headScripts: Option[Html]): HtmlFormat.Appendable =
    new Head(optimizelySnippet, assetsConfig, gtmSnippet).apply(linkElem, headScripts)

  def headerNav(
    navTitle: Option[String],
    navTitleLink: Option[play.api.mvc.Call],
    showBetaLink: Boolean,
    navLinks: Option[Html]): HtmlFormat.Appendable =
    new HeaderNav().apply(navTitle, navTitleLink, showBetaLink, navLinks)

  def footer(
    analyticsToken: scala.Option[scala.Predef.String],
    analyticsHost: scala.Predef.String,
    ssoUrl: scala.Option[scala.Predef.String],
    scriptElem: scala.Option[play.twirl.api.Html],
    gaCalls: scala.Option[(String, String) => Html],
    analyticsAnonymizeIp: scala.Boolean,
    analyticsAdditionalJs: scala.Option[play.twirl.api.Html],
    allowQueryStringInAnalytics: scala.Boolean): HtmlFormat.Appendable =
    new Footer(assetsConfig).apply(
      analyticsToken,
      analyticsHost,
      ssoUrl,
      scriptElem,
      gaCalls,
      analyticsAnonymizeIp,
      analyticsAdditionalJs,
      allowQueryStringInAnalytics)

  def footerLinks(
    additionalLinks: scala.Option[play.twirl.api.Html],
    euExitLinks: scala.Option[play.twirl.api.Html],
    accessibilityFooterUrl: scala.Option[scala.Predef.String])(
    implicit messages: play.api.i18n.Messages): play.twirl.api.HtmlFormat.Appendable =
    new FooterLinks().apply(additionalLinks, euExitLinks, accessibilityFooterUrl)

  def govUkTemplate(title: scala.Option[scala.Predef.String], bodyClasses: scala.Option[scala.Predef.String])(
    head: play.twirl.api.Html,
    bodyEnd: play.twirl.api.Html,
    insideHeader: play.twirl.api.Html,
    afterHeader: play.twirl.api.Html,
    footerTop: play.twirl.api.Html,
    footerLinks: scala.Option[play.twirl.api.Html],
    nav: scala.Boolean)(content: play.twirl.api.Html)(
    implicit messages: play.api.i18n.Messages): play.twirl.api.HtmlFormat.Appendable =
    new GovUkTemplate()
      .apply(title, bodyClasses)(head, bodyEnd, insideHeader, afterHeader, footerTop, footerLinks, nav)(content)

  def reportAProblemLink(ajaxFormPartialUrl: scala.Predef.String, nonJSFormPageUrl: scala.Predef.String)(
    implicit messages: play.api.i18n.Messages): play.twirl.api.HtmlFormat.Appendable =
    new ReportAProblemLink().apply(ajaxFormPartialUrl, nonJSFormPageUrl)

  def mainContent(
    article: play.twirl.api.Html,
    mainClass: scala.Option[scala.Predef.String],
    mainDataAttributes: scala.Option[play.twirl.api.Html],
    mainContentHeader: play.twirl.api.Html,
    serviceInfo: play.twirl.api.Html,
    sidebar: play.twirl.api.Html,
    getHelpForm: play.twirl.api.Html): play.twirl.api.HtmlFormat.Appendable =
    new MainContent().apply(
      article,
      mainClass,
      mainDataAttributes,
      mainContentHeader,
      serviceInfo,
      sidebar = sidebar,
      getHelpForm = getHelpForm)

  def mainContentHeader(contentHeader: play.twirl.api.Html): play.twirl.api.HtmlFormat.Appendable =
    new MainContentHeader().apply(contentHeader)

  def serviceInfo(
    betaBanner: play.twirl.api.Html,
    includeGridWrapper: scala.Boolean,
    serviceInfoContent: scala.Option[play.twirl.api.Html],
    includeHMRCBranding: scala.Boolean,
    setLang: scala.Predef.String): play.twirl.api.HtmlFormat.Appendable =
    new ServiceInfo().apply(betaBanner, includeGridWrapper, serviceInfoContent, includeHMRCBranding, setLang)

  override def webchatClickToChatScriptPartial(entryPoint: String, template: String)(
    implicit request: Request[_]): Html =
    webChatClient.webchatClickToChatScriptPartial(entryPoint, template)
}
