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

package uk.gov.hmrc.gform.views

import java.time.{ Instant, ZoneId }
import java.time.format.DateTimeFormatter
import java.util.Locale
import play.api.i18n.Messages
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.gform.routes
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.govukfrontend.views.Aliases.HtmlContent
import uk.gov.hmrc.hmrcfrontend.views.viewmodels.footer.FooterItem
import uk.gov.hmrc.gform.testonly.routes.TestOnlyController

package object html {

  def summaryTextArea(str: String): Html = {
    val replaceBy = "<br/>"
    val escaped = HtmlFormat.escape(str).body

    // https://stackoverflow.com/a/14217315/2522894
    val replaced = List("\r\n", "\r", "\n").foldLeft(escaped) { case (acc, seq) =>
      acc.replaceAll(seq, replaceBy)
    }

    Html(replaced)
  }

  def localisedDateString(dateString: String)(implicit messages: Messages): String = {
    val dateParts: Array[java.lang.String] = dateString.split(" +")
    dateParts(1) = messages(s"date.${dateParts(1)}")
    dateParts.mkString(" ")
  }

  private val dtf = DateTimeFormatter
    .ofPattern("dd MMM yyyy HH:mm:ss")
    .withLocale(Locale.UK)
    .withZone(ZoneId.of("Europe/London"))

  def formatInstant(instant: Instant): String = dtf.format(instant)

  def testOnlyAdditionalFooterItems(
    maybeFormTemplate: Option[FormTemplate],
    accessCode: Option[AccessCode]
  ): Seq[FooterItem] =
    maybeFormTemplate.fold(Seq.empty[FooterItem]) { formTemplate =>
      startNewFormFooterItem(formTemplate) +: accessCode.fold(Seq(toolboxFooterItem(formTemplate))) { accessCode =>
        Seq(toolboxFooterItemWithAccessCode(formTemplate, accessCode))
      } :+ createNewSnapshot(formTemplate, accessCode) :+ documentationFooterItem
    }

  private def toolboxFooterItem(formTemplate: FormTemplate) =
    new FooterItem(
      text = Some("Toolbox"),
      href = Some(s"/submissions/test-only/toolbox/${formTemplate._id.value}"),
      attributes = Map.empty
    )

  private def toolboxFooterItemWithAccessCode(formTemplate: FormTemplate, accessCode: AccessCode) =
    new FooterItem(
      text = Some("Toolbox"),
      href = Some(s"/submissions/test-only/toolbox/${formTemplate._id.value}/${accessCode.value}"),
      attributes = Map.empty
    )

  private def startNewFormFooterItem(formTemplate: FormTemplate) = {
    val newFormUrl = routes.NewFormController.dashboard(formTemplate._id).url
    new FooterItem(
      text = Some("New form"),
      href = Some(newFormUrl),
      attributes = Map.empty
    )
  }

  private def documentationFooterItem =
    new FooterItem(
      text = Some("Documentation"),
      href = Some("https://github.com/hmrc/gform-frontend/wiki"),
      attributes = Map("target" -> "_blank")
    )

  private def createNewSnapshot(formTemplate: FormTemplate, accessCode: Option[AccessCode]) = {
    val createSnapshotUrl = TestOnlyController.createSnapshot(formTemplate._id, accessCode).url
    new FooterItem(
      text = Some("Quick save"),
      href = Some(createSnapshotUrl),
      attributes = Map("target" -> "_blank")
    )
  }

  import uk.gov.hmrc.hmrcfrontend.views.viewmodels.language.{ Cy, En, LanguageSelect }
  import uk.gov.hmrc.govukfrontend.views.Aliases.ServiceNavigation
  import uk.gov.hmrc.hmrcfrontend.views.html.components.HmrcServiceNavigationLanguageSelect
  import uk.gov.hmrc.govukfrontend.views.viewmodels.servicenavigation.ServiceNavigationSlot
  import uk.gov.hmrc.hmrcfrontend.config.SupportedLanguagesConfig

  // This is variation of RichServiceNavigationSupport.scala
  // https://github.com/hmrc/play-frontend-hmrc/blob/034abab2c1b68625d5032341a07b5de9b5ee61a7/play-frontend-hmrc-play-30/src/main/scala/uk/gov/hmrc/govukfrontend/views/implicits/RichServiceNavigationSupport.scala#L31
  def withLanguageToggle(
    serviceNavigation: ServiceNavigation,
    appConfig: FrontendAppConfig,
    maybeFormTemplate: Option[FormTemplate],
    accessCode: Option[AccessCode]
  )(implicit
    messages: Messages
  ): ServiceNavigation = {

    val enLink = appConfig.languageSwitchCall(maybeFormTemplate, accessCode, SupportedLanguagesConfig.en)
    val cyLink = appConfig.languageSwitchCall(maybeFormTemplate, accessCode, SupportedLanguagesConfig.cy)

    val languageSelect: LanguageSelect =
      LanguageSelect(
        if (messages.lang.code == SupportedLanguagesConfig.cy) Cy else En,
        (En, enLink.url),
        (Cy, cyLink.url)
      )

    val languageSelectHtml = new HmrcServiceNavigationLanguageSelect()(languageSelect)

    val slots: Option[ServiceNavigationSlot] = serviceNavigation.slots
      .map(_.copy(end = HtmlContent(languageSelectHtml)))
      .orElse(Some(ServiceNavigationSlot(end = HtmlContent(languageSelectHtml))))
    serviceNavigation.copy(
      slots = slots,
      classes = s"${serviceNavigation.classes} hmrc-service-navigation--with-language-select".trim
    )
  }

}
