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

package uk.gov.hmrc.gform.gform

import play.twirl.api.Html
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AcknowledgementSectionPdf

class HtmlSanitiserSpec extends Spec {

  private implicit val l = LangADT.En

  val input =
    """
<!doctype html>
<html lang="en" class="govuk-template ">
 <head>
  <meta charset="utf-8">
  <title>Check your answers - Minimal declaration - GOV.UK</title>
  <meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">
  <meta name="theme-color" content="#0b0c0c">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <link rel="shortcut icon" sizes="16x16 32x32 48x48" href="/submissions/govuk-frontend/assets/images/favicon.ico" type="image/x-icon">
  <link rel="mask-icon" href="/submissions/govuk-frontend/assets/images/govuk-mask-icon.svg" color="#0b0c0c">
  <link rel="apple-touch-icon" sizes="180x180" href="/submissions/govuk-frontend/assets/images/govuk-apple-touch-icon-180x180.png">
  <link rel="apple-touch-icon" sizes="167x167" href="/submissions/govuk-frontend/assets/images/govuk-apple-touch-icon-167x167.png">
  <link rel="apple-touch-icon" sizes="152x152" href="/submissions/govuk-frontend/assets/images/govuk-apple-touch-icon-152x152.png">
  <link rel="apple-touch-icon" href="/submissions/govuk-frontend/assets/images/govuk-apple-touch-icon.png">
  <link href="/submissions/assets/stylesheets/accessible-autocomplete.min.css" rel="stylesheet">
  <link href="/submissions/assets/stylesheets/gform.css" rel="stylesheet">
  <link href="/submissions/assets/stylesheets/application.css" media="screen" rel="stylesheet" type="text/css">
  <script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start': new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);})(window,document,'script','dataLayer','GTM-TSFTCWZ');</script>
  <script>
          window.getCookie = function (name) {
            var match = document.cookie.match(new RegExp('(^| )' + name + '=([^;]+)'));
            if (match) return match[2];
          };
          window.gform = window.gform || {};
          window.gform.baseLookupUrl = "/submissions/lookup/";
          window.gform.lang = window.getCookie("PLAY_LANG") || "en";
    </script>
  <meta name="format-detection" content="telephone=no">
  <meta property="og:image" content="/submissions/govuk-frontend/assets/images/govuk-opengraph-image.png">
 </head>
 <body class="govuk-template__body ">
  <script>document.body.className = ((document.body.className) ? document.body.className + ' js-enabled' : 'js-enabled');</script> <a href="#main-content" class="govuk-skip-link">Skip to main content</a>
  <header class="govuk-header hmrc-header  hmrc-header--with-additional-navigation" role="banner" data-module="header">
   <div class="govuk-header__container  govuk-width-container">
    <div class="govuk-header__logo"> <a href="https://www.gov.uk/" class="govuk-header__link govuk-header__link--homepage"> <span class="govuk-header__logotype">
       <svg role="presentation" focusable="false" class="govuk-header__logotype-crown" xmlns="http://www.w3.org/2000/svg" viewbox="0 0 132 97" height="32" width="36"> <path fill="currentColor" fill-rule="evenodd" d="M25 30.2c3.5 1.5 7.7-.2 9.1-3.7 1.5-3.6-.2-7.8-3.9-9.2-3.6-1.4-7.6.3-9.1 3.9-1.4 3.5.3 7.5 3.9 9zM9 39.5c3.6 1.5 7.8-.2 9.2-3.7 1.5-3.6-.2-7.8-3.9-9.1-3.6-1.5-7.6.2-9.1 3.8-1.4 3.5.3 7.5 3.8 9zM4.4 57.2c3.5 1.5 7.7-.2 9.1-3.8 1.5-3.6-.2-7.7-3.9-9.1-3.5-1.5-7.6.3-9.1 3.8-1.4 3.5.3 7.6 3.9 9.1zm38.3-21.4c3.5 1.5 7.7-.2 9.1-3.8 1.5-3.6-.2-7.7-3.9-9.1-3.6-1.5-7.6.3-9.1 3.8-1.3 3.6.4 7.7 3.9 9.1zm64.4-5.6c-3.6 1.5-7.8-.2-9.1-3.7-1.5-3.6.2-7.8 3.8-9.2 3.6-1.4 7.7.3 9.2 3.9 1.3 3.5-.4 7.5-3.9 9zm15.9 9.3c-3.6 1.5-7.7-.2-9.1-3.7-1.5-3.6.2-7.8 3.7-9.1 3.6-1.5 7.7.2 9.2 3.8 1.5 3.5-.3 7.5-3.8 9zm4.7 17.7c-3.6 1.5-7.8-.2-9.2-3.8-1.5-3.6.2-7.7 3.9-9.1 3.6-1.5 7.7.3 9.2 3.8 1.3 3.5-.4 7.6-3.9 9.1zM89.3 35.8c-3.6 1.5-7.8-.2-9.2-3.8-1.4-3.6.2-7.7 3.9-9.1 3.6-1.5 7.7.3 9.2 3.8 1.4 3.6-.3 7.7-3.9 9.1zM69.7 17.7l8.9 4.7V9.3l-8.9 2.8c-.2-.3-.5-.6-.9-.9L72.4 0H59.6l3.5 11.2c-.3.3-.6.5-.9.9l-8.8-2.8v13.1l8.8-4.7c.3.3.6.7.9.9l-5 15.4v.1c-.2.8-.4 1.6-.4 2.4 0 4.1 3.1 7.5 7 8.1h.2c.3 0 .7.1 1 .1.4 0 .7 0 1-.1h.2c4-.6 7.1-4.1 7.1-8.1 0-.8-.1-1.7-.4-2.4V34l-5.1-15.4c.4-.2.7-.6 1-.9zM66 92.8c16.9 0 32.8 1.1 47.1 3.2 4-16.9 8.9-26.7 14-33.5l-9.6-3.4c1 4.9 1.1 7.2 0 10.2-1.5-1.4-3-4.3-4.2-8.7L108.6 76c2.8-2 5-3.2 7.5-3.3-4.4 9.4-10 11.9-13.6 11.2-4.3-.8-6.3-4.6-5.6-7.9 1-4.7 5.7-5.9 8-.5 4.3-8.7-3-11.4-7.6-8.8 7.1-7.2 7.9-13.5 2.1-21.1-8 6.1-8.1 12.3-4.5 20.8-4.7-5.4-12.1-2.5-9.5 6.2 3.4-5.2 7.9-2 7.2 3.1-.6 4.3-6.4 7.8-13.5 7.2-10.3-.9-10.9-8-11.2-13.8 2.5-.5 7.1 1.8 11 7.3L80.2 60c-4.1 4.4-8 5.3-12.3 5.4 1.4-4.4 8-11.6 8-11.6H55.5s6.4 7.2 7.9 11.6c-4.2-.1-8-1-12.3-5.4l1.4 16.4c3.9-5.5 8.5-7.7 10.9-7.3-.3 5.8-.9 12.8-11.1 13.8-7.2.6-12.9-2.9-13.5-7.2-.7-5 3.8-8.3 7.1-3.1 2.7-8.7-4.6-11.6-9.4-6.2 3.7-8.5 3.6-14.7-4.6-20.8-5.8 7.6-5 13.9 2.2 21.1-4.7-2.6-11.9.1-7.7 8.8 2.3-5.5 7.1-4.2 8.1.5.7 3.3-1.3 7.1-5.7 7.9-3.5.7-9-1.8-13.5-11.2 2.5.1 4.7 1.3 7.5 3.3l-4.7-15.4c-1.2 4.4-2.7 7.2-4.3 8.7-1.1-3-.9-5.3 0-10.2l-9.5 3.4c5 6.9 9.9 16.7 14 33.5 14.8-2.1 30.8-3.2 47.7-3.2z"></path> <image src="/assets/images/govuk-logotype-crown.png" class="govuk-header__logotype-crown-fallback-image"></image>
       </svg> <span class="govuk-header__logotype-text"> GOV.UK </span> </span> </a>
    </div>
    <div class="govuk-header__content"> <a href="" class="govuk-header__link govuk-header__link--service-name"> Minimal declaration </a>
     <nav class="hmrc-sign-out-nav"> <a class="govuk-link hmrc-sign-out-nav__link" href="/submissions/sign-out/minimal-declaration"> Sign out </a>
     </nav>
    </div>
   </div>
  </header>
  <div class="govuk-width-container">
  </div>
  <div class="govuk-width-container ">
   <noscript> <iframe src="https://www.googletagmanager.com/ns.html?id=GTM-TSFTCWZ" height="0" width="0" style="display: none;
          visibility: hidden"> </iframe>
   </noscript>
   <div class="govuk-phase-banner">
    <p class="govuk-phase-banner__content"> <strong class="govuk-tag govuk-phase-banner__content__tag"> alpha </strong> <span class="govuk-phase-banner__text"> This is a new service - your <a class="govuk-link" href="/contact/beta-feedback-unauthenticated?service=GForm">feedback</a> will help us to improve it. </span> </p>
   </div>
   <div class="hmrc-banner" role="banner">
    <div class="hmrc-organisation-logo">
     <p class="govuk-body-s"> HM Revenue &amp; Customs </p>
    </div>
   </div>
   <nav class="hmrc-language-select" aria-label="Language switcher">
    <ul class="hmrc-language-select__list">
     <li class="hmrc-language-select__list-item"> <span aria-current="true">English</span> </li>
     <li class="hmrc-language-select__list-item"> <a href="/submissions/language/cymraeg" hreflang="cy" lang="cy" rel="alternate" class="govuk-link" data-journey-click="link - click:lang-select:Cymraeg"> <span class="govuk-visually-hidden"> Newid yr iaith ir Gymraeg </span> <span aria-hidden="true">Cymraeg</span> </a> </li>
    </ul>
   </nav>
   <main class="govuk-main-wrapper govuk-main-wrapper--auto-spacing" id="main-content" role="main">
    <div class="govuk-grid-row">
     <div class="govuk-grid-column-two-thirds">
      <h1 class="govuk-heading-l"> Check your answers </h1>
      <div class="govuk-body govuk-!-margin-bottom-9">
       <p>Make sure the information you have given is correct</p>
      </div>
      <form action="/submissions/summary/minimal-declaration/-" method="POST" method="POST" id="gf-form" novalidate class="js-form govuk-!-margin-bottom-9" autocomplete="off">
       <input type="hidden" name="csrfToken" value="c165513304b3df81c6e524160ecb324cc2202b08-1593443728695-0d7220e3fe86f9e4ffca663b">
       <h3 class="govuk-heading-m"> Page A : </h3>
       <dl class="govuk-summary-list govuk-!-margin-bottom-9">
        <div class="govuk-summary-list__row">
         <dt class="govuk-summary-list__key">
           Page A :
         </dt>
         <dd class="govuk-summary-list__value">
           £12.00
         </dd>
         <dd class="govuk-summary-list__actions"> <a class="govuk-link" href="/submissions/form/minimal-declaration/Page-A-?n=0&amp;se=t"> Change<span class="govuk-visually-hidden"> Page A :</span> </a>
         </dd>
        </div>
       </dl>
       <h3 class="govuk-heading-m"> Page B : </h3>
       <dl class="govuk-summary-list govuk-!-margin-bottom-9">
        <div class="govuk-summary-list__row">
         <dt class="govuk-summary-list__key">
           Page B :
         </dt>
         <dd class="govuk-summary-list__value">
           £34.00
         </dd>
         <dd class="govuk-summary-list__actions"> <a class="govuk-link" href="/submissions/form/minimal-declaration/Page-B-?n=1&amp;se=t"> Change<span class="govuk-visually-hidden"> Page B :</span> </a>
         </dd>
        </div>
       </dl>
       <div class="govuk-body">
        <h2>Now send your form</h2>
        <p>You need to submit your form on the next screen.</p>
        <p>Before you do this you can <a href="/submissions/summary/pdf/minimal-declaration" target="_blank" class="govuk-link">print or save a PDF copy of your answers (opens in a new window or tab)</a>.</p>
       </div>
       <input type="hidden" id="gform-action" name="save" value="Declaration"> <button value="SummaryContinue" type="submit" class="govuk-button" data-module="govuk-button"> Save and continue </button>
       <p class="govuk-body"> <a href="#" id="saveComeBackLaterExit" class="govuk-link">Save and come back later</a> </p>
      </form> <a lang="en" hreflang="en" class="govuk-link " target="_blank" href="/contact/problem_reports_nonjs?newTab=true&amp;service=GForm"> Get help with this page (opens in a new window or tab) </a>
     </div>
    </div>
   </main>
  </div>
  <footer class="govuk-footer " role="contentinfo">
   <div class="govuk-width-container ">
    <div class="govuk-footer__meta">
     <div class="govuk-footer__meta-item govuk-footer__meta-item--grow">
      <h2 class="govuk-visually-hidden">Support links</h2>
      <ul class="govuk-footer__inline-list">
       <li class="govuk-footer__inline-list-item"> <a class="govuk-footer__link" href="/help/cookies"> Cookies </a> </li>
       <li class="govuk-footer__inline-list-item"> <a class="govuk-footer__link" href="/help/privacy"> Privacy policy </a> </li>
       <li class="govuk-footer__inline-list-item"> <a class="govuk-footer__link" href="/help/terms-and-conditions"> Terms and conditions </a> </li>
       <li class="govuk-footer__inline-list-item"> <a class="govuk-footer__link" href="https://www.gov.uk/help"> Help using GOV.UK </a> </li>
       <li class="govuk-footer__inline-list-item"> <a class="govuk-footer__link" href="/submissions/accessibility/minimal-declaration" target="_blank" data-sso="false" data-journey-click="footer:Click:Accessibility"> Accessibility </a> </li>
      </ul>
      <svg aria-hidden="true" focusable="false" class="govuk-footer__licence-logo" xmlns="http://www.w3.org/2000/svg" viewbox="0 0 483.2 195.7" height="17" width="41"> <path fill="currentColor" d="M421.5 142.8V.1l-50.7 32.3v161.1h112.4v-50.7zm-122.3-9.6A47.12 47.12 0 0 1 221 97.8c0-26 21.1-47.1 47.1-47.1 16.7 0 31.4 8.7 39.7 21.8l42.7-27.2A97.63 97.63 0 0 0 268.1 0c-36.5 0-68.3 20.1-85.1 49.7A98 98 0 0 0 97.8 0C43.9 0 0 43.9 0 97.8s43.9 97.8 97.8 97.8c36.5 0 68.3-20.1 85.1-49.7a97.76 97.76 0 0 0 149.6 25.4l19.4 22.2h3v-87.8h-80l24.3 27.5zM97.8 145c-26 0-47.1-21.1-47.1-47.1s21.1-47.1 47.1-47.1 47.2 21 47.2 47S123.8 145 97.8 145" />
      </svg> <span class="govuk-footer__licence-description"> All content is available under the <a class="govuk-footer__link" href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/" rel="license">Open Government Licence v3.0</a>, except where otherwise stated </span>
     </div>
     <div class="govuk-footer__meta-item"> <a class="govuk-footer__link govuk-footer__copyright-logo" href="https://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/">© Crown copyright</a>
     </div>
    </div>
   </div>
  </footer>
  <script src="/submissions/assets/javascripts/jquery.min.js"></script>
  <script src="/submissions/assets/javascripts/gformGAEvents.js"></script>
  <script src="/submissions/assets/javascripts/gformFileUpload.js"></script>
  <script src="/submissions/assets/javascripts/gformRepeatingGroups.js"></script>
  <script src="/submissions/assets/javascripts/gformToggleAddressDivsAndLabels.js"></script>
  <script src="/submissions/assets/javascripts/gformFormActionHandlers.js"></script>
  <script src="/submissions/assets/javascripts/gformSummaryLayout.js"></script>
  <script src="/submissions/assets/javascripts/accessible-autocomplete.min.js"></script>
  <script src="/submissions/assets/javascripts/gformAutoComplete.js"></script>
  <script src="/submissions/assets/javascripts/gform.js"></script>
  <script src="/submissions/assets/javascripts/bignumber.min.js"></script>
  <script src="/submissions/assets/lib/govuk-frontend/govuk/all.js"></script>
  <script src="/submissions/assets/lib/hmrc-frontend/hmrc/all.js"></script>
  <meta name="hmrc-timeout-dialog" data-language="en" , data-timeout="900" data-countdown="120" data-keep-alive-url="/submissions/keep-alive" data-sign-out-url="/submissions/sign-out/minimal-declaration" data-title="You’re about to be signed out" data-message="For security reasons, you will be signed out of this service in" data-keep-alive-button-text="Stay signed in" data-sign-out-button-text="Sign out">
  <script>window.GOVUKFrontend.initAll();</script>
  <script>window.HMRCFrontend.initAll();</script>
 </body>
</html>
"""

  val expected = """
<!doctype html>
<html>
 <head>
  <title>Check your answers - Minimal declaration - GOV.UK</title>
  <style>body{font-family:Arial,sans-serif;font-size: 19px;}
dl{border-bottom: 1px solid #bfc1c3;}
dt{font-weight: bold;}
dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;}
    </style>
 </head>
 <body>
  <div>
   <p> HM Revenue &amp; Customs </p>
  </div>
  <form>
   <h3> Page A : </h3>
   <dl>
    <div>
     <dt>
       Page A :
     </dt>
     <dd>
       &pound;12.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h3> Page B : </h3>
   <dl>
    <div>
     <dt>
       Page B :
     </dt>
     <dd>
       &pound;34.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
  </form>
 </body>
</html>
"""

  "HtmlSanitiser.sanitiseHtmlForPDF" should "produce clean pdf having only question-answer content" in {
    val res = HtmlSanitiser.sanitiseHtmlForPDF(Html(input), doc => ())

    noWhitespace(res) shouldBe noWhitespace(expected)

  }

  val expectedSummaryPagePdf =
    """
<!doctype html>
<html>
 <head>
  <title>Check your answers - Minimal declaration - GOV.UK</title>
  <style>body{font-family:Arial,sans-serif;font-size: 19px;}
dl{border-bottom: 1px solid #bfc1c3;}
dt{font-weight: bold;}
dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;}
    </style>
 </head>
 <body>
  <div>
   <p> HM Revenue &amp; Customs </p>
  </div>
  <form>
   <h1>AAA999 dev test template</h1>
   <h1>Summary Title</h1>
   <p>Summary Header</p>
   <h3> Page A : </h3>
   <dl>
    <div>
     <dt>
       Page A :
     </dt>
     <dd>
       &pound;12.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h3> Page B : </h3>
   <dl>
    <div>
     <dt>
       Page B :
     </dt>
     <dd>
       &pound;34.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
  </form>
 </body>
</html>
"""

  "HtmlSanitiser.summaryPagePdf" should "embellish pdf with summary section data" in {
    val res =
      HtmlSanitiser.sanitiseHtmlForPDF(Html(input), doc => HtmlSanitiser.summaryPagePdf(doc, buildFormTemplate))

    noWhitespace(res) shouldBe noWhitespace(expectedSummaryPagePdf)

  }

  val expectedAcknowledgementPdf =
    """
<!doctype html>
<html>
 <head>
  <title>Check your answers - Minimal declaration - GOV.UK</title>
  <style>body{font-family:Arial,sans-serif;font-size: 19px;}
dl{border-bottom: 1px solid #bfc1c3;}
dt{font-weight: bold;}
dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;}
    </style>
 </head>
 <body>
  <div>
   <p> HM Revenue &amp; Customs </p>
  </div>
  <form>
   <h1>AAA999 dev test template</h1>
   <p>It's a Acknowledgement Section Pdf header.</p>
   <h3> Page A : </h3>
   <dl>
    <div>
     <dt>
       Page A :
     </dt>
     <dd>
       &pound;12.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h3> Page B : </h3>
   <dl>
    <div>
     <dt>
       Page B :
     </dt>
     <dd>
       &pound;34.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h2>Extra Data &pound;</h2>
   <p>It's a Acknowledgement Section Pdf footer.</p>
  </form>
 </body>
</html>
"""

  val expectedAcknowledgementPdfWithNoHeaderAndFooter =
    """
<!doctype html>
<html>
 <head>
  <title>Check your answers - Minimal declaration - GOV.UK</title>
  <style>body{font-family:Arial,sans-serif;font-size: 19px;}
dl{border-bottom: 1px solid #bfc1c3;}
dt{font-weight: bold;}
dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;}
    </style>
 </head>
 <body>
  <div>
   <p> HM Revenue &amp; Customs </p>
  </div>
  <form>
   <h1>AAA999 dev test template</h1>
   <h3> Page A : </h3>
   <dl>
    <div>
     <dt>
       Page A :
     </dt>
     <dd>
       &pound;12.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h3> Page B : </h3>
   <dl>
    <div>
     <dt>
       Page B :
     </dt>
     <dd>
       &pound;34.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h2>Extra Data &pound;</h2>
  </form>
 </body>
</html>
"""

  val expectedAcknowledgementPdfWithNoFooter =
    """
<!doctype html>
<html>
 <head>
  <title>Check your answers - Minimal declaration - GOV.UK</title>
  <style>body{font-family:Arial,sans-serif;font-size: 19px;}
dl{border-bottom: 1px solid #bfc1c3;}
dt{font-weight: bold;}
dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;}
    </style>
 </head>
 <body>
  <div>
   <p> HM Revenue &amp; Customs </p>
  </div>
  <form>
   <h1>AAA999 dev test template</h1>
   <p>It's a Acknowledgement Section Pdf header.</p>
   <h3> Page A : </h3>
   <dl>
    <div>
     <dt>
       Page A :
     </dt>
     <dd>
       &pound;12.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h3> Page B : </h3>
   <dl>
    <div>
     <dt>
       Page B :
     </dt>
     <dd>
       &pound;34.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h2>Extra Data &pound;</h2>
  </form>
 </body>
</html>
"""

  val expectedAcknowledgementPdfWithNoHeader =
    """
<!doctype html>
<html>
 <head>
  <title>Check your answers - Minimal declaration - GOV.UK</title>
  <style>body{font-family:Arial,sans-serif;font-size: 19px;}
dl{border-bottom: 1px solid #bfc1c3;}
dt{font-weight: bold;}
dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;}
    </style>
 </head>
 <body>
  <div>
   <p> HM Revenue &amp; Customs </p>
  </div>
  <form>
   <h1>AAA999 dev test template</h1>
   <h3> Page A : </h3>
   <dl>
    <div>
     <dt>
       Page A :
     </dt>
     <dd>
       &pound;12.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h3> Page B : </h3>
   <dl>
    <div>
     <dt>
       Page B :
     </dt>
     <dd>
       &pound;34.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h2>Extra Data &pound;</h2>
   <p>It's a Acknowledgement Section Pdf footer.</p>
  </form>
 </body>
</html>
"""

  "HtmlSanitiser.acknowledgementPdf" should "embellish pdf with summary section data" in {

    val submissionDetails = "<h2>Extra Data £</h2>"
    val res =
      HtmlSanitiser
        .sanitiseHtmlForPDF(
          Html(input),
          doc => HtmlSanitiser.acknowledgementPdf(doc, submissionDetails, buildFormTemplate))

    noWhitespace(res) shouldBe noWhitespace(expectedAcknowledgementPdf)

  }

  it should "embellish pdf with summary section data having AcknowledgementSection with no Header and Footer" in {
    val submissionDetails = "<h2>Extra Data £</h2>"

    val ackSectionWithNoHeaderAndFooter = ackSection.copy(pdf = None)

    val destinationListWithNoAckSectionHeaderAndFooter =
      destinationList.copy(acknowledgementSection = ackSectionWithNoHeaderAndFooter)

    val res =
      HtmlSanitiser.sanitiseHtmlForPDF(
        Html(input),
        doc =>
          HtmlSanitiser.acknowledgementPdf(
            doc,
            submissionDetails,
            buildFormTemplate.copy(destinations = destinationListWithNoAckSectionHeaderAndFooter))
      )

    noWhitespace(res) shouldBe noWhitespace(expectedAcknowledgementPdfWithNoHeaderAndFooter)
  }

  it should "embellish pdf with summary section data having AcknowledgementSection with no Footer" in {
    val submissionDetails = "<h2>Extra Data £</h2>"

    val ackSectionWithNoFooter = ackSection.copy(
      pdf = Some(AcknowledgementSectionPdf(Some(toSmartString("It's a Acknowledgement Section Pdf header.")), None)))

    val destinationListWithNoAckSectionFooter =
      destinationList.copy(acknowledgementSection = ackSectionWithNoFooter)

    val res =
      HtmlSanitiser.sanitiseHtmlForPDF(
        Html(input),
        doc =>
          HtmlSanitiser
            .acknowledgementPdf(
              doc,
              submissionDetails,
              buildFormTemplate.copy(destinations = destinationListWithNoAckSectionFooter))
      )

    noWhitespace(res) shouldBe noWhitespace(expectedAcknowledgementPdfWithNoFooter)
  }

  it should "embellish pdf with summary section data having AcknowledgementSection with no Header" in {
    val submissionDetails = "<h2>Extra Data £</h2>"

    val ackSectionWithNoHeader = ackSection.copy(
      pdf = Some(AcknowledgementSectionPdf(None, Some(toSmartString("It's a Acknowledgement Section Pdf footer.")))))

    val destinationListWithNoAckSectionHeader =
      destinationList.copy(acknowledgementSection = ackSectionWithNoHeader)

    val res =
      HtmlSanitiser.sanitiseHtmlForPDF(
        Html(input),
        doc =>
          HtmlSanitiser
            .acknowledgementPdf(
              doc,
              submissionDetails,
              buildFormTemplate.copy(destinations = destinationListWithNoAckSectionHeader))
      )

    noWhitespace(res) shouldBe noWhitespace(expectedAcknowledgementPdfWithNoHeader)
  }

  val expectedPrintSectionPdf =
    """
<!doctype html>
<html>
 <head>
  <title>Check your answers - Minimal declaration - GOV.UK</title>
  <style>body{font-family:Arial,sans-serif;font-size: 19px;}
dl{border-bottom: 1px solid #bfc1c3;}
dt{font-weight: bold;}
dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;}
    </style>
 </head>
 <body>
  <div>
   <p> HM Revenue &amp; Customs </p>
  </div>
  <form>
   <h2>Header Html &pound;</h2>
   <h3> Page A : </h3>
   <dl>
    <div>
     <dt>
       Page A :
     </dt>
     <dd>
       &pound;12.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h3> Page B : </h3>
   <dl>
    <div>
     <dt>
       Page B :
     </dt>
     <dd>
       &pound;34.00
     </dd>
     <dd>
     </dd>
    </div>
   </dl>
   <h2>Footer Html &pound;</h2>
  </form>
 </body>
</html>
"""

  "HtmlSanitiser.printSectionPdf" should "embellish pdf with summary section data" in {

    val headerHtml = "<h2>Header Html £</h2>"
    val footerHtml = "<h2>Footer Html £</h2>"
    val res =
      HtmlSanitiser.sanitiseHtmlForPDF(Html(input), doc => HtmlSanitiser.printSectionPdf(doc, headerHtml, footerHtml))

    noWhitespace(res) shouldBe noWhitespace(expectedPrintSectionPdf)

  }

  private def noWhitespace(s: String): String = s.replaceAll("\\s", "")
}
