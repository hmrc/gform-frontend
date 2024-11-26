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

package uk.gov.hmrc.gform.pdf

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

trait PdfRenderServiceExpectations {

  val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
  val dateFormat = DateTimeFormatter.ofPattern("d MMM yyyy")

  private def htmlBase(summaryRows: String, bookmarks: String, signatureBox: String)(implicit time: LocalDateTime) =
    s"""
       |<!DOCTYPE html PUBLIC "-//OPENHTMLTOPDF//DOC XHTML Character Entities Only 1.0//EN" "">
       |<html lang="en">
       |	<head>
       |		<title>PDF Title</title>
       |		<style nonce="a-nonce">
       |        body{font-family:Arial,sans-serif;font-size: 16px; margin:50px;}
       |        dl{border-bottom: 1px solid #bfc1c3;}
       |        dt{font-weight: bold;}
       |        dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:15px;}
       |        .signature-date-field {width:25px;height:35px;border:0.5px solid #000;}
       |    </style>
       |    $bookmarks
       |	</head>
       |	<body>
       |    <p> HM Revenue &amp; Customs </p>
       |    <div>
       |      <h1>Some form template</h1>
       |      <h1>Page title</h1>
       |      <p>Some PDF header</p>
       |      $summaryRows
       |      <h3 class="govuk-heading-m">
       |        Submission Details
       |      </h3>
       |      <dl class="govuk-summary-list govuk-!-margin-bottom-9">
       |        <div class = "govuk-summary-list__row">
       |          <dt class="govuk-summary-list__key">
       |            Submission Date
       |          </dt>
       |          <dd class="govuk-summary-list__value">
       |            ${time.format(dateFormat)} ${time.format(timeFormat)}
       |          </dd>
       |          <dd class="govuk-summary-list__actions">
       |          </dd>
       |        </div>
       |        <div class = "govuk-summary-list__row">
       |          <dt class="govuk-summary-list__key">
       |             Submission Reference
       |          </dt>
       |          <dd class="govuk-summary-list__value">
       |            submission-ref
       |          </dd>
       |          <dd class="govuk-summary-list__actions">
       |          </dd>
       |        </div>
       |        <div class = "govuk-summary-list__row">
       |          <dt class="govuk-summary-list__key">
       |            Submission Mark
       |          </dt>
       |          <dd class="govuk-summary-list__value">
       |            abcdefgh
       |          </dd>
       |          <dd class="govuk-summary-list__actions">
       |          </dd>
       |        </div>
       |      </dl>
       |      $signatureBox
       |    </div>
       |	</body>
       |</html>
       |""".stripMargin

  def htmlTabularBase(summaryRows: String, bookmarks: String = "", summaryDetails: String, signatureBox: String) =
    s"""
       |<!DOCTYPE html PUBLIC "-//OPENHTMLTOPDF//DOC XHTML Character Entities Only 1.0//EN" "">
       |<html lang="en">
       |	<head>
       |		<title>PDF Title</title>
       |		<style nonce="a-nonce">
       |         body {
       |          font-family:Arial, sans-serif;
       |          font-size: 16px;
       |         }
       |         body {
       |          margin: 0;
       |         }
       |         * {
       |          box-sizing: border-box;
       |         }
       |         *:before,
       |         *:after {
       |          box-sizing: border-box;
       |         }
       |         .container-fluid {
       |          padding-right: 15px;
       |          padding-left: 15px;
       |          margin-right: auto;
       |          margin-left: auto;
       |         }
       |         .row {
       |          margin-right: -15px;
       |          margin-left: -15px;
       |         }
       |         .container-fluid:before,
       |         .container-fluid:after,
       |         .row:before,
       |         .row:after {
       |          display: table;
       |          content: " ";
       |         }
       |         .container-fluid:after,
       |         .row:after {
       |          clear: both;
       |         }
       |         .col-lg-4, .col-lg-8, .col-lg-12 {
       |          position: relative;
       |          min-height: 1px;
       |          padding-right: 16px;
       |          padding-left: 16px;
       |          padding-top: 11px;
       |          padding-bottom: 11px;
       |          word-wrap: break-word;
       |         }
       |         .col-lg-4, .col-lg-8, .col-lg-12 {
       |          float: left;
       |         }
       |         .col-lg-12 {
       |          width: 100%;
       |         }
       |         .col-lg-4 {
       |          width: 33.33333333%;
       |         }
       |         .col-lg-8 {
       |          width: 66.66666667%;
       |         }
       |         .label { font-weight: bold }
       |         .heading-1 { font-size: 22px; font-weight: bold;}
       |         .heading-2 { font-size: 20px; font-weight: bold;}
       |         .heading-3 { font-size: 18px; font-weight: bold;}
       |         .signature-date-field {width:25px;height:35px;border:0.5px solid #000;}
       |      </style>
       |      $bookmarks
       |	</head>
       |	<body>
       |		<div class="container-fluid">
       |			<div class="row">
       |				<div class="col-lg-12 heading-1">
       |					<p>Some PDF header</p>
       |				</div>
       |			</div>
       |      $summaryRows
       |			<div class="row">
       |				<div class="col-lg-12">
       |					<p>Some PDF footer</p>
       |				</div>
       |			</div>
       |      $summaryDetails
       |      $signatureBox
       |		</div>
       |	</body>
       |</html>
       |""".stripMargin

  val htmlSignatureBoxBase =
    """
      |<p>Signature of authorising officer on behalf of HMRC</p>
      |<div style="width:380px;height:70px;border:0.5px solid #000;"></div>
      |<p>Date  DD MM YYYY</p>
      |<div>
      |    <table style="border-collapse: separate; border-spacing: 3px;">
      |        <tr>
      |            <th class="signature-date-field"/>
      |            <th class="signature-date-field"/>
      |            <th><span>&nbsp;&nbsp;</span></th>
      |            <th class="signature-date-field"/>
      |            <th class="signature-date-field"/>
      |            <th><span>&nbsp;&nbsp;</span></th>
      |            <th class="signature-date-field"/>
      |            <th class="signature-date-field"/>
      |            <th class="signature-date-field"/>
      |            <th class="signature-date-field"/>
      |        </tr>
      |    </table>
      |</div>
      |""".stripMargin

  def htmlSignatureBox =
    s"""
       |<dl>
       |   <dd>$htmlSignatureBoxBase</dd>
       |</dl>
       |""".stripMargin

  def htmlSignatureBoxAsTabular =
    s"""
       |<div class="row">
       |   <div class="col-lg-12">
       |      $htmlSignatureBoxBase
       |   </div>
       |</div>
       |""".stripMargin

  def nonRepeatingPageSummaryPDFHTML(signatureBox: String = "")(implicit time: LocalDateTime) =
    htmlBase(
      """
        |<h2 id="n0">Section Name</h2>
        |<dl>
        |   <h3>
        |     name
        |   </h3>
        |   <p>
        |      name-value<br/>
        |   </p>
        |</dl>
        |<p>Some PDF footer</p>
        |""".stripMargin,
      """
        |<meta name="subject" content="Some form template"></meta>
        |<bookmarks>
        |  <bookmark name="Section Name" href="#n0"/>
        |</bookmarks>
        |""".stripMargin,
      signatureBox
    ).trimLines

  def nonRepeatingPageTabularSummaryPDFHTML(signatureBox: String = "")(implicit time: LocalDateTime) =
    htmlTabularBase(
      s"""
         |<div id="n0">
         |   <div class="row">
         |      <div class="col-lg-12 heading-1">
         |         Section Name
         |      </div>
         |   </div>
         |   <div class="row">
         |      <div class="col-lg-4 label">
         |         name
         |      </div>
         |      <div class="col-lg-8">
         |         name-value<br/>
         |      </div>
         |   </div>
         |</div>
         |
         |""".stripMargin,
      """
        |<meta name="subject" content="Some form template"></meta>
        |<bookmarks>
        |  <bookmark name="Section Name" href="#n0"/>
        |</bookmarks>
        |""".stripMargin,
      s"""
         |<div class="row">
         |    <div class="col-lg-12 heading-2">Submission Details</div>
         |</div>
         |<div class="row">
         |    <div class="col-lg-4 label">Submission Date</div>
         |    <div class="col-lg-8">${time.format(dateFormat)} ${time.format(timeFormat)}</div>
         |</div>
         |<div class="row">
         |    <div class="col-lg-4 label">Submission Reference</div>
         |    <div class="col-lg-8">submission-ref</div>
         |</div>
         |<div class="row">
         |    <div class="col-lg-4 label">Submission Mark</div>
         |    <div class="col-lg-8">abcdefgh</div>
         |</div>""".stripMargin,
      signatureBox
    ).trimLines

  def nonRepeatingPageInstructionPDFHTML(signatureBox: String = "") =
    htmlTabularBase(
      """
        |<div id="n0">
        |   <div class="row">
        |      <div class="col-lg-12 heading-1">
        |         page1-instruction
        |      </div>
        |   </div>
        |   <div class="row">
        |      <div class="col-lg-4 label">
        |         name-instruction
        |      </div>
        |      <div class="col-lg-8">
        |         name-value<br/>
        |      </div>
        |   </div>
        |</div>
        |""".stripMargin,
      """
        |<meta name="subject" content="Some form template"></meta>
        |<bookmarks>
        |  <bookmark name="page1-instruction" href="#n0"/>
        |</bookmarks>
        |""".stripMargin,
      "",
      signatureBox
    ).trimLines

  implicit class StringOps(input: String) {
    def trimLines: String =
      input.split("\n").map(_.trim).mkString("")
  }
}
