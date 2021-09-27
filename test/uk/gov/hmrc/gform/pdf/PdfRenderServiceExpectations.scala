/*
 * Copyright 2021 HM Revenue & Customs
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
  val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")

  private def htmlBase(summaryRows: String, bookmarks: String)(implicit time: LocalDateTime) =
    s"""
       |<html>
       |	<head>
       |		<title>PDF Title</title>
       |		<style>
       |        body{font-family:Arial,sans-serif;font-size: 16px; margin:50px;}
       |        dl{border-bottom: 1px solid #bfc1c3;}
       |        dt{font-weight: bold;}
       |        dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:15px;}
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
       |    </div>
       |	</body>
       |</html>
       |""".stripMargin

  def htmlTabularBase(summaryRows: String, bookmarks: String = "") =
    s"""
       |<html>
       |	<head>
       |		<title>PDF Title</title>
       |		<style>
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
       |				<div class="col-lg-12 heading-1">
       |					<p>Some PDF footer</p>
       |				</div>
       |			</div>
       |		</div>
       |	</body>
       |</html>
       |""".stripMargin

  def nonRepeatingPageSummaryPDFHTML(implicit time: LocalDateTime) =
    htmlBase(
      """
        |<h2 id="0">Section Name</h2>
        |<dl>
        |   <dt>
        |     name
        |   </dt>
        |   <dd>
        |      name-value<br/>
        |   </dd>
        |</dl>
        |<p>Some PDF footer</p>
        |""".stripMargin,
      """
        |<bookmarks>
        |  <bookmark name="Section Name" href="#0"/>
        |</bookmarks>
        |""".stripMargin
    ).trimLines

  def nonRepeatingPageInstructionPDFHTML =
    htmlTabularBase(
      """
        |<div id="0">
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
        |<bookmarks>
        |  <bookmark name="page1-instruction" href="#0"/>
        |</bookmarks>
        |""".stripMargin
    ).trimLines

  implicit class StringOps(input: String) {
    def trimLines: String =
      input.split("\n").map(_.trim).mkString("")
  }
}
