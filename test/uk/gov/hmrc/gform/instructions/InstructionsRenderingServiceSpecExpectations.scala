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

package uk.gov.hmrc.gform.instructions

import java.time.format.DateTimeFormatter

trait InstructionsRenderingServiceSpecExpectations {

  val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
  val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")

  def htmlBase(summaryRows: String) =
    s"""
       |<html>
       |	<head>
       |		<title>Instructions PDF - AAA999 dev test template</title>
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
       |	</head>
       |	<body>
       |		<div class="container-fluid">
       |			<div class="row">
       |				<div class="col-lg-12">
       |            HM Revenue &amp; Customs
       |        </div>
       |			</div>
       |			<div class="row">
       |				<div class="col-lg-12 heading-1">
       |					<p>some-pdf-header</p>
       |				</div>
       |			</div>
       |      $summaryRows
       |			<div class="row">
       |				<div class="col-lg-12 heading-1">
       |					<p>some-pdf-footer</p>
       |				</div>
       |			</div>
       |		</div>
       |	</body>
       |</html>
       |""".stripMargin

  def nonRepeatingSectionsHtml() =
    trimLines(htmlBase(s"""
                          |<div class="row">
                          |    <div class="col-lg-12 heading-1">
                          |        page2Instruction
                          |    </div>
                          |</div>
                          |<div class="row">
                          |    <div class="col-lg-4 label">
                          |        page2Field2Instruction
                          |    </div>
                          |    <div class="col-lg-8">
                          |        page2Field2Value
                          |        <br/>
                          |    </div>
                          |</div>
                          |<div class="row">
                          |    <div class="col-lg-4 label">
                          |        page2Field1Instruction
                          |    </div>
                          |    <div class="col-lg-8">
                          |        page2Field1Value
                          |        <br/>
                          |    </div>
                          |</div>
                          |<div class="row">
                          |    <div class="col-lg-12 heading-1">
                          |        page1Instruction
                          |    </div>
                          |</div>
                          |<div class="row">
                          |    <div class="col-lg-4 label">
                          |        page1Field1Instruction
                          |    </div>
                          |    <div class="col-lg-8">
                          |        page1Field1Value
                          |        <br/>
                          |    </div>
                          |</div>
                          |<div class="row">
                          |    <div class="col-lg-4 label">
                          |        page1Field2Instruction
                          |    </div>
                          |    <div class="col-lg-8">
                          |        page1Field2Value
                          |        <br/>
                          |    </div>
                          |</div>
                          |""".stripMargin))

  def nonRepeatingSectionsWithGroupHtml() =
    trimLines(htmlBase("""
                         |<div class="row">
                         |    <div class="col-lg-12 heading-1">
                         |        page1Instruction
                         |    </div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-12 heading-2">
                         |        page1Field1Instruction
                         |    </div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-4 label">
                         |        page1Field1GroupElement2Instruction
                         |    </div>
                         |    <div class="col-lg-8">page1Field1GroupElement2Value1<br/></div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-4 label">
                         |        page1Field1GroupElement1Instruction
                         |    </div>
                         |    <div class="col-lg-8">page1Field1GroupElement1Value1<br/></div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-12 heading-2">
                         |        page1Field1Instruction
                         |    </div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-4 label">
                         |        page1Field1GroupElement2Instruction
                         |    </div>
                         |    <div class="col-lg-8">page1Field1GroupElement2Value2<br/></div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-4 label">
                         |        page1Field1GroupElement1Instruction
                         |    </div>
                         |    <div class="col-lg-8">page1Field1GroupElement1Value2<br/></div>
                         |</div>
                         |""".stripMargin))

  def repeatingSectionHtml() =
    trimLines(htmlBase("""
                         |<div class="row">
                         |    <div class="col-lg-12 heading-1">
                         |        page1Instruction
                         |    </div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-4 label">
                         |        page1Field1Instruction
                         |    </div>
                         |    <div class="col-lg-8">page1Field1Value1<br/></div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-12 heading-1">
                         |        page1Instruction
                         |    </div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-4 label">
                         |        page1Field1Instruction
                         |    </div>
                         |    <div class="col-lg-8">page1Field1Value2<br/></div>
                         |</div>
                         |""".stripMargin))

  def addToListSectionHtml() =
    trimLines(
      htmlBase(
        """
          |<div class="row">
          |    <div class="col-lg-12 heading-1">
          |        addToListShortName
          |    </div>
          |</div>
          |<div class="row">
          |    <div class="col-lg-12 heading-1">
          |        page2Instruction
          |    </div>
          |</div>
          |<div class="row">
          |    <div class="col-lg-4 label">
          |        page2FieldInstruction
          |    </div>
          |    <div class="col-lg-8">page2Field-value1<br/></div>
          |</div>
          |<div class="row">
          |    <div class="col-lg-12 heading-1">
          |        page1Instruction
          |    </div>
          |</div>
          |<div class="row">
          |    <div class="col-lg-4 label">
          |        page1FieldInstruction
          |    </div>
          |    <div class="col-lg-8">page1Field-value1<br/></div>
          |</div>
          |<div class="row">
          |    <div class="col-lg-12 heading-1">
          |        addToListShortName
          |    </div>
          |</div>
          |<div class="row">
          |    <div class="col-lg-12 heading-1">
          |        page2Instruction
          |    </div>
          |</div>
          |<div class="row">
          |    <div class="col-lg-4 label">
          |        page2FieldInstruction
          |    </div>
          |    <div class="col-lg-8">page2Field-value2<br/></div>
          |</div>
          |<div class="row">
          |    <div class="col-lg-12 heading-1">
          |        page1Instruction
          |    </div>
          |</div>
          |<div class="row">
          |    <div class="col-lg-4 label">
          |        page1FieldInstruction
          |    </div>
          |    <div class="col-lg-8">page1Field-value2<br/></div>
          |</div>
          |""".stripMargin
      ))

  def revealingChoiceSectionHtml =
    trimLines(htmlBase("""
                         |<div class="row">
                         |    <div class="col-lg-12 heading-1">
                         |        revealingChoicePageInstruction
                         |    </div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-12 heading-2">
                         |        revealingChoiceFieldInstruction
                         |    </div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-12 heading-3">
                         |        choice1
                         |    </div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-4 label">
                         |        revealingChoice1FieldInstruction
                         |    </div>
                         |    <div class="col-lg-8">value1<br/></div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-12 heading-3">
                         |        choice2
                         |    </div>
                         |</div>
                         |<div class="row">
                         |    <div class="col-lg-4 label">
                         |        revealingChoice2FieldInstruction
                         |    </div>
                         |    <div class="col-lg-8">value2<br/></div>
                         |</div>
                         |""".stripMargin))

  def trimLines(input: String): String =
    input.split("\n").map(_.trim).mkString("")

}
