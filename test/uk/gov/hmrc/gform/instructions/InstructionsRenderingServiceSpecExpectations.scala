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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

trait InstructionsRenderingServiceSpecExpectations {

  val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
  val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")

  def htmlBase(rows: String)(implicit now: LocalDateTime) =
    s"""
       |<!doctype html>
       |<html>
       |   <head>
       |      <title>Instructions - AAA999 dev test template</title>
       |      <style>body{font-family:Arial,sans-serif;font-size: 19px;}
       |         dl{border-bottom: 1px solid #bfc1c3;}
       |         dt{font-weight: bold;}
       |         dt,dd{margin:0; width: 100%; display:block; text-align:left; padding-left:0;padding-bottom:10px;} </style>
       |   </head>
       |   <body>
       |      <div>
       |         <p> HM Revenue &amp; Customs </p>
       |      </div>
       |      <form>
       |         <h1><p>some-pdf-header</p></h1>
       |         $rows
       |         <h3> submission.details </h3>
       |         <dl>
       |            <div>
       |               <dt> submission.date </dt>
       |               <dd> ${now.format(dateFormat)} ${now.format(timeFormat)} </dd>
       |               <dd></dd>
       |            </div>
       |            <div>
       |               <dt> submission.reference </dt>
       |               <dd> some-submission-ref </dd>
       |               <dd></dd>
       |            </div>
       |            <div>
       |               <dt> submission.mark </dt>
       |               <dd></dd>
       |               <dd></dd>
       |            </div>
       |         </dl>
       |         <h1><p>some-pdf-footer</p></h1>
       |      </form>
       |   </body>
       |</html>
       |""".stripMargin

  def htmlBase2(summaryRows: String)(implicit now: LocalDateTime) =
    s"""
       |<html>
       |<head>
       |   <title>Instructions PDF - AAA999 dev test template</title>
       |   <style>
       |    body { font-family:Arial, sans-serif; font-size: 16px; }
       |    td,th { vertical-align: top; text-align: left; padding-bottom: 15px; }
       |    th { font-weight: bold; }
       |    .pdf-header-footer { font-size: 25px; font-weight: bold; }
       |    .section-title { font-size: 25px; font-weight: bold; }
       |    .section-bottom-border { border-bottom: 2px solid #bfc1c3; }
       |    .field-label { font-weight: bold; width: 40%; }
       |    .field-value { width: 60%; }
       |   </style>
       |</head>
       |<body>
       |   <div>
       |      <p>HM Revenue &amp; Customs</p>
       |   </div>
       |   <table>
       |    <tr>
       |      <th class="pdf-header-footer"> <p>some-pdf-header</p> </th>
       |    </tr>
       |   </table>
       |   $summaryRows
       |   <table>
       |   <tr>
       |      <th class="section-title" colspan="2"> submission.details </th>
       |   </tr>
       |   <tr>
       |      <th> submission.date </th>
       |      <td> ${now.format(dateFormat)} ${now.format(timeFormat)} </td>
       |   </tr>
       |   <tr>
       |      <th> submission.reference </th>
       |      <td> some-submission-ref </td>
       |   </tr>
       |   <tr>
       |      <th> submission.mark </th>
       |      <td> </td>
       |   </tr>
       |   </table>
       |   <table>
       |    <tr>
       |      <th class="pdf-header-footer"> <p>some-pdf-footer</p> </th>
       |    </tr>
       |   </table>
       |</body>
       |</html>
       |""".stripMargin

  def nonRepeatingSectionsHtml()(implicit now: LocalDateTime) =
    trimLines(htmlBase2(s"""
                           |<table>
                           |   <tr>
                           |      <th class="section-title" colspan="2">page2Instruction</th>
                           |   </tr>
                           |   <tr>
                           |      <th>page2Field2Instruction</th>
                           |      <td>page2Field2Value<br/></td>
                           |   </tr>
                           |   <tr>
                           |      <th>page2Field1Instruction</th>
                           |      <td>page2Field1Value<br/></td>
                           |   </tr>
                           |</table>
                           |<table>
                           |   <tr>
                           |      <th class="section-title" colspan="2">page1Instruction</th>
                           |   </tr>
                           |   <tr>
                           |      <th>page1Field1Instruction</th>
                           |      <td>page1Field1Value<br/></td>
                           |   </tr>
                           |   <tr>
                           |      <th>page1Field2Instruction</th>
                           |      <td>page1Field2Value<br/></td>
                           |   </tr>
                           |</table>
                           |""".stripMargin))

  def nonRepeatingSectionsWithGroupHtml()(implicit now: LocalDateTime) =
    trimLines(htmlBase2("""
                          |<table>
                          |<tr>
                          |    <th class="section-title" colspan="2">page1Instruction</th>
                          |</tr>
                          |<tr>
                          |    <th class="section-title" colspan="2">page1Field1Instruction</th>
                          |</tr>
                          |<tr>
                          |    <th>page1Field1GroupElement1Instruction</th>
                          |    <td>page1Field1GroupElement1Value1<br/></td>
                          |</tr>
                          |<tr>
                          |     <th class="section-title" colspan="2">page1Field1Instruction</th>
                          |</tr>
                          |<tr>
                          |    <th>page1Field1GroupElement1Instruction</th>
                          |    <td>page1Field1GroupElement1Value2<br/></td>
                          |</tr>
                          |</table>
                          |""".stripMargin))

  def repeatingSectionHtml()(implicit now: LocalDateTime) =
    trimLines(htmlBase2("""
                          |<table>
                          |<tr>
                          |   <th class="section-title" colspan="2">page1Instruction</th>
                          |</tr>
                          |<tr>
                          |   <th>page1Field1Instruction</th>
                          |   <td>page1Field1Value1<br/></td>
                          |</tr>
                          |</table>
                          |<table>
                          |<tr>
                          |   <th class="section-title" colspan="2">page1Instruction</th>
                          |</tr>
                          |<tr>
                          |   <th>page1Field1Instruction</th>
                          |   <td>page1Field1Value2<br/></td>
                          |</tr>
                          |</table>
                          |""".stripMargin))

  def addToListSectionHtml()(implicit now: LocalDateTime) =
    trimLines(
      htmlBase2(
        """
          |<table>
          |<tr>
          |   <th class="section-title">addToListShortName</th>
          |</tr>
          |<tr>
          |<td>
          |  <table>
          |    <tr>
          |      <th class="section-title" colspan="2">page1Instruction</th>
          |    </tr>
          |    <tr>
          |        <th>page1FieldInstruction</th>
          |        <td>page1Field-value1<br/></td>
          |     </tr>
          |  </table>
          |</td>
          |</tr>
          |<tr>
          |<td>
          |  <table>
          |    <tr>
          |      <th class="section-title" colspan="2">page2Instruction</th>
          |    </tr>
          |    <tr>
          |      <th>page2FieldInstruction</th>
          |      <td>page2Field-value1<br/></td>
          |    </tr>
          |   </table>
          |</td>
          | </tr>
          |<tr>
          |   <th class="section-title">addToListShortName</th>
          |</tr>
          |<tr>
          |<td>
          |  <table>
          |    <tr>
          |      <th class="section-title" colspan="2">page1Instruction</th>
          |    </tr>
          |    <tr>
          |       <th>page1FieldInstruction</th>
          |       <td>page1Field-value2<br/></td>
          |    </tr>
          |  </table>
          |</td>
          |</tr>
          |<tr>
          |<td>
          |   <table>
          |     <tr>
          |       <th class="section-title" colspan="2">page2Instruction</th>
          |     </tr>
          |     <tr>
          |       <th>page2FieldInstruction</th>
          |       <td>page2Field-value2<br/></td>
          |     </tr>
          |    </table>
          |</td>
          |</tr>
          |</table>
          |""".stripMargin
      ))

  def revealingChoiceSectionHtml(implicit now: LocalDateTime) =
    trimLines(htmlBase2("""
                          |<table>
                          |<tr>
                          |   <th class="section-title" colspan="2">revealingChoicePageInstruction</th>
                          |</tr>
                          |<tr>
                          |   <th class="section-title" colspan="2">revealingChoiceFieldInstruction</th>
                          |</tr>
                          |<tr>
                          |   <th class="section-title" colspan="2">choice1</th>
                          |</tr>
                          |<tr>
                          |   <th>revealingChoice1FieldInstruction</th>
                          |   <td>value1<br/></td>
                          |</tr>
                          |<tr>
                          |   <th class="section-title" colspan="2">choice2</th>
                          |</tr>
                          |<tr>
                          |   <th>revealingChoice2FieldInstruction</th>
                          |   <td>value2<br/></td>
                          |</tr>
                          |</table>
                          |""".stripMargin))

  def trimLines(input: String): String =
    input.split("\n").map(_.trim).mkString("")

}
