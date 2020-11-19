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
       |               <dd> 6FJX-HVQL-U4FD </dd>
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

  def nonRepeatingSectionsHtml()(implicit now: LocalDateTime) =
    trimLines(htmlBase("""
                         |<h2> page2Instruction </h2>
                         |<dl>
                         |  <div>
                         |   <dt> page2Field2Instruction </dt>
                         |   <dd> page2Field2Value </dd>
                         |   <dd></dd>
                         |  </div>
                         |  <div>
                         |   <dt> page2Field1Instruction </dt>
                         |   <dd> page2Field1Value </dd>
                         |   <dd></dd>
                         |  </div>
                         |</dl>
                         |<h2> page1Instruction </h2>
                         |<dl>
                         |  <div>
                         |   <dt> page1Field1Instruction </dt>
                         |   <dd> page1Field1Value </dd>
                         |   <dd></dd>
                         |  </div>
                         |  <div>
                         |   <dt> page1Field2Instruction </dt>
                         |   <dd> page1Field2Value </dd>
                         |   <dd></dd>
                         |  </div>
                         |</dl>
                         |""".stripMargin))

  def nonRepeatingSectionsWithGroupHtml()(implicit now: LocalDateTime) =
    trimLines(htmlBase("""
                         | <h2> page1Instruction </h2>
                         | <dl>
                         |   <div>
                         |      <dt> page1Field1Instruction </dt>
                         |      <dd></dd>
                         |      <span></span>
                         |    </div>
                         |    <div>
                         |      <dt> page1Field1GroupElement1Instruction </dt>
                         |      <dd> page1Field1GroupElement1Value1 </dd>
                         |      <dd></dd>
                         |    </div>
                         |    <div>
                         |      <dt> page1Field1GroupElement1Instruction </dt>
                         |      <dd> page1Field1GroupElement1Value2 </dd>
                         |      <dd></dd>
                         |    </div>
                         | </dl>
                         |""".stripMargin))

  def repeatingSectionHtml()(implicit now: LocalDateTime) = trimLines(htmlBase("""
                                                                                 |<h2> page1Instruction </h2>
                                                                                 |<dl>
                                                                                 |   <div>
                                                                                 |      <dt> page1Field1Instruction </dt>
                                                                                 |      <dd> page1Field1Value1 </dd>
                                                                                 |      <dd></dd>
                                                                                 |   </div>
                                                                                 |</dl>
                                                                                 |<h2> page1Instruction </h2>
                                                                                 |<dl>
                                                                                 |   <div>
                                                                                 |      <dt> page1Field1Instruction </dt>
                                                                                 |      <dd> page1Field1Value2 </dd>
                                                                                 |      <dd></dd>
                                                                                 |   </div>
                                                                                 |</dl>
                                                                                 |""".stripMargin))

  def addToListSectionHtml()(implicit now: LocalDateTime) =
    trimLines(
      htmlBase(
        """
          |<h2> addToListSummary </h2>
          |<dl>
          | <div>
          |   <dt> addToList </dt>
          |   <dd> addToList <br>addToList </dd>
          | </div>
          |</dl>
          |<h2> addToListShortName </h2>
          |<h2> page1Instruction </h2>
          |<dl>
          | <div>
          |   <dt> page1FieldInstruction </dt>
          |   <dd> page1Field-value1 </dd>
          |   <dd></dd>
          | </div>
          |</dl>
          |<h2> page2Instruction </h2>
          |<dl>
          |  <div>
          |   <dt> page2FieldInstruction </dt>
          |   <dd> page2Field-value1 </dd>
          |   <dd></dd>
          |  </div>
          |</dl>
          |<h2> addToListShortName </h2>
          |<h2> page1Instruction </h2>
          |<dl>
          |  <div>
          |   <dt> page1FieldInstruction </dt>
          |   <dd> page1Field-value2 </dd>
          |   <dd></dd>
          |  </div>
          |</dl>
          |<h2> page2Instruction </h2>
          |<dl>
          |  <div>
          |   <dt> page2FieldInstruction </dt>
          |   <dd> page2Field-value2 </dd>
          |   <dd></dd>
          |  </div>
          |</dl>
          |""".stripMargin
      ))

  def revealingChoiceSectionHtml(implicit now: LocalDateTime) =
    trimLines(htmlBase("""
                         |<h2> revealingChoicePageInstruction </h2>
                         |<dl>
                         |<div>
                         |   <dt> revealingChoiceFieldInstruction </dt>
                         |   <dd> choice1 </dd>
                         |   <dd></dd>
                         |</div>
                         |<div>
                         |   <dt> revealingChoice1FieldInstruction </dt>
                         |   <dd> revealingChoice1FieldValue </dd>
                         |   <dd></dd>
                         |</div>
                         |</dl>
                         |""".stripMargin))

  def trimLines(input: String): String =
    input.split("\n").map(_.trim).mkString("")

}
