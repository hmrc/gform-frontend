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

package uk.gov.hmrc.gform.sharedmodel

import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ ExprGen, PrimitiveGen }

class SmartStringSpec extends Spec {

  def welsh: String = "Welsh"
  def english: String = "English"

  "JSON" should "round trip" in {
    forAll(Gen.asciiStr, Gen.asciiStr, PrimitiveGen.zeroOrMoreGen(ExprGen.exprGen())) { (english, welsh, exprs) =>
      val cEnglish = condition(english)
      val cWelsh = condition(welsh)

      val smartString = SmartString(LocalisedString(Map(LangADT.En -> cEnglish, LangADT.Cy -> cWelsh)), exprs)

      verifyRoundTrip(smartString)

    }
  }

  "SmartString when Welsh requested" should "return English if Welsh not defined" in {
    implicit val l: LangADT = LangADT.Cy
    val smartString = SmartString(LocalisedString(Map(LangADT.En -> english)), Nil)
    smartString.rawDefaultValue shouldBe english
  }

  it should "return English if Welsh is defined and blank" in {
    implicit val l: LangADT = LangADT.Cy
    val smartString = SmartString(LocalisedString(Map(LangADT.En -> english, LangADT.Cy -> "")), Nil)
    smartString.rawDefaultValue shouldBe english
  }

  it should "return Welsh if Welsh is defined and not blank" in {
    implicit val l: LangADT = LangADT.Cy
    val smartString = SmartString(LocalisedString(Map(LangADT.En -> english, LangADT.Cy -> welsh)), Nil)
    smartString.rawDefaultValue shouldBe welsh
  }

  private def condition(s: String): String =
    s.flatMap { c =>
      if (c >= 32 && c <= 127 && c != '"' && c != '\\') Seq(c)
      else Seq.empty
    }.mkString
}
