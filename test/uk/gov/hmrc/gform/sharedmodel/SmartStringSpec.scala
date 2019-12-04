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

package uk.gov.hmrc.gform.sharedmodel

import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ ExprGen, PrimitiveGen }

class SmartStringSpec extends Spec {
  "JSON" should "read a simple String" in {
    forAll(Gen.asciiStr) { s =>
      val c = condition(s)
      verifyRead(
        SmartString(LocalisedString(Map(LangADT.En -> c)), Nil),
        s""""$c""""
      )
    }
  }

  it should "read a LocalisedString map without interpolations" in {
    forAll(Gen.asciiStr, Gen.asciiStr) { (english, welsh) =>
      val cEnglish = condition(english)
      val cWelsh = condition(welsh)

      verifyRead(
        SmartString(LocalisedString(Map(LangADT.En -> cEnglish, LangADT.Cy -> cWelsh)), Nil),
        s"""|{
            |  "en": "$cEnglish",
            |  "cy": "$cWelsh"
            |}""".stripMargin
      )
    }
  }

  it should "read a LocalisedString map with interpolations" in {
    forAll(Gen.asciiStr, Gen.asciiStr, PrimitiveGen.zeroOrMoreGen(ExprGen.exprGen())) { (english, welsh, exprs) =>
      val cEnglish = condition(english)
      val cWelsh = condition(welsh)
      val interpolations = exprs.map(Expr.format.writes).map(_.toString).mkString(", ")

      verifyRead(
        SmartString(LocalisedString(Map(LangADT.En -> cEnglish, LangADT.Cy -> cWelsh)), exprs),
        s"""|{
            |  "en": "$cEnglish",
            |  "cy": "$cWelsh",
            |  "interpolations": [ $interpolations ]
            |}""".stripMargin
      )
    }
  }

  it should "round trip" in {
    forAll(Gen.asciiStr, Gen.asciiStr, PrimitiveGen.zeroOrMoreGen(ExprGen.exprGen())) { (english, welsh, exprs) =>
      val cEnglish = condition(english)
      val cWelsh = condition(welsh)

      val smartString = SmartString(LocalisedString(Map(LangADT.En -> cEnglish, LangADT.Cy -> cWelsh)), exprs)

      verifyRoundTrip(smartString)

    }
  }

  private def condition(s: String): String =
    s.flatMap { c =>
      if (c >= 32 && c <= 127 && c != '"' && c != '\\') Seq(c)
      else Seq.empty
    }.mkString
}
