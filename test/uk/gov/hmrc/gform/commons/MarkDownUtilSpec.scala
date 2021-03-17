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

package uk.gov.hmrc.gform.commons

import org.scalatest.prop.TableDrivenPropertyChecks
import play.twirl.api.Html
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.sharedmodel.LangADT

class MarkDownUtilSpec extends Spec with TableDrivenPropertyChecks {
  implicit val l = LangADT.En
  "addTargetToLinks" should "add target link" in {

    val dataAndExpectations = Table(
      ("input", "output"),
      ("", ""),
      (
        "link [making a claim](/submissions/new-form/)",
        """<p>link <a href="/submissions/new-form/" class="govuk-link">making a claim</a></p>"""
      ),
      (
        "link [making a claim](/some/relative/path)",
        """<p>link <a href="/some/relative/path" target="_blank" class="govuk-link">making a claim</a></p>"""
      ),
      (
        "link [making a claim](https://www.gov.uk/government)",
        """<p>link <a href="https://www.gov.uk/government" target="_blank" class="govuk-link">making a claim</a></p>"""
      ),
      (
        "link [print pdf](/submissions/acknowledgement/pdf/test-form)",
        """<p>link <a href="/submissions/acknowledgement/pdf/test-form" target="_blank" class="govuk-link print-link">print pdf</a></p>"""
      )
    )

    forAll(dataAndExpectations) { (input, expected) =>
      val res = MarkDownUtil.markDownParser(toSmartString(input))

      res should be(Html(expected))

    }
  }

  "markDownParser" should "convert markdown text to HTML, handling special characters" in {
    val data = Table(
      ("input", "expected"),
      ("""## Heading (h2) #""", """<h2>Heading (h2)</h2>"""),
      ("""## \#Heading (h2) #""", """<h2>#Heading (h2)</h2>"""),
      (
        """1. Total is 1\.00 and 2\.00""",
        """<ol>
          | <li>Total is 1.00 and 2.00</li>
          |</ol>""".stripMargin
      )
    )
    forAll(data) { (input, expected) =>
      MarkDownUtil.markDownParser(toSmartString(input)) shouldBe Html(expected)
    }
  }

  "escapeMarkdown" should "escape markdown special characters with backslash" in {
    val data = Table(
      ("input", "expected"),
      ("""abc\def""", """abc\\def"""),
      ("abc/def", """abc\/def"""),
      ("abc`def", """abc\`def"""),
      ("abc*def", """abc\*def"""),
      ("abc_def", """abc\_def"""),
      ("abc{def", """abc\{def"""),
      ("abc}def", """abc\}def"""),
      ("abc[def", """abc\[def"""),
      ("abc]def", """abc\]def"""),
      ("abc(def", """abc\(def"""),
      ("abc)def", """abc\)def"""),
      ("abc#def", """abc\#def"""),
      ("abc+def", """abc\+def"""),
      ("abc-def", """abc\-def"""),
      ("abc.def", """abc\.def"""),
      ("abc!def", """abc\!def""")
    )

    forAll(data) { (input, expected) =>
      MarkDownUtil.escapeMarkdown(input) shouldBe expected
    }
  }

  "unescapeMarkdownHtml" should "unescape markdown special characters escaped with backslash from input" in {
    val data = Table(
      ("expected", "input"),
      ("""<div>abc\def</div>""", """<div>abc\\def</div>"""),
      ("<div>abc/def</div>", """<div>abc\/def</div>"""),
      ("<div>abc`def</div>", """<div>abc\`def</div>"""),
      ("<div>abc*def</div>", """<div>abc\*def</div>"""),
      ("<div>abc_def</div>", """<div>abc\_def</div>"""),
      ("<div>abc{def</div>", """<div>abc\{def</div>"""),
      ("<div>abc}def</div>", """<div>abc\}def</div>"""),
      ("<div>abc[def</div>", """<div>abc\[def</div>"""),
      ("<div>abc]def</div>", """<div>abc\]def</div>"""),
      ("<div>abc(def</div>", """<div>abc\(def</div>"""),
      ("<div>abc)def</div>", """<div>abc\)def</div>"""),
      ("<div>abc#def</div>", """<div>abc\#def</div>"""),
      ("<div>abc+def</div>", """<div>abc\+def</div>"""),
      ("<div>abc-def</div>", """<div>abc\-def</div>"""),
      ("<div>abc.def</div>", """<div>abc\.def</div>"""),
      ("<div>abc!def</div>", """<div>abc\!def</div>""")
    )

    forAll(data) { (expected, input) =>
      MarkDownUtil.unescapeMarkdownHtml(input) shouldBe expected
    }
  }
}
