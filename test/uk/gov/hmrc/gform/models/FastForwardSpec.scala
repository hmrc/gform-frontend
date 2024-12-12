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

package uk.gov.hmrc.gform.models

import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkAddToListSection, mkFormComponent, page }
import uk.gov.hmrc.gform.models.FastForward.StopAt
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SourceOrigin }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, Equals, FormComponentId, FormCtx, IncludeIf, SectionNumber, TemplateSectionIndex, Value }

class FastForwardSpec extends AnyFreeSpecLike with FormModelSupport with VariadicFormDataSupport with Matchers {

  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  "StopAt.next" - {
    "should get the StopAt(number) for the next visible section" - {
      val table = Table(
        ("description", "sections", "data", "currentStopAt", "expectedStopAt"),
        (
          "next section is visible",
          List(
            mkAddToListSection(
              page(List(mkFormComponent("fc1", Value))),
              page(List(mkFormComponent("fc2", Value)))
            )
          ),
          List("1_fc1" -> "a"),
          SectionNumber.Classic.AddToListPage.Page(TemplateSectionIndex(0), 1, 0),
          SectionNumber.Classic.AddToListPage.Page(TemplateSectionIndex(0), 1, 1)
        ),
        (
          "next section is visible (includeIf)",
          List(
            mkAddToListSection(
              page(List(mkFormComponent("fc1", Value))),
              page(
                List(mkFormComponent("fc2", Value)),
                Some(IncludeIf(Equals(FormCtx(FormComponentId("fc1")), Constant("a"))))
              )
            )
          ),
          List("1_fc1" -> "a"),
          SectionNumber.Classic.AddToListPage.Page(TemplateSectionIndex(0), 1, 0),
          SectionNumber.Classic.AddToListPage.Page(TemplateSectionIndex(0), 1, 1)
        ),
        (
          "next section is hidden (includeIf)",
          List(
            mkAddToListSection(
              page(List(mkFormComponent("fc1", Value))),
              page(
                List(mkFormComponent("fc2", Value)),
                Some(IncludeIf(Equals(FormCtx(FormComponentId("fc1")), Constant("b"))))
              )
            )
          ),
          List("1_fc1" -> "a"),
          SectionNumber.Classic.AddToListPage.Page(TemplateSectionIndex(0), 1, 0),
          SectionNumber.Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(0), 1)
        )
      )

      forAll(table) { (description, sections, data, stopAt, expectedStopAt) =>
        description in {
          val fmb = mkFormModelFromSections(sections)
          val variadicData = variadicFormData[SourceOrigin.OutOfDate](data: _*)
          val fmvo = fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](variadicData, None)
          StopAt(stopAt).next(fmvo.formModel, stopAt) shouldBe StopAt(expectedStopAt)
        }
      }
    }
  }
}
