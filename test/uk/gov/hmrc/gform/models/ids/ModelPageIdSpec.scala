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

package uk.gov.hmrc.gform.models.ids

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.gov.hmrc.gform.sharedmodel.formtemplate.PageId

class ModelPageIdSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  "fromPageId" should "build ModelPageId from the given PageId" in {
    val table =
      Table(("pageId", "expected"), ("page1", ModelPageId.Pure("page1")), ("1_page1", ModelPageId.Indexed("page1", 1)))
    forAll(table) { (pageId, modelPageId) =>
      ModelPageId.fromPageId(PageId(pageId)) shouldBe modelPageId
    }
  }

  "toIndexed" should "return the indexed PageId for the given index" in {
    val table = Table(
      ("modelPageId", "index", "expected"),
      (ModelPageId.Pure("page1"), 1, ModelPageId.Indexed("page1", 1)),
      (ModelPageId.Indexed("page1", 1), 2, ModelPageId.Indexed("page1", 1))
    )
    forAll(table) { (modelPageId, index, expected) =>
      modelPageId.toIndexed(index) shouldBe expected
    }
  }

  "baseId" should "return the based id for the given ModelPageId" in {
    val table = Table(
      ("modelPageId", "expected"),
      (ModelPageId.Pure("page1"), "page1"),
      (ModelPageId.Indexed("page1", 1), "page1")
    )
    forAll(table) { (modelPageId, expected) =>
      modelPageId.baseId shouldBe expected
    }
  }
}
