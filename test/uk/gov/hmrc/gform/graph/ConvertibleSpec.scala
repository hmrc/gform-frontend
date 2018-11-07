/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.graph

import cats.instances.option._
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import FormTemplateBuilder._

class ConvertibleSpec extends FlatSpec with Matchers {

  "Convertible" should "convert to String with respect to maxDecimalDigits" in {

    val formComponentIds = Table(
      ("computable", "scale", "output"),
      (NonComputable, 2, ""),
      (Computed(1.23456), 2, "1.23"),
      (Computed(1.239), 2, "1.23"),
      (Computed(1.2), 2, "1.2"),
      (Computed(12), 2, "12")
    )

    forAll(formComponentIds) { (computable, scale, expectedOutput) ⇒
      val converted = Convertible.round(Converted[Option](Some(computable)), scale, mkFormTemplate(List.empty))
      converted shouldBe Some(expectedOutput)
    }
  }
}
