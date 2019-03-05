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

package uk.gov.hmrc.gform.sharedmodel.formtemplate
import uk.gov.hmrc.gform.Spec

class ConcreteDateSpec extends Spec {

  "ConcreteDate(2017, 4, 4).getExactParameters" should "return List(ExactDay(4), ExactMonth(4), ExactYear(2017))" in {
    ConcreteDate(2017, 4, 4).getNumericParameters shouldBe List(ExactDay(4), ExactMonth(4), ExactYear(2017))
  }

  "ConcreteDate(ExactYear(2017), AnyMonth, AnyDay).getExactParameters" should "return List(ExactYear(2017))" in {
    ConcreteDate(ExactYear(2017), AnyMonth, AnyDay).getNumericParameters shouldBe List(ExactYear(2017))
  }

  "ConcreteDate(ExactYear(2017), AnyMonth, ExactDay(14)).getExactParameters" should "return List(ExactDay(14), ExactYear(2017))" in {
    ConcreteDate(ExactYear(2017), AnyMonth, ExactDay(14)).getNumericParameters shouldBe List(
      ExactDay(14),
      ExactYear(2017))
  }

  "ConcreteDate(AnyYear, AnyMonth, AnyDay).getExactParameters" should "return List()" in {
    ConcreteDate(AnyYear, AnyMonth, AnyDay).getNumericParameters shouldBe List()
  }

  "ConcreteDate(AnyYear, AnyMonth, AnyDay).isExact" should "false" in {
    ConcreteDate(AnyYear, AnyMonth, AnyDay).isExact shouldBe false
  }

  "ConcreteDate(2017, 4, 4).isExact" should "return true" in {
    ConcreteDate(2017, 4, 4).isExact shouldBe true
  }

  "ConcreteDate(ExactYear(2017), ExactMonth(4), FirstDay).isExact" should "return List(ExactDay(14), ExactYear(2017))" in {
    ConcreteDate(ExactYear(2017), ExactMonth(4), FirstDay).isExact shouldBe true
  }

}
