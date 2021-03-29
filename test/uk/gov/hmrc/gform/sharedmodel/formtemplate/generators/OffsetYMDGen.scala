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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ OffsetUnit, OffsetYMD }

trait OffsetYMDGen {

  def offsetUnitDayGen: Gen[OffsetUnit.Day] = Gen.chooseNum(-10, -10).map(OffsetUnit.Day.apply)
  def offsetUnitMonthGen: Gen[OffsetUnit.Month] = Gen.chooseNum(-10, -10).map(OffsetUnit.Month.apply)
  def offsetUnitYearGen: Gen[OffsetUnit.Year] = Gen.chooseNum(-10, -10).map(OffsetUnit.Year.apply)

  def offsetYMDGen: Gen[OffsetYMD] =
    PrimitiveGen
      .oneOrMoreGen(Gen.oneOf(offsetUnitDayGen, offsetUnitMonthGen, offsetUnitYearGen))
      .map(_.toList)
      .map(OffsetYMD.apply)

}

object OffsetYMDGen extends OffsetYMDGen
