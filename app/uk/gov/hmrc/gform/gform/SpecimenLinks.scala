/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber

final case class SpecimenLinks(previous: Option[SectionNumber.Classic], next: Option[SectionNumber.Classic])

object SpecimenLinks {
  def from(sectionNumbers: List[SectionNumber.Classic], currentSectionNumber: SectionNumber.Classic): SpecimenLinks = {

    val previous: Option[SectionNumber.Classic] = sectionNumbers.filter(_ < currentSectionNumber).maxOption
    val next: Option[SectionNumber.Classic] = sectionNumbers.filter(_ > currentSectionNumber).minOption

    SpecimenLinks(previous, next)
  }
}
