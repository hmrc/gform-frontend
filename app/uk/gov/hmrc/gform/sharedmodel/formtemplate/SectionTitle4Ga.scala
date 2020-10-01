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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json.Format
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }

// For Google Analytics only. Sole purpose of this value is to be present in URLs.
case class SectionTitle4Ga(value: String) extends AnyVal

object SectionTitle4Ga {

  def sectionTitle4GaFactory(title: SmartString, sectionNumber: SectionNumber) = {
    val sectionTitle = title.localised.value(LangADT.En)
    val finalSectionTitle =
      if (sectionTitle.isEmpty) {
        "section" + sectionNumber.value
      } else {
        sectionTitle
      }
    SectionTitle4Ga(finalSectionTitle.replace(' ', '-').replaceAll("[^-A-Za-z0-9]", ""))
  }

  implicit val format: Format[SectionTitle4Ga] =
    uk.gov.hmrc.gform.models.ValueClassFormat.format(SectionTitle4Ga.apply)(_.value)

}
