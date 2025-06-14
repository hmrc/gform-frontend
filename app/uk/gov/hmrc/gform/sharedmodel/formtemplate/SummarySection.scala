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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.models.Basic
import uk.gov.hmrc.gform.sharedmodel.SmartString

case class SummarySection(
  title: SmartString,
  caption: Option[SmartString],
  header: SmartString,
  footer: SmartString,
  continueLabel: Option[SmartString],
  fields: Option[NonEmptyList[FormComponent]],
  displayWidth: LayoutDisplayWidth.LayoutDisplayWidth = LayoutDisplayWidth.M,
  keyDisplayWidth: KeyDisplayWidth.KeyDisplayWidth = KeyDisplayWidth.S,
  includeIf: Option[IncludeIf],
  pdf: Option[PdfCxt],
  excludeFromPdf: Option[List[FormComponentId]],
  hideDefaultRows: Option[Boolean]
) {
  def toPage: Page[Basic] =
    Page(
      title = title,
      id = None,
      noPIITitle = None,
      description = None,
      shortName = None,
      caption = None,
      includeIf = None,
      fields = fields.fold(List.empty[FormComponent])(_.toList),
      continueLabel = None,
      continueIf = None,
      instruction = None,
      presentationHint = None,
      dataRetrieve = None,
      confirmation = None,
      redirects = None,
      hideSaveAndComeBackButton = None,
      removeItemIf = None,
      displayWidth = None,
      notRequiredIf = None
    )

  def excludeFieldsFromPDF: SummarySection = copy(
    fields = fields.map(_.filterNot(fc => excludeFromPdf.getOrElse(Nil).contains(fc.id))).flatMap(NonEmptyList.fromList)
  )
}

object SummarySection extends JsonUtils {
  implicit val format: OFormat[SummarySection] = Json.format[SummarySection]
}
