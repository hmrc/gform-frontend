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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.instances.int._
import cats.syntax.eq._
import play.api.libs.json.{ Json, OFormat }
import scala.util.Try
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber

case class VisitIndex(visitsIndex: Set[Int]) extends AnyVal {
  def visit(sectionNumber: SectionNumber): VisitIndex = VisitIndex(visitsIndex + sectionNumber.value)
  def contains(index: Int): Boolean = visitsIndex.contains(index)
}

object VisitIndex {

  def updateSectionVisits(
    browserFormModel: FormModel[DataExpanded],
    mongoFormModel: FormModel[DataExpanded],
    visitsIndex: VisitIndex
  ): Set[Int] =
    visitsIndex.visitsIndex
      .map { index =>
        Try(mongoFormModel(index)).toOption.fold(-1) { mongoPageModel =>
          mongoPageModel.allFormComponents.headOption.fold(-1) { mongoPageModelFirstFc =>
            browserFormModel.pages.indexWhere { browserPageModel =>
              browserPageModel.allFormComponents.headOption.fold(false)(_.id === mongoPageModelFirstFc.id)
            }
          }
        }
      }
      .filterNot(_ === -1)

  implicit val format: OFormat[VisitIndex] = Json.format

}
