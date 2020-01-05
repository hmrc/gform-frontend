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

package uk.gov.hmrc.gform.gform

import cats.Eq
import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent

trait SummaryPagePurpose extends Product with Serializable {
  def forUser = this === SummaryPagePurpose.ForUser
  def fileName(maybeFileName: Option[String], fc: FormComponent): Option[String] =
    maybeFileName
      .map { fileName =>
        this match {
          case SummaryPagePurpose.ForUser => fileName.replace(fc.id + "_", "")
          case SummaryPagePurpose.ForDms  => fileName
        }
      }

}

object SummaryPagePurpose {
  case object ForUser extends SummaryPagePurpose
  case object ForDms extends SummaryPagePurpose

  implicit val equal: Eq[SummaryPagePurpose] = Eq.fromUniversalEquals
}
