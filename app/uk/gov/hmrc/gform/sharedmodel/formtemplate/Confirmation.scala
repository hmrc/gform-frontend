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

import play.api.libs.json.{ Json, OFormat }
import cats.data.NonEmptyList

import com.softwaremill.quicklens._

final case class Confirmation(
  question: FormComponent,
  redirects: NonEmptyList[ConfirmationRedirect]
) {
  def updateExpr(f: Expr => Expr): Confirmation = this
    .modify(_.question)
    .using(_.updateExpr(f))
    .copy(redirects = redirects.map(_.modify(_.`if`.booleanExpr).using(_.updateExpr(f))))
}

object Confirmation {
  import JsonUtils._
  implicit val confirmationFormat: OFormat[Confirmation] = Json.format[Confirmation]
}
