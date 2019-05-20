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

package uk.gov.hmrc.gform.validation
import cats.Monoid
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, UkSortCode }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.{ validationFailure, validationSuccess }

object SortCodeValidation {

  def validateSortCode(fieldValue: FormComponent, sC: UkSortCode, mandatory: Boolean)(
    data: FormDataRecalculated)(implicit messages: Messages, l: LangADT) =
    Monoid[ValidatedType[Unit]].combineAll(
      UkSortCode
        .fields(fieldValue.id)
        .toList
        .map { fieldId =>
          val sortCode: Seq[String] = data.data.get(fieldId).toList.flatten
          (sortCode.filterNot(_.isEmpty), mandatory) match {
            case (Nil, true) =>
              validationFailure(fieldValue, messages("generic.error.sortcode"))
            case (Nil, false)      => validationSuccess
            case (value :: Nil, _) => checkLength(fieldValue, value, 2)
          }
        }
    )

  def checkLength(fieldValue: FormComponent, value: String, desiredLength: Int)(
    implicit messages: Messages,
    l: LangADT) = {
    val WholeShape = s"[0-9]{$desiredLength}".r
    val x = "y"
    val FractionalShape = "([+-]?)(\\d*)[.](\\d+)".r
    value match {
      case FractionalShape(_, _, _) =>
        validationFailure(fieldValue, messages("generic.error.wholeNumber"))
      case WholeShape() => validationSuccess
      case _ =>
        validationFailure(fieldValue, messages("generic.error.sortcode"))
    }
  }
}
