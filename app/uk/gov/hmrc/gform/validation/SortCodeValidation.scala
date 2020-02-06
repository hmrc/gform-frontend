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

package uk.gov.hmrc.gform.validation
import cats.Monoid
import cats.data.Validated
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.ids.IndexedComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, UkSortCode }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.{ validationFailure, validationSuccess }

object SortCodeValidation {

  def validateSortCode[D <: DataOrigin](
    fieldValue: FormComponent,
    sC: UkSortCode,
    mandatory: Boolean
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(
    implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] =
    Monoid[ValidatedType[Unit]].combineAll {
      val indexedComponentId
        : IndexedComponentId = fieldValue.modelComponentId.indexedComponentId // TODO JoVl, this is weird, let's use MultiValueId istead here to create `fieldIdList`
      val fieldIdList = UkSortCode
        .fields(indexedComponentId)
        .toList
      fieldIdList.map { fieldId =>
        val sortCode: Option[String] =
          formModelVisibilityOptics.data.one(fieldId).filterNot(_.isEmpty)
        (sortCode, mandatory) match {
          case (None, true) =>
            validationFailure(fieldValue, "generic.error.sortcode", None)
          case (None, false)    => validationSuccess
          case (Some(value), _) => checkLength(fieldValue, value, 2)
        }
      }
    }

  def checkLength(
    fieldValue: FormComponent,
    value: String,
    desiredLength: Int
  )(
    implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] = {
    val WholeShape = s"[0-9]{$desiredLength}".r
    val x = "y"
    val FractionalShape = "([+-]?)(\\d*)[.](\\d+)".r
    value match {
      case FractionalShape(_, _, _) =>
        validationFailure(fieldValue, "generic.error.wholeNumber", None)
      case WholeShape() => validationSuccess
      case _ =>
        validationFailure(fieldValue, "generic.error.sortcode", None)
    }
  }
}
