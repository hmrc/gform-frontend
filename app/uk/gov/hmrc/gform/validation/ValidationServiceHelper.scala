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

import cats.implicits._
import play.api.i18n.Messages
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

object ValidationServiceHelper {

  val validationSuccess: ValidatedType[Unit] = ().valid

  def validationFailure[A](
    fieldValue: FormComponent,
    messageKey: String,
    vars: Option[List[String]]
  )(
    implicit
    l: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[A] = validationFailure(fieldValue.modelComponentId, fieldValue, messageKey, vars)

  def validationFailure[A](
    modelComponentId: ModelComponentId,
    fieldValue: FormComponent,
    messageKey: String,
    vars: Option[List[String]]
  )(
    implicit
    l: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[A] =
    Map(modelComponentId -> ComponentsValidatorHelper.errors(fieldValue, messageKey, vars)).invalid

}
