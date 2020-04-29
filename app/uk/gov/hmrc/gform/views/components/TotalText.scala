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

package uk.gov.hmrc.gform.views.components

import cats.syntax.option._
import uk.gov.hmrc.gform.commons.{ BigDecimalUtil, NumberFormatUtil }
import uk.gov.hmrc.gform.ops.FormComponentOps
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.hint.Hint

class TotalText(
  formComponent: FormComponent,
  val label: Text,
  maybeUnit: Option[String],
  maybeHint: Option[Hint],
  maybeErrorMessage: Option[ErrorMessage],
  maybeCurrentValue: Option[String]
) {

  val id: String = formComponent.id.value
  private val hintId = s"$id-hint".some
  val hint: Option[Hint] = maybeHint.map(_.copy(id = hintId))

  private val errorId = s"$id-error".some
  val errorMessage: Option[ErrorMessage] = maybeErrorMessage.map(_.copy(id = errorId))

  val currentValue: String = maybeCurrentValue.getOrElse("")

  val formatedValue: String =
    if (formComponent.isSterling)
      NumberFormatUtil.currencyFormat.format(BigDecimalUtil.toBigDecimalDefault(currentValue))
    else currentValue

  val unit: String = maybeUnit.getOrElse("")

}
