/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.email.EmailConfirmationCode
import uk.gov.hmrc.gform.sharedmodel.form.EmailAndCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen

import org.typelevel.ci._
trait EmailVerificationCodeGen {
  def emailVerificationCodeGen: Gen[EmailAndCode] =
    for {
      email <- PrimitiveGen.nonEmptyAsciiPrintableString
      code  <- PrimitiveGen.nonEmptyAsciiPrintableString
    } yield EmailAndCode(ci"$email", EmailConfirmationCode(ci"$code"))
}

object EmailVerificationCodeGen extends EmailVerificationCodeGen
