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

package uk.gov.hmrc.gform.auth.models

sealed trait CheckEnrolmentsResult extends Product with Serializable

object CheckEnrolmentsResult {
  case object Successful extends CheckEnrolmentsResult
  case object Failed extends CheckEnrolmentsResult
  case object InvalidIdentifiers extends CheckEnrolmentsResult
  case object InvalidCredentials extends CheckEnrolmentsResult
  case object Conflict extends CheckEnrolmentsResult
  case object InsufficientEnrolments extends CheckEnrolmentsResult
}
