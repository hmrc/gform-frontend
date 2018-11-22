/*
 * Copyright 2018 HM Revenue & Customs
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

import play.api.mvc.Request
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import play.api.mvc.AnyContent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AuthConfig

sealed trait CheckEnrolmentsResult extends Product with Serializable {
  def toAuthResult: AuthResult = this match {
    case EnrolmentSuccessful(retrievals) => AuthSuccessful(retrievals)
    case EnrolmentFailed                 => AuthForbidden("Enrolment unsuccessful")
  }
}

object CheckEnrolmentsResult {
  implicit object hasAuthResult extends HasAuthResult[CheckEnrolmentsResult] {

    def getAuthResult(cer: CheckEnrolmentsResult): AuthResult = cer.toAuthResult

    def errorHandler(
      request: Request[AnyContent],
      authConfig: AuthConfig,
      appConfig: AppConfig,
      formTemplate: FormTemplate): PartialFunction[Throwable, CheckEnrolmentsResult] = {
      case _ => EnrolmentFailed
    }
  }
}

case class EnrolmentSuccessful(retrievals: MaterialisedRetrievals) extends CheckEnrolmentsResult
case object EnrolmentFailed extends CheckEnrolmentsResult
