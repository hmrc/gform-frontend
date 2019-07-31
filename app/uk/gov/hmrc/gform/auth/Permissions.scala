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

package uk.gov.hmrc.gform.auth

import play.api.Logger
import uk.gov.hmrc.gform.auth.models.OperationWithForm.{ EditForm => EditFormWith, _ }
import uk.gov.hmrc.gform.auth.models.OperationWithoutForm.{ EditForm => EditFormWithout, _ }
import uk.gov.hmrc.gform.auth.models.PermissionResult.{ NotPermitted, Permitted }
import uk.gov.hmrc.gform.auth.models.Role.{ Agent, Customer, Reviewer }
import uk.gov.hmrc.gform.auth.models.{ OperationWithForm, OperationWithoutForm, PermissionResult, Role }
import uk.gov.hmrc.gform.sharedmodel.form.{ Accepted, Accepting, FormStatus, InProgress, NeedsReview, Returning, Signed, Submitted, Submitting, Summary, Validated }

object Permissions {
  def apply(operation: OperationWithoutForm, role: Role): PermissionResult = (operation, role) match {
    case (EditFormWithout, Agent | Customer) => Permitted
    case (ShowAccessCode, Agent | Customer)  => Permitted
    case (ViewDashboard, _)                  => Permitted
    case _                                   => notPermitted(operation, role)
  }

  def apply(operation: OperationWithForm, role: Role, status: FormStatus): PermissionResult =
    (operation, role, status) match {
      case (DownloadSummaryPdf, _, _)                                      => Permitted
      case (EditFormWith, Agent | Customer, CustomerEditableFormStatus(_)) => Permitted
      case (EditFormWith, Customer | Agent, _)                             => permitWithWarning(operation, role, status)
      case (EditFormWith, Reviewer, Submitted)                             => notPermitted(operation, role, status)
      case (EditFormWith, Reviewer, _)                                     => Permitted
      case (ReviewAccepted, Reviewer, NeedsReview)                         => Permitted
      case (ReviewReturned, Reviewer, NeedsReview)                         => Permitted
      case (ReviewSubmitted, Reviewer, Accepted)                           => Permitted
      case (AcceptSummary, _, Summary | Validated)                         => Permitted
      case (SubmitDeclaration, Customer | Agent, Validated)                => Permitted
      case (UpdateFormField, Reviewer, ReviewFormStatus(_))                => Permitted
      case (ViewDeclaration, _, Validated)                                 => Permitted
      case (ViewSummary, Reviewer, NeedsReview)                            => Permitted
      case (ViewSummary, _, Summary | Validated | Signed)                  => Permitted
      case _                                                               => permitWithWarning(operation, role, status)
    }

  private def permitWithWarning(operation: OperationWithForm, role: Role, status: FormStatus) = {
    Logger.warn(
      s"Allowing invalid operation $operation on form with status $status by a user with role $role. This should be fixed before going to production.")
    Permitted
  }

  private def notPermitted(operation: OperationWithForm, role: Role, status: FormStatus) = {
    Logger.warn(s"Invalid operation $operation attempted on form with status $status by a user with role $role")
    NotPermitted
  }

  private def notPermitted(operation: OperationWithoutForm, role: Role) = {
    Logger.warn(s"Invalid operation $operation attempted by a user with role $role")
    NotPermitted
  }

  object ReviewFormStatus {
    def unapply(status: FormStatus): Option[FormStatus] = status match {
      case NeedsReview | Accepting | Returning | Accepting | Submitting => Some(status)
      case _                                                            => None
    }
  }

  object CustomerEditableFormStatus {
    def unapply(status: FormStatus): Option[FormStatus] = status match {
      case InProgress | Summary | Validated | Summary => Some(status)
      case _                                          => None
    }
  }
}
