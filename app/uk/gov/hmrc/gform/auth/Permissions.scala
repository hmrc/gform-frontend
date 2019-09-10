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

import uk.gov.hmrc.gform.auth.models.OperationWithForm.{ EditForm => EditFormWith, _ }
import uk.gov.hmrc.gform.auth.models.OperationWithoutForm.{ EditForm => EditFormWithout, _ }
import uk.gov.hmrc.gform.auth.models.PermissionResult.{ NotPermitted, Permitted }
import uk.gov.hmrc.gform.auth.models.Role.{ Agent, Customer, Reviewer }
import uk.gov.hmrc.gform.auth.models.{ OperationWithForm, OperationWithoutForm, PermissionResult, Role }
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.sharedmodel.form.{ Accepted, Accepting, FormStatus, InProgress, NeedsReview, Returning, Signed, Submitted, Submitting, Summary, Validated }

object Permissions {
  def apply(operation: OperationWithoutForm, role: Role): PermissionResult = (operation, role) match {
    case (EditFormWithout, Agent | Customer) => permitted(operation, role)
    case (ShowAccessCode, Agent | Customer)  => permitted(operation, role)
    case (ViewDashboard, _)                  => permitted(operation, role)
    case _                                   => notPermitted(operation, role)
  }

  def apply(operation: OperationWithForm, role: Role, status: FormStatus): PermissionResult =
    (operation, role, status) match {
      case (DownloadSummaryPdf, _, _)                                      => permitted(operation, role, status)
      case (EditFormWith, Agent | Customer, CustomerEditableFormStatus(_)) => permitted(operation, role, status)
      case (EditFormWith, Customer | Agent, _)                             => notPermitted(operation, role, status)
      case (EditFormWith, Reviewer, Submitted)                             => notPermitted(operation, role, status)
      case (EditFormWith, Reviewer, _)                                     => permitted(operation, role, status)
      case (ReviewAccepted, Reviewer, NeedsReview)                         => permitted(operation, role, status)
      case (ReviewReturned, Reviewer, NeedsReview)                         => permitted(operation, role, status)
      case (ReviewSubmitted, Reviewer, Accepted | NeedsReview)             => permitted(operation, role, status)
      case (AcceptSummary, _, Summary | Validated)                         => permitted(operation, role, status)
      case (AcceptSummary, Reviewer, NeedsReview)                          => permitted(operation, role, status)
      case (SubmitDeclaration, Customer | Agent, Validated)                => permitted(operation, role, status)
      case (UpdateFormField, Reviewer, ReviewFormStatus(_))                => permitted(operation, role, status)
      case (ViewDeclaration, _, Validated)                                 => permitted(operation, role, status)
      case (ViewDeclaration, Reviewer, NeedsReview)                        => permitted(operation, role, status)
      case (ViewSummary, Reviewer, NeedsReview | Accepted)                 => permitted(operation, role, status)
      case (ViewSummary, _, Summary | Validated | Signed)                  => permitted(operation, role, status)
      case (ForceUpdateFormStatus, Reviewer, _)                            => permitted(operation, role, status)
      case _                                                               => notPermitted(operation, role, status)
    }

  private def permitted(operation: OperationWithForm, role: Role, status: FormStatus) = {
    Loggers.permissions.info(formatLogMessage(operation.toString, role, Some(status), "Valid"))
    Permitted
  }

  private def permitted(operation: OperationWithoutForm, role: Role) = {
    Loggers.permissions.info(formatLogMessage(operation.toString, role, None, "Valid"))
    Permitted
  }

  private def permitWithWarning(operation: OperationWithForm, role: Role, status: FormStatus) = {
    Loggers.permissions.warn(
      formatLogMessage(operation.toString, role, Some(status), "Invalid") + "Allowing anyway. This should be fixed before going to production.")
    Permitted
  }

  private def notPermitted(operation: OperationWithForm, role: Role, status: FormStatus) = {
    Loggers.permissions.warn(formatLogMessage(operation.toString, role, Some(status), "Invalid"))
    NotPermitted
  }

  private def notPermitted(operation: OperationWithoutForm, role: Role) = {
    Loggers.permissions.warn(formatLogMessage(operation.toString, role, None, "Invalid"))
    NotPermitted
  }

  private def formatLogMessage(operation: String, role: Role, status: Option[FormStatus], validity: String) =
    f"$validity%-20s $operation%-20s $role%-20s ${status.map(_.toString).getOrElse("")}%-20s"

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
