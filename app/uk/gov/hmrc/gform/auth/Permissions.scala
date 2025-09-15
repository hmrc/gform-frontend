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

package uk.gov.hmrc.gform.auth

import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.auth.models.OperationWithForm.{ EditForm => EditFormWith, _ }
import uk.gov.hmrc.gform.auth.models.OperationWithoutForm.{ EditForm => EditFormWithout, _ }
import uk.gov.hmrc.gform.auth.models.PermissionResult.{ FormSubmitted, NotPermitted, Permitted }
import uk.gov.hmrc.gform.auth.models.Role.{ Agent, Customer, Reviewer }
import uk.gov.hmrc.gform.auth.models.{ OperationWithForm, OperationWithoutForm, PermissionResult, Role }
import uk.gov.hmrc.gform.sharedmodel.form._

object Permissions {

  private val logger = LoggerFactory.getLogger(getClass)

  def apply(operation: OperationWithoutForm, role: Role): PermissionResult =
    evaluateOperationWithoutForm(operation, role)(logger)

  def apply(operation: OperationWithForm, role: Role, status: FormStatus): PermissionResult =
    evaluateOperationWithForm(operation, role, status)(logger)

  private[auth] def evaluateOperationWithoutForm(operation: OperationWithoutForm, role: Role)(implicit
    logger: Logger
  ): PermissionResult =
    (operation, role) match {
      case (Lookup, _)                         => valid(operation, role)
      case (EditFormWithout, Agent | Customer) => valid(operation, role)
      case (ShowAccessCode, Agent | Customer)  => valid(operation, role)
      case (ViewDashboard, _)                  => valid(operation, role)
      case _                                   => mostLikelyInvalid(operation, role)
    }

  private[auth] def evaluateOperationWithForm(operation: OperationWithForm, role: Role, status: FormStatus)(implicit
    logger: Logger
  ): PermissionResult =
    (operation, role, status) match {
      case (DownloadSummaryPdf, _, _)                                      => valid(operation, role, status)
      case (ViewAcknowledgement, _, Submitted | NeedsReview)               => valid(operation, role, status)
      case (ViewSaveAcknowledgement, _, _)                                 => valid(operation, role, status)
      case (ForceUpdateFormStatus, _, Submitted)                           => valid(operation, role, status)
      case (EditFormAcknowledgement, _, Submitted)                         => valid(operation, role, status)
      case (ForceReturnToCYA, _, _)                                        => valid(operation, role, status)
      case (NewFormLink, _, _)                                             => valid(operation, role, status)
      case (SwitchLanguage, _, _)                                          => valid(operation, role, status)
      case (_, _, Submitted)                                               => formSubmitted(operation, role, status)
      case (EditFormWith, Agent | Customer, CustomerEditableFormStatus(_)) => valid(operation, role, status)
      case (EditFormWith, Customer | Agent, _)                             => mostLikelyInvalid(operation, role, status)
      case (EditFormWith, Reviewer, StableReviewFormStatus(_))             => valid(operation, role, status)
      case (EditFormWith, Reviewer, _)                                     => valid(operation, role, status)
      case (ReviewAccepted, Reviewer, NeedsReview)                         => valid(operation, role, status)
      case (ReviewAccepted, Reviewer, Accepting)                           => validTransient(operation, role, status)
      case (ReviewReturned, Reviewer, NeedsReview)                         => valid(operation, role, status)
      case (ReviewReturned, Reviewer, Returning)                           => validTransient(operation, role, status)
      case (ReviewSubmitted, Reviewer, Accepted | NeedsReview)             => valid(operation, role, status)
      case (ReviewSubmitted, Reviewer, Submitting)                         => validTransient(operation, role, status)
      case (AcceptSummary, _, InProgress | Summary | Validated | Signed)   => valid(operation, role, status)
      case (AcceptSummary, Reviewer, NeedsReview)                          => valid(operation, role, status)
      case (SubmitDeclaration, Customer | Agent, Validated | Signed)       => valid(operation, role, status)
      case (UpdateFormField, Reviewer, StableReviewFormStatus(_))          => valid(operation, role, status)
      case (UpdateFormField, Reviewer, TransientReviewFormStatus(_))       => validTransient(operation, role, status)
      case (ViewDeclaration, _, InProgress)                                => definitelyInvalid(operation, role, status)
      case (ViewDeclaration, _, Validated)                                 => valid(operation, role, status)
      case (ViewDeclaration, Reviewer, NeedsReview)                        => valid(operation, role, status)
      case (ViewSummary, Reviewer, NeedsReview | Accepted)                 => valid(operation, role, status)
      case (ViewSummary, Reviewer, TransientReviewFormStatus(_))           => validTransient(operation, role, status)
      case (ViewSummary, _, Summary | Validated | Signed | InProgress)     => valid(operation, role, status)
      case (ForceUpdateFormStatus, Reviewer, _)                            => valid(operation, role, status)
      case (ViewPrintSection, _, Validated)                                => valid(operation, role, status)
      case (DownloadPrintSectionPdf, _, Validated)                         => valid(operation, role, status)
      case (AuditSessionEnd, _, _)                                         => valid(operation, role, status)
      case (ViewImageByInternalLink, _, _)                                 => valid(operation, role, status)
      case _                                                               => mostLikelyInvalid(operation, role, status)
    }

  private def valid(operation: OperationWithForm, role: Role, status: FormStatus)(implicit logger: Logger) = {
    logger.info(formatLogMessage(operation.toString, role, Some(status), "Valid"))
    Permitted
  }

  private def valid(operation: OperationWithoutForm, role: Role)(implicit logger: Logger) = {
    logger.info(formatLogMessage(operation.toString, role, None, "Valid"))
    Permitted
  }

  private def validTransient(operation: OperationWithForm, role: Role, status: FormStatus)(implicit logger: Logger) = {
    logger.info(
      formatLogMessage(
        operation.toString,
        role,
        Some(status),
        "Valid",
        "Form stuck in a transient state. It is worth investigating why the form got stuck."
      )
    )
    Permitted
  }

  private def mostLikelyInvalid(operation: OperationWithForm, role: Role, status: FormStatus)(implicit
    logger: Logger
  ) = {
    logger.error(
      formatLogMessage(
        operation.toString,
        role,
        Some(status),
        "Invalid",
        "This combination is currently blocked. Verify that it should be."
      )
    )
    NotPermitted
  }

  private def mostLikelyInvalid(operation: OperationWithoutForm, role: Role)(implicit logger: Logger) = {
    logger.error(
      formatLogMessage(
        operation.toString,
        role,
        None,
        "Invalid",
        "This combination is currently blocked. Verify that it should be."
      )
    )
    NotPermitted
  }

  private def definitelyInvalid(operation: OperationWithForm, role: Role, status: FormStatus)(implicit
    logger: Logger
  ) = {
    logger.info(
      formatLogMessage(
        operation.toString,
        role,
        Some(status),
        "Invalid",
        "This combination has been examined and is correctly blocked."
      )
    )
    NotPermitted
  }

  private def formSubmitted(operation: OperationWithForm, role: Role, status: FormStatus)(implicit logger: Logger) = {
    logger.info(
      formatLogMessage(
        operation.toString,
        role,
        Some(status),
        "FormSubmitted",
        "This combination has been blocked because the form has been submitted."
      )
    )
    FormSubmitted
  }

  private def formatLogMessage(
    operation: String,
    role: Role,
    status: Option[FormStatus],
    validity: String,
    comment: String = ""
  ) =
    f"$validity%-20s $operation%-20s $role%-20s ${status.map(_.toString).getOrElse("")}%-20s$comment"

  object StableReviewFormStatus {
    def unapply(status: FormStatus): Option[FormStatus] = status match {
      case NeedsReview => Some(status)
      case _           => None
    }
  }

  object TransientReviewFormStatus {
    def unapply(status: FormStatus): Option[FormStatus] = status match {
      case Accepting | Returning | Submitting => Some(status)
      case _                                  => None
    }
  }

  object CustomerEditableFormStatus {
    def unapply(status: FormStatus): Option[FormStatus] = status match {
      case InProgress | Summary | Validated | Signed => Some(status)
      case _                                         => None
    }
  }
}
