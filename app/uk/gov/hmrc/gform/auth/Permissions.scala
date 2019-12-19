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

import org.slf4j.helpers.NOPLogger
import play.api.Logger
import uk.gov.hmrc.gform.auth.models.OperationWithForm.{ EditForm => EditFormWith, _ }
import uk.gov.hmrc.gform.auth.models.OperationWithoutForm.{ EditForm => EditFormWithout, _ }
import uk.gov.hmrc.gform.auth.models.PermissionResult.{ FormSubmitted, NotPermitted, Permitted }
import uk.gov.hmrc.gform.auth.models.Role.{ Agent, Customer, Reviewer }
import uk.gov.hmrc.gform.auth.models.{ OperationWithForm, OperationWithoutForm, PermissionResult, Role }
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.sharedmodel.form.{ Accepted, Accepting, FormStatus, InProgress, NeedsReview, Returning, Signed, Submitted, Submitting, Summary, Validated }

import scala.io.Source

object Permissions {
  def apply(operation: OperationWithoutForm, role: Role): PermissionResult =
    evaluateOperationWithoutForm(operation, role)(Loggers.permissions)

  def apply(operation: OperationWithForm, role: Role, status: FormStatus): PermissionResult =
    evaluateOperationWithForm(operation, role, status)(Loggers.permissions)

  private[auth] def evaluateOperationWithoutForm(operation: OperationWithoutForm, role: Role)(
    implicit logger: Logger): PermissionResult =
    (operation, role) match {
      case (Lookup, _)                         => valid(operation, role)
      case (EditFormWithout, Agent | Customer) => valid(operation, role)
      case (ShowAccessCode, Agent | Customer)  => valid(operation, role)
      case (ViewDashboard, _)                  => valid(operation, role)
      case _                                   => mostLikelyInvalid(operation, role)
    }

  private[auth] def evaluateOperationWithForm(operation: OperationWithForm, role: Role, status: FormStatus)(
    implicit logger: Logger): PermissionResult =
    (operation, role, status) match {
      case (DownloadSummaryPdf, _, _)                                      => valid(operation, role, status)
      case (ViewAcknowledgement, _, Submitted | NeedsReview)               => valid(operation, role, status)
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
        "Form stuck in a transient state. It is worth investigating why the form got stuck."))
    Permitted
  }

  private def mostLikelyInvalid(operation: OperationWithForm, role: Role, status: FormStatus)(
    implicit logger: Logger) = {
    logger.error(
      formatLogMessage(
        operation.toString,
        role,
        Some(status),
        "Invalid",
        "This combination is currently blocked. Verify that it should be."))
    NotPermitted
  }

  private def mostLikelyInvalid(operation: OperationWithoutForm, role: Role)(implicit logger: Logger) = {
    logger.error(
      formatLogMessage(
        operation.toString,
        role,
        None,
        "Invalid",
        "This combination is currently blocked. Verify that it should be."))
    NotPermitted
  }

  private def definitelyInvalid(operation: OperationWithForm, role: Role, status: FormStatus)(
    implicit logger: Logger) = {
    logger.info(
      formatLogMessage(
        operation.toString,
        role,
        Some(status),
        "Invalid",
        "This combination has been examined and is correctly blocked."))
    NotPermitted
  }

  private def formSubmitted(operation: OperationWithForm, role: Role, status: FormStatus)(implicit logger: Logger) = {
    logger.info(
      formatLogMessage(
        operation.toString,
        role,
        Some(status),
        "FormSubmitted",
        "This combination has been blocked because the form has been submitted."))
    FormSubmitted
  }

  private def formatLogMessage(
    operation: String,
    role: Role,
    status: Option[FormStatus],
    validity: String,
    comment: String = "") =
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

object PermissionsTable extends App {
  import cats.syntax.eq._
  import cats.instances.string._

  sealed trait Row {
    def operation: String
    def role: String
  }
  case class RowWithoutForm(operation: String, role: String) extends Row
  case class RowWithForm(operation: String, role: String, status: String) extends Row

  private val roles = Set(Role.Customer, Role.Reviewer, Role.Agent)

  private val logged: Map[Row, Boolean] = readLoggedOperations
  println(logged)

  private val enumeratedRows: Seq[(Row, PermissionResult)] = sort(enumerateRows)
  println(enumeratedRows)

  showTable("Tested", row => logged.contains(row))
  println
  showTable("Untested", row => !logged.contains(row))

  private def showTable(title: String, pred: Row => Boolean): Unit = {
    underline(title, "=")
    underline(showRow("Form Status", "Role", "Operation", "Validity"), "-")

    enumeratedRows.foreach {
      case (row, valid) =>
        if (pred(row)) println(show(row, valid))
    }
  }

  private def underline(title: String, c: String): Unit = {
    println(title)
    println(c * title.length)
  }

  private def sort(map: Map[Row, PermissionResult]): Seq[(Row, PermissionResult)] =
    map.toList.sortBy(row => (showStatus(row._1), row._1.role, row._1.operation))

  private def show(row: Row, valid: PermissionResult): String =
    showRow(
      showStatus(row),
      row.role,
      row.operation,
      valid match {
        case PermissionResult.Permitted     => "Permitted"
        case PermissionResult.NotPermitted  => "Not Permitted"
        case PermissionResult.FormSubmitted => "Form Submitted"
      }
    )

  private def showRow(status: String, role: String, operation: String, validity: String): String = {
    val paddedValidity = pad(validity, 10)
    val paddedOperation = pad(operation, 25)
    val paddedRole = pad(role, 10)
    val paddedStatus = pad(status, 20)

    s"$paddedStatus$paddedRole$paddedOperation$paddedValidity"
  }

  private def showStatus(row: Row) = row match {
    case _: RowWithoutForm => ""
    case r: RowWithForm    => r.status
  }

  private def pad(s: String, l: Int) = s + (" " * (l - s.length))

  private def enumerateRows: Map[Row, PermissionResult] =
    enumerateWithoutFormPermittedRows ++ enumerateWithFormPermittedRows

  private def enumerateWithoutFormPermittedRows: Map[Row, PermissionResult] = {
    for {
      operation <- Set(
                    OperationWithoutForm.EditForm,
                    OperationWithoutForm.ShowAccessCode,
                    OperationWithoutForm.ViewDashboard,
                    OperationWithoutForm.Lookup)
      role <- roles
    } yield
      RowWithoutForm(operation.toString, role.toString) -> (Permissions.evaluateOperationWithoutForm(operation, role)(
        new Logger(NOPLogger.NOP_LOGGER)))
  }.toMap

  private def enumerateWithFormPermittedRows: Map[Row, PermissionResult] = {
    for {
      operation <- Set(
                    OperationWithForm.ForceUpdateFormStatus,
                    OperationWithForm.SubmitDeclaration,
                    OperationWithForm.EditForm,
                    OperationWithForm.ReviewSubmitted,
                    OperationWithForm.ReviewReturned,
                    OperationWithForm.DownloadSummaryPdf,
                    OperationWithForm.AcceptSummary,
                    OperationWithForm.ReviewAccepted,
                    OperationWithForm.UpdateFormField,
                    OperationWithForm.ViewDeclaration,
                    OperationWithForm.ViewSummary,
                    OperationWithForm.ViewAcknowledgement
                  )
      role   <- roles
      status <- FormStatus.all
    } yield
      RowWithForm(operation.toString, role.toString, status.toString) -> (Permissions
        .evaluateOperationWithForm(operation, role, status)(new Logger(NOPLogger.NOP_LOGGER)))
  }.toMap

  private def readLoggedOperations: Map[Row, Boolean] = {
    val logPattern =
      """.*(Valid|Invalid) *([A-Za-z]+) *(Customer|Agent|Reviewer) *([A-Za-z]*).*""".r

    Source
      .fromFile("logs/gform-frontend-permissions.log")
      .getLines
      .map {
        case logPattern(validity, operationWithoutForm, role, "") =>
          RowWithoutForm(operationWithoutForm, role) -> (validity === "Valid")
        case logPattern(validity, operationWithForm, role, status) =>
          RowWithForm(operationWithForm, role, status) -> (validity === "Valid")
      }
      .toMap
  }
}
