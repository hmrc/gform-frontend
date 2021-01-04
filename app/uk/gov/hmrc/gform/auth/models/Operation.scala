/*
 * Copyright 2021 HM Revenue & Customs
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

sealed trait OperationWithoutForm extends Product with Serializable

object OperationWithoutForm {
  final case object EditForm extends OperationWithoutForm
  final case object ShowAccessCode extends OperationWithoutForm
  final case object ViewDashboard extends OperationWithoutForm
  final case object Lookup extends OperationWithoutForm
}

sealed trait OperationWithForm extends Product with Serializable

object OperationWithForm {
  final case object DownloadSummaryPdf extends OperationWithForm
  final case object ReviewAccepted extends OperationWithForm
  final case object ReviewReturned extends OperationWithForm
  final case object ReviewSubmitted extends OperationWithForm
  final case object AcceptSummary extends OperationWithForm
  final case object SubmitDeclaration extends OperationWithForm
  final case object EditForm extends OperationWithForm
  final case object UpdateFormField extends OperationWithForm
  final case object ViewDeclaration extends OperationWithForm
  final case object ViewSummary extends OperationWithForm
  final case object ForceUpdateFormStatus extends OperationWithForm
  final case object ViewAcknowledgement extends OperationWithForm
  final case object ViewPrintSection extends OperationWithForm
  final case object DownloadPrintSectionPdf extends OperationWithForm
}
