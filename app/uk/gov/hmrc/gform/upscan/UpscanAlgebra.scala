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

package uk.gov.hmrc.gform.upscan

import scala.language.higherKinds
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId, SectionNumber }
import uk.gov.hmrc.http.HeaderCarrier

trait UpscanAlgebra[F[_]] {
  def upscanInitiate(
    fileUploadIds: List[FormComponentId],
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    form: Form,
    formIdData: FormIdData
  )(implicit hc: HeaderCarrier): F[UpscanInitiate]

  def retrieveConfirmationOrFail(
    reference: UpscanReference
  )(implicit hc: HeaderCarrier): F[UpscanConfirmation]

  def retrieveConfirmation(
    reference: UpscanReference
  )(implicit hc: HeaderCarrier): F[Option[UpscanConfirmation]]

  def deleteConfirmation(
    reference: UpscanReference
  )(implicit hc: HeaderCarrier): F[Unit]
}
