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

package uk.gov.hmrc.gform.validation

import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.objectStore.FileStatus
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.objectStore.File
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.FileId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileComponentId, FormComponent, IsMultiFileUpload }
import ComponentChecker._

class FileUploadChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    validate(context)

  implicit val fileValueForReport: ValueForReport[File] = new ValueForReport[File] {
    def valueForReport(): File =
      File(
        FileId("report-file-id"),
        "report-file-name",
        FileStatus.Available,
        ContentType("application/pdf"),
        10L,
        Map()
      )

  }

  def validate(context: CheckerDependency[D])(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val formComponent: FormComponent = context.formComponent
    val envelope: EnvelopeWithMapping = context.envelope

    val isMultiFileUplaod = formComponent match {
      case IsMultiFileUpload(_) => true
      case _                    => false
    }
    val file: Option[File] = if (isMultiFileUplaod) {
      val files: List[(FileComponentId, File)] = envelope.findMulti(formComponent.id.modelComponentId)
      files.headOption.map { case (_, file) => file }
    } else {
      envelope.findSingle(formComponent.id.modelComponentId)
    }
    file.foldProgram(
      onNone = ifProgram(
        andCond = formComponent.mandatory.eval(context.formModelVisibilityOptics.booleanExprResolver),
        thenProgram = CheckerServiceHelper.validationFailure(formComponent, "generic.error.upload", None),
        elseProgram = successProgram(())
      ),
      onSomeFun = { _ =>
        successProgram(())
      }
    )
  }
}
