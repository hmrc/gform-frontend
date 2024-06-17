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
import uk.gov.hmrc.gform.objectStore.Available
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.objectStore.Error
import uk.gov.hmrc.gform.objectStore.File
import uk.gov.hmrc.gform.objectStore.Infected
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.FileId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent

import ComponentChecker._

class FileUploadChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    validate(context.formComponent, context.envelope)

  implicit val fileValueForReport: ValueForReport[File] = new ValueForReport[File] {
    def valueForReport(): File =
      File(
        FileId("report-file-id"),
        Available,
        "report-file-name",
        ContentType("application/pdf"),
        10L,
        Map()
      )

  }

  def validate(formComponent: FormComponent, envelope: EnvelopeWithMapping)(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val file: Option[File] = envelope.find(formComponent.id.modelComponentId)
    file.foldProgram(
      onNone = ifProgram(
        andCond = formComponent.mandatory,
        thenProgram = CheckerServiceHelper.validationFailure(formComponent, "generic.error.upload", None),
        elseProgram = successProgram(())
      ),
      onSomeFun = { f =>
        val File(_, status, _, _, _, _) = f
        val isErrorStatus = status match {
          case Error(_) => true
          case _        => false
        }
        val isInfectedStatus = status match {
          case Infected => true
          case _        => false
        }
        switchProgram(
          switchCase(
            cond = isErrorStatus,
            thenProgram = CheckerServiceHelper.validationFailure(formComponent, "generic.error.unknownUpload", None)
          ),
          switchCase(
            cond = isInfectedStatus,
            thenProgram = CheckerServiceHelper.validationFailure(formComponent, "generic.error.virus", None)
          )
        )(elseProgram = successProgram(()))
      }
    )
  }

}
