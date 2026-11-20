/*
 * Copyright 2026 HM Revenue & Customs
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

import ComponentChecker._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.bulkupload.{ BulkUploadResult, BulkUploadValidator }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, IsBulkUpload }

class BulkUploadChecker() extends ComponentChecker[Unit] {

  override protected def checkProgram(context: CheckerDependency)(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    validate(context)

  def validate(context: CheckerDependency)(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val formComponent: FormComponent = context.formComponent

    formComponent match {
      case IsBulkUpload(bulkUpload) =>
        context.cache.thirdPartyData.bulkUploadLookup match {
          case None => successProgram(())
          case Some(lookup) =>
            lookup.get(formComponent.id) match {
              case None => successProgram(())
              case Some(bulkUploadResult) =>
                bulkUploadResult match {
                  case BulkUploadResult.Ok(data) =>
                    val validationErrors = BulkUploadValidator.validate(bulkUpload, data)
                    if (validationErrors.isEmpty) {
                      successProgram(())
                    } else {
                      CheckerServiceHelper.validationFailure(
                        formComponent,
                        "bulk.upload.validation.errors",
                        None
                      )
                    }
                  case BulkUploadResult.Error(error) =>
                    CheckerServiceHelper.validationFailure(
                      formComponent,
                      error,
                      None
                    )
                }

            }
        }
      case _ => successProgram(())
    }
  }

}
