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

package uk.gov.hmrc.gform.gform

import play.api.mvc.Results.Redirect
import uk.gov.hmrc.gform.gform.processor.FormProcessor
import uk.gov.hmrc.gform.models.{ ConfirmationAction, ConfirmationPage, EnteredVariadicFormData, FastForward, FormModel, PageModel, ProcessData, Visibility }
import uk.gov.hmrc.gform.models.ids.ModelPageId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, SectionNumber, SuppressErrors }

class ConfirmationService(
  formProcessor: FormProcessor
) {

  def processConfirmation(
    sectionNumber: SectionNumber,
    processData: ProcessData,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    fastForward: FastForward
  ): ConfirmationAction = {

    val formModel: FormModel[Visibility] = processData.formModelOptics.formModelVisibilityOptics.formModel
    val pageModel: PageModel[Visibility] = formModel(sectionNumber)
    val confirmationPage = pageModel.confirmationPage(formModel.reverseConfirmationMap)

    confirmationPage match {
      case ConfirmationPage.Confirmee(confirmedSectionNumber, confirmation) =>
        val browserData = processData.formModelOptics.formModelVisibilityOptics.data.forSectionNumber(sectionNumber)
        val mongoData = formModelOptics.formModelVisibilityOptics.data.forSectionNumber(sectionNumber)

        if (browserData != mongoData) {
          // We need to remove confirmation answer
          ConfirmationAction
            .UpdateConfirmation(
              processData => processData.removeConfirmation(confirmation),
              true
            )
        } else {
          ConfirmationAction.noop
        }

      case ConfirmationPage.Confirmator(confirmation) =>
        processData.formModelOptics.formModelVisibilityOptics.data
          .many(confirmation.question.id.modelComponentId)
          .toList
          .flatten match {
          case Nil =>
            val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processData, sectionNumber)
            ConfirmationAction
              .NotConfirmed(
                Redirect(
                  routes.FormController
                    .form(
                      formTemplateId,
                      maybeAccessCode,
                      sectionNumber,
                      sectionTitle4Ga,
                      SuppressErrors.No,
                      fastForward
                    )
                )
              )
          case "0" :: Nil =>
            ConfirmationAction.noop // Page is confirmed by user
          case _ => // Page is not confirmed
            val modelPageId: ModelPageId = confirmation.pageId.modelPageId

            val sn: SectionNumber = formModel.pageIdSectionNumberMap
              .getOrElse(modelPageId, throw new Exception(s"No section number found for pageId $modelPageId"))

            val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processData, sn)
            ConfirmationAction
              .NotConfirmed(
                Redirect(
                  routes.FormController
                    .form(
                      formTemplateId,
                      maybeAccessCode,
                      sn,
                      sectionTitle4Ga,
                      SuppressErrors.Yes,
                      fastForward
                    )
                )
              )
        }
      case ConfirmationPage.Not => ConfirmationAction.noop
    }
  }

  def purgeConfirmationData(
    sectionNumber: SectionNumber,
    processData: ProcessData,
    enteredVariadicFormData: EnteredVariadicFormData = EnteredVariadicFormData.empty
  ): PurgeConfirmationData = {

    val formModel: FormModel[Visibility] = processData.formModelOptics.formModelVisibilityOptics.formModel
    val pageModel: PageModel[Visibility] = formModel(sectionNumber)

    pageModel.maybeConfirmation match {

      // We do not want to keep confirmation data on exit from confirmation page (or when back link is clicked)
      // But only remove data when answer is "1" ie. not confirmed
      case Some(confirmation) =>
        PurgeConfirmationData(
          processData.formModelOptics.formModelVisibilityOptics.data
            .many(confirmation.question.id.modelComponentId)
            .toList
            .flatten match {
            case "1" :: Nil => _.removeConfirmation(confirmation)
            case _          => identity
          },
          EnteredVariadicFormData(
            enteredVariadicFormData.userData -- enteredVariadicFormData.userData.by(confirmation.question)
          )
        )

      case None => PurgeConfirmationData(identity, enteredVariadicFormData)

    }
  }
}

case class PurgeConfirmationData(f: ProcessData => ProcessData, enteredVariadicFormData: EnteredVariadicFormData)
