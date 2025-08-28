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

package uk.gov.hmrc.gform.gform

import cats.syntax.all._
import cats.data.NonEmptyList
import play.api.i18n.Messages
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.gform.eval.ExpressionResultWithTypeInfo
import uk.gov.hmrc.gform.gform.processor.FormProcessor
import uk.gov.hmrc.gform.models.{ ConfirmationAction, ConfirmationPage, EnteredVariadicFormData, FastForward, FormModel, PageModel, ProcessData, Visibility }
import uk.gov.hmrc.gform.models.ids.ModelPageId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.VariadicValue
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Confirmation, Expr, FormComponentId, HasExpr }
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, SectionNumber, SuppressErrors }

object ConfirmationService {

  // Detect an invalid confirmations based on expression only
  // For example on sign-in into the form (at that point there are no browser data)
  def processConfirmation(
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    form: Form
  )(implicit messages: Messages): (List[Confirmation], Map[FormComponentId, List[String]]) =
    processConfirmation(formModelOptics, DataOrigin.unSwapDataOrigin(formModelOptics), form)

  def processConfirmation(
    mongoFormModelOptics: FormModelOptics[DataOrigin.Mongo],
    browserFormModelOptics: FormModelOptics[DataOrigin.Browser],
    form: Form
  )(implicit messages: Messages): (List[Confirmation], Map[FormComponentId, List[String]]) = {
    val confirmations = form.thirdPartyData.confirmations

    val formModel: FormModel[Visibility] = mongoFormModelOptics.formModelVisibilityOptics.formModel

    val allConfirmations = formModel.allConfirmations

    val currentConfirmations: Map[FormComponentId, List[String]] =
      mkCurrentConfirmations(allConfirmations, browserFormModelOptics)

    val mongoConfirmations = confirmations.getOrElse(Map.empty[FormComponentId, List[String]])

    val confirmationsToReset: List[Confirmation] = allConfirmations.filter { confirmation =>
      val isExpressionChange0 = isExpressionChange(confirmation, currentConfirmations, mongoConfirmations)
      val isFieldChange0 = isFieldChange(
        confirmation,
        browserFormModelOptics,
        mongoFormModelOptics,
        form
      )
      isExpressionChange0 || isFieldChange0
    }

    (confirmationsToReset, currentConfirmations)
  }

  def isExpressionChange(
    confirmation: Confirmation,
    currentConfirmations: Map[FormComponentId, List[String]],
    mongoConfirmations: Map[FormComponentId, List[String]]
  ): Boolean = {

    val formComponentId = confirmation.question.id
    val maybeMongoConfirmation = mongoConfirmations.get(formComponentId)
    val maybeCurrentConfirmation = currentConfirmations.get(formComponentId)

    (maybeMongoConfirmation, maybeCurrentConfirmation) match {
      case (Some(mongoConfirmation), Some(currentConfirmation)) =>
        mongoConfirmation =!= currentConfirmation.toList
      case (None, None) => false
      case _            => true
    }
  }

  def isFieldChange(
    confirmation: Confirmation,
    browserFormModelOptics: FormModelOptics[DataOrigin.Browser],
    mongoFormModelOptics: FormModelOptics[DataOrigin.Mongo],
    form: Form
  ): Boolean =
    confirmation.fieldsConfirmed
      .fold(false) { formComponentIds =>
        formComponentIds.exists { formComponentId =>
          val browserData: List[VariadicValue] =
            browserFormModelOptics.dataLookup.getOrElse(formComponentId.baseComponentId, List.empty[VariadicValue])

          val mongoData: List[VariadicValue] =
            mongoFormModelOptics.dataLookup.getOrElse(formComponentId.baseComponentId, List.empty[VariadicValue])

          val dataChange = browserData != mongoData

          val maybeFormComponent =
            browserFormModelOptics.formModelVisibilityOptics.fcLookup.get(formComponentId)

          val exprChange = maybeFormComponent.fold(false) { formComponent =>
            formComponent match {
              // When formComponent contains 'value' field with an expression,
              // we need to look directly into form data to see old value.
              case HasExpr(expr) =>
                val browserDataValue: Option[VariadicValue] =
                  browserFormModelOptics.formModelVisibilityOptics.data.get(formComponentId.modelComponentId)
                val formDataValue: Option[VariadicValue] =
                  form.formData.find(formComponentId.modelComponentId).map(res => VariadicValue.One(res))
                browserDataValue != formDataValue
              case _ => false
            }
          }

          dataChange || exprChange
        }
      }

  def mkCurrentConfirmations(
    allConfirmations: List[Confirmation],
    formModelOptics: FormModelOptics[DataOrigin.Browser]
  )(implicit messages: Messages): Map[FormComponentId, List[String]] =
    allConfirmations.flatMap { confirmation =>
      val expressionsConfirmed: Option[NonEmptyList[Expr]] = confirmation.expressionsConfirmed

      val results: Option[NonEmptyList[String]] = expressionsConfirmed.map { exprs =>
        exprs.map { expr =>
          val newResult: ExpressionResultWithTypeInfo =
            formModelOptics.formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr)
          val newRes: String = newResult.stringRepresentation
          newRes
        }
      }

      results.map(rs => confirmation.question.id -> rs.toList)
    }.toMap
}

class ConfirmationService(
  formProcessor: FormProcessor
) {

  def processConfirmation(
    sectionNumber: SectionNumber,
    processData: ProcessData,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    fastForward: List[FastForward],
    form: Form
  )(implicit messages: Messages): ConfirmationAction = {

    val (confirmationsToReset, currentConfirmations) =
      ConfirmationService.processConfirmation(formModelOptics, processData.formModelOptics, form)

    val formModel: FormModel[Visibility] = processData.formModelOptics.formModelVisibilityOptics.formModel
    val pageModel: PageModel[Visibility] = formModel(sectionNumber)

    val confirmationPage: ConfirmationPage = pageModel.confirmationPage

    confirmationPage match {
      case ConfirmationPage.Not =>
        if (confirmationsToReset.nonEmpty) {
          // We need to remove confirmation answer
          ConfirmationAction
            .UpdateConfirmation(processData =>
              processData
                .copy(confirmations = Some(currentConfirmations))
                .removeConfirmation(confirmationsToReset)
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
          case _ =>
            val maybeRedirect = confirmation.redirects.toList
              .flatMap(_.toList)
              .find(r => processData.formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(r.`if`, None))
            maybeRedirect.fold[ConfirmationAction](
              // This is needed to store confirmed expressions when confirmation is very first page in the journey
              ConfirmationAction
                .UpdateConfirmation(processData =>
                  processData
                    .copy(confirmations = Some(currentConfirmations))
                )
            ) { redirect =>
              // Page is not confirmed
              val modelPageId: ModelPageId = redirect.pageId.modelPageId

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
        }
    }
  }

  def purgeConfirmationData(
    sectionNumber: SectionNumber,
    processData: ProcessData,
    enteredVariadicFormData: EnteredVariadicFormData
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
            case "1" :: Nil => _.removeConfirmation(List(confirmation))
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
