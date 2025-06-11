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

import cats.implicits.catsSyntaxEq
import play.api.i18n.Messages
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.gform.eval.{ AllPageModelExpressionsGetter, TypeInfo }
import uk.gov.hmrc.gform.gform.processor.FormProcessor
import uk.gov.hmrc.gform.models.{ Bracket, ConfirmationAction, ConfirmationPage, DataExpanded, EnteredVariadicFormData, FastForward, FormModel, PageModel, ProcessData, Visibility }
import uk.gov.hmrc.gform.models.ids.ModelPageId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SmartString, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.{ ConfirmationExprMapping, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormCtx, FormTemplateId, IsInformationMessage, SectionNumber, SuppressErrors }

class ConfirmationService(
  formProcessor: FormProcessor
) {

  def processConfirmation(
    sectionNumber: SectionNumber,
    processData: ProcessData,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    fastForward: List[FastForward]
  )(implicit messages: Messages): ConfirmationAction = {

    val formModel: FormModel[Visibility] = processData.formModelOptics.formModelVisibilityOptics.formModel
    val pageModel: PageModel[Visibility] = formModel(sectionNumber)

    updateConfirmationPagesForChangedExprs(processData, formModelOptics, sectionNumber, formModel, pageModel) match {
      case Some(confirmationAction) => confirmationAction
      case _ =>
        val confirmationPage: ConfirmationPage = pageModel.confirmationPage(formModel.reverseConfirmationMap)

        confirmationPage match {
          case ConfirmationPage.Confirmee(confirmedSectionNumber, confirmation) =>
            val browserData: Set[VariadicValue] =
              processData.formModelOptics.formModelVisibilityOptics.data.withSectionNumber(sectionNumber)
            val mongoData = formModelOptics.formModelVisibilityOptics.data.withSectionNumber(sectionNumber)

            if (browserData != mongoData) {
              // We need to remove confirmation answer
              ConfirmationAction
                .UpdateConfirmation(
                  processData => processData.removeConfirmation(List(confirmation)),
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
              case _ =>
                val maybeRedirect = confirmation.redirects.toList
                  .flatMap(_.toList)
                  .find(r => processData.formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(r.`if`, None))
                maybeRedirect.fold(
                  ConfirmationAction.noop
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
          case ConfirmationPage.Not => ConfirmationAction.noop
        }
    }
  }

  private def updateConfirmationPagesForChangedExprs(
    processData: ProcessData,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sectionNumber: SectionNumber,
    formModel: FormModel[Visibility],
    pageModel: PageModel[Visibility]
  )(implicit messages: Messages): Option[ConfirmationAction.UpdateConfirmation] =
    pageModel.maybeConfirmation match {
      case Some(confirmation) => None
      case _ =>
        val browserData =
          processData.formModelOptics.formModelVisibilityOptics.data.allWithSectionNumber(sectionNumber).toMap
        val mongoData = formModelOptics.formModelVisibilityOptics.data.allWithSectionNumber(sectionNumber).toMap

        val changedIds = browserData.collect {
          case (id, value) if mongoData.get(id).exists(_ != value) => id
        }

        if (changedIds.nonEmpty) {
          val formModel = formModelOptics.formModelRenderPageOptics.formModel

          val impactedExprs = changedIds
            .flatMap(id => formModel.fcLookup.get(id.toFormComponentId))
            .map(fc => FormCtx(fc.id))

          val changedConfirmationPages = formModel.confirmationPageMap.collect {
            case (sn, confirmation) if impactedExprs.exists(getAllExprs(formModel, sn).flatMap(_.leafs()).contains) =>
              confirmation
          }

          changedConfirmationPages match {
            case Nil => None
            case confirmationPages =>
              Some(
                ConfirmationAction.UpdateConfirmation(
                  processData => processData.removeConfirmation(confirmationPages),
                  true
                )
              )
          }
        } else {
          processAllConfirmationExprs(
            processData.formModelOptics.formModelVisibilityOptics,
            processData.confirmationExprMapping
          )
        }
    }

  def processAllConfirmationExprs(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
    confirmationExprMapping: ConfirmationExprMapping
  )(implicit messages: Messages): Option[ConfirmationAction.UpdateConfirmation] = {
    val formModel = formModelVisibilityOptics.formModel

    val confirmationPageExprsMap = formModel.confirmationPageMap.map { case (sectionNumber, confirmation) =>
      val pageModel: PageModel[Visibility] = formModel(sectionNumber)

      val infoTexts: Seq[SmartString] = pageModel.allFormComponents.collect { case IsInformationMessage(im) =>
        im.infoText
      }

      val infoTextExprs: Seq[Expr] = infoTexts.flatMap(_.allInterpolations)

      val exprMap: Map[String, String] = infoTextExprs.map { expr =>
        val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(expr)
        val result = formModelVisibilityOptics
          .evalAndApplyTypeInfo(typeInfo)
          .stringRepresentation
        expr.toString -> result
      }.toMap
      confirmation -> exprMap
    }.toMap

    val changedConfirmationPages = confirmationPageExprsMap.flatMap { case (confirmation, exprMap) =>
      val existingExprMap = confirmationExprMapping.mapping.getOrElse(confirmation.question.id, Map.empty)

      val addedOrChanged: Map[String, String] = exprMap.filter { case (k, v) =>
        !existingExprMap.get(k).contains(v)
      }

      val removed: Map[String, String] = existingExprMap.keySet
        .diff(exprMap.keySet)
        .map { k =>
          k -> existingExprMap(k)
        }
        .toMap

      if (addedOrChanged.nonEmpty || removed.nonEmpty) {
        Some(confirmation)
      } else {
        None
      }
    }

    val newConfirmationExprMapping = confirmationPageExprsMap.map { case (confirmation, map) =>
      confirmation.question.id -> map
    }

    Some(
      ConfirmationAction.UpdateConfirmation(
        processData =>
          processData
            .copy(confirmationExprMapping = ConfirmationExprMapping(newConfirmationExprMapping))
            .removeConfirmation(changedConfirmationPages.toList),
        true
      )
    )
  }

  private def getAllExprs(formModel: FormModel[DataExpanded], sectionNumber: SectionNumber): Seq[Expr] = {
    val bracket = formModel.brackets.withSectionNumber(sectionNumber)

    bracket match {
      case atl @ Bracket.AddToList(_, _) =>
        val iteration: Bracket.AddToListIteration[DataExpanded] = atl.iterationForSectionNumber(sectionNumber)
        iteration.singletons
          .filter(_.sectionNumber === sectionNumber)
          .map(_.singleton)
          .flatMap(AllPageModelExpressionsGetter.fromSingleton(formModel))
      case _ => bracket.allExprs(formModel)
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
