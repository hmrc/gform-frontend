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

package uk.gov.hmrc.gform.builder

import cats.implicits._
import io.circe.CursorOp._
import io.circe._
import io.circe.syntax._
import play.api.i18n.{ I18nSupport, Messages }
import play.api.libs.circe.Circe
import play.api.libs.json.{ Json => PlayJson }
import play.api.mvc.{ MessagesControllerComponents, Request, Result }
import play.twirl.api.Html

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.gform.{ ExtraInfo, RenderUnit, SectionRenderingService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.{ Bracket, DataExpanded, FormModel, SectionSelectorType, Singleton }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.{ NotChecked, Obligations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summary.AddressRecordLookup
import uk.gov.hmrc.gform.upscan.{ UpscanData, UpscanInitiate }
import uk.gov.hmrc.gform.validation.{ ComponentField, FieldOk, FormFieldValidationResult, HtmlFieldId }
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class BuilderController(
  auth: AuthenticatedRequestActions,
  renderer: SectionRenderingService,
  i18nSupport: I18nSupport,
  gformConnector: GformConnector,
  messagesControllerComponents: MessagesControllerComponents
)(implicit
  ex: ExecutionContext
) extends FrontendController(messagesControllerComponents) with Circe {

  implicit val htmlEncoder: Encoder[Html] = new Encoder[Html] {
    final def apply(html: Html): Json = Json.fromString(html.toString)
  }

  // Returns section from raw json which correspond to runtime sectionNumber parameter.
  def originalSection(formTemplateId: FormTemplateId, sectionNumber: SectionNumber) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => lang => cache => sse => formModelOptics =>
        gformConnector.getFormTemplateRaw(formTemplateId).map { formTemplateRaw =>
          val jsonString: String = PlayJson.stringify(formTemplateRaw)
          val maybeCirceJson: Either[ParsingFailure, Json] =
            io.circe.parser.parse(jsonString)

          val json: Option[Json] = maybeCirceJson.toOption.flatMap { json =>
            val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel

            val bracket = formModel.bracket(sectionNumber)

            bracket.fold { nonRepeatingPage =>
              val pageModel = nonRepeatingPage.singleton

              pageModel.allFormComponentIds match {
                case Nil => None
                case head :: _ =>
                  val compName = head.baseComponentId

                  sectionNumber match {
                    case SectionNumber.Classic(value) =>
                      originalNonRepeatingPageJson(json.hcursor, compName, Nil)
                    case SectionNumber.TaskList(coordinates, sn) =>
                      val acursor: ACursor = json.hcursor
                        .downField("sections")
                        .downN(coordinates.taskSectionNumber.value)
                        .downField("tasks")
                        .downN(coordinates.taskNumber.value)

                      val historySuffix =
                        List.fill(coordinates.taskNumber.value)(MoveRight) :::
                          List(DownArray, DownField("tasks")) :::
                          List.fill(coordinates.taskSectionNumber.value)(MoveRight) :::
                          List(DownArray, DownField("sections"))

                      originalNonRepeatingPageJson(acursor, compName, historySuffix)
                  }
              }
            }(repeatingPage => Option.empty[Json]) { addToList =>
              sectionNumber match {
                case SectionNumber.Classic(value) =>
                  originalSectionInAddToList(json, addToList, formModel, sectionNumber, Nil)
                case SectionNumber.TaskList(coordinates, sn) =>
                  json.hcursor
                    .downField("sections")
                    .values
                    .toList
                    .flatMap { taskSections =>
                      taskSections.zipWithIndex.flatMap { case (taskSection, taskSectionIndex) =>
                        taskSection.hcursor.downField("tasks").values.toList.flatMap { tasks =>
                          tasks.zipWithIndex.map { case (task, taskIndex) =>
                            val historySuffix =
                              List.fill(taskIndex)(MoveRight) ::: List(DownArray, DownField("tasks")) :::
                                List.fill(taskSectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))
                            originalSectionInAddToList(
                              task,
                              addToList,
                              formModel,
                              sectionNumber,
                              historySuffix
                            )
                          }
                        }
                      }
                    }
                    .collectFirst { case Some(json) => json }
              }
            }
          }

          val hiddenComponentIds = hiddenComponentIdsBySection(formModelOptics, sectionNumber);
          json
            .map(
              _.deepMerge(
                Json.obj(
                  "hiddenComponentIds" := hiddenComponentIds,
                  "version" := 1 // This is managed manually. Increase it any time API used by builder extension is changed.
                )
              )
            )
            .fold(BadRequest(s"No section for $sectionNumber found in form template $formTemplateId"))(json => Ok(json))
        }
    }

  private def originalNonRepeatingPageJson(
    json: ACursor,
    compName: BaseComponentId,
    historySuffix: List[CursorOp]
  ) =
    json
      .downField("sections")
      .values
      .flatMap { sections =>
        sections.zipWithIndex
          .find { case (section, sectionIndex) =>
            section.hcursor.downField("fields").values.exists { fields =>
              fields.exists { field =>
                field.hcursor.downField("id").focus.contains(Json.fromString(compName.value))
              }
            }
          }
      }
      .map { case (section, sectionIndex) =>
        val history: List[CursorOp] =
          List
            .fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections")) ::: historySuffix
        Json.obj(
          "section" := section,
          "sectionPath" := CursorOp.opsToPath(history)
        )
      }

  private def originalSectionInAddToList(
    json: Json,
    addToList: Bracket.AddToList[DataExpanded],
    formModel: FormModel[DataExpanded],
    sectionNumber: SectionNumber,
    historySuffix: List[CursorOp]
  ): Option[Json] = {
    val addAnotherQuestionId = Json.fromString(addToList.source.addAnotherQuestion.id.value)
    json.hcursor.downField("sections").values.flatMap { sections =>
      sections.zipWithIndex
        .find { case (section, _) =>
          section.hcursor
            .downField("addAnotherQuestion")
            .downField("id")
            .focus
            .contains(addAnotherQuestionId)
        }
        .flatMap { case (addToListJson, sectionIndex) =>
          formModel.pageModelLookup
            .get(sectionNumber)
            .flatMap { pageModel =>
              pageModel.allFormComponentIds match {
                case Nil => None
                case head :: _ =>
                  findFormComponentInAddToList(addToListJson, head.baseComponentId)
                    .map { case (json, pageNumber) =>
                      val history: List[CursorOp] =
                        List.fill(pageNumber)(MoveRight) ::: List(DownArray, DownField("pages")) :::
                          List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections")) :::
                          historySuffix

                      Json.obj(
                        "atlIterationIndex" := head.modelComponentId.maybeIndex.getOrElse(0),
                        "section" := json,
                        "sectionPath" := CursorOp.opsToPath(history)
                      )
                    }
              }
            }
        }
    }
  }

  private def findFormComponentInAddToList(addToListJson: Json, compName: BaseComponentId): Option[(Json, Int)] =
    addToListJson.hcursor
      .downField("pages")
      .values
      .flatMap { pages =>
        pages.zipWithIndex.find { case (page, _) =>
          page.hcursor.downField("fields").values.exists { fields =>
            fields.exists { field =>
              field.hcursor
                .downField("id")
                .focus
                .contains(Json.fromString(compName.value))
            }
          }
        }
      }

  private def sanitiseHtml(html: Html): String =
    html.toString
      .trim()
      .replaceAll("\\s+\n", "\n")

  def fetchSectionAndFormComponentHtml(
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    formComponentId: FormComponentId
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        import i18nSupport._
        val sectionHtml = toSectionHtml(formModelOptics, sectionNumber)
        val componentHtml = toComponentHtml(formModelOptics, formTemplateId, sectionNumber, formComponentId, cache)

        componentHtml match {
          case Left(error) => badRequest(error).pure[Future]
          case Right(html) =>
            Ok(
              Json.obj(
                "sectionHtml" := sanitiseHtml(sectionHtml),
                "html" := sanitiseHtml(html)
              )
            ).pure[Future]
        }
    }

  def fetchSectionHeaderHtml(formTemplateId: FormTemplateId, sectionNumber: SectionNumber) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        import i18nSupport._
        val sectionHtml = toSectionHtml(formModelOptics, sectionNumber)

        Ok(sectionHtml).pure[Future]
    }

  private def badRequest(error: String): Result = BadRequest(PlayJson.obj("error" -> error))

  def fetchFormComponentHtml(
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    formComponentId: FormComponentId
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        import i18nSupport._

        toComponentHtml(formModelOptics, formTemplateId, sectionNumber, formComponentId, cache) match {
          case Left(error) => badRequest(error).pure[Future]
          case Right(html) => Ok(html).pure[Future]
        }
    }

  private def toSectionHtml(formModelOptics: FormModelOptics[DataOrigin.Mongo], sectionNumber: SectionNumber)(implicit
    sse: SmartStringEvaluator,
    request: Request[_],
    messages: Messages,
    l: LangADT
  ): Html = {
    val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel
    val singleton = formModel(sectionNumber).asInstanceOf[Singleton[DataExpanded]]

    val sectionHeader = singleton.page.sectionHeader()

    val validationResult = ValidationResult.empty

    val formLevelHeading = renderer.shouldDisplayHeading(singleton, formModelOptics, validationResult)

    val sectionHtml = html.form.section_header(sectionHeader, !formLevelHeading)

    sectionHtml

  }

  private def toComponentHtml(
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    formComponentId: FormComponentId,
    cache: AuthCacheWithForm
  )(implicit
    sse: SmartStringEvaluator,
    request: Request[_],
    messages: Messages,
    l: LangADT
  ): Either[String, Html] = {
    val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel

    val maybeFormComponent = formModel.fcLookup.get(formComponentId)

    maybeFormComponent match {
      case None => Left(s"No component with id = $formComponentId found")
      case Some(formComponent) =>
        val singleton = formModel(sectionNumber).asInstanceOf[Singleton[DataExpanded]]

        val formFieldValidationResult: FormFieldValidationResult = formComponent match {
          case IsOverseasAddress(_) =>
            val data = Map(HtmlFieldId.pure(formComponent.modelComponentId) -> FieldOk(formComponent, ""))

            ComponentField(formComponent, data)
          case IsChoice(_) | IsRevealingChoice(_) =>
            val maybeCheckedOptions: Option[Seq[String]] =
              formModelOptics.formModelVisibilityOptics.data.many(formComponent.modelComponentId)

            val data = maybeCheckedOptions.fold(Map.empty[HtmlFieldId, FormFieldValidationResult]) { checkedOptions =>
              checkedOptions.map { index =>
                HtmlFieldId.indexed(formComponent.id, index) -> FieldOk(formComponent, index)
              }.toMap
            }
            ComponentField(formComponent, data)
          case _ =>
            val currentValue = formModelOptics.formModelVisibilityOptics.data.one(formComponent.modelComponentId)
            FieldOk(formComponent, currentValue.getOrElse(""))
        }

        val validationResult = ValidationResult(Map(formComponent.id -> formFieldValidationResult), None)

        val formLevelHeading = renderer.shouldDisplayHeading(singleton, formModelOptics, validationResult)

        val renderUnit = RenderUnit.Pure(formComponent)

        val ei: ExtraInfo = ExtraInfo(
          singleton = singleton,
          maybeAccessCode = None,
          sectionNumber = sectionNumber,
          formModelOptics = formModelOptics,
          formTemplate = cache.formTemplate,
          envelopeId = cache.form.envelopeId,
          envelope = EnvelopeWithMapping.empty,
          formMaxAttachmentSizeMB = 10,
          retrievals = cache.retrievals,
          formLevelHeading = formLevelHeading,
          specialAttributes = Map.empty[String, String],
          addressRecordLookup = AddressRecordLookup.from(cache.form.thirdPartyData)
        )
        val obligations: Obligations = NotChecked

        val formComponentHtml = renderer.htmlFor(
          renderUnit,
          formTemplateId,
          ei,
          validationResult,
          obligations,
          UpscanInitiate.empty,
          Map.empty[FormComponentId, UpscanData]
        )

        Right(formComponentHtml)
    }
  }

  private def hiddenComponentIdsBySection(
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sectionNumber: SectionNumber
  ): List[String] = {
    val visibilityOptics = formModelOptics.formModelVisibilityOptics
    val renderOptics = formModelOptics.formModelRenderPageOptics
    val visibleFormComponentIds = visibilityOptics.formModel(sectionNumber).allFormComponentIds.map(_.value)
    val allFormComponentIds = renderOptics.formModel(sectionNumber).allFormComponentIds.map(_.value)
    allFormComponentIds diff visibleFormComponentIds
  }

}
