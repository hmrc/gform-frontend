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

import cats.data.NonEmptyList
import cats.implicits._
import io.circe.CursorOp._
import io.circe._
import io.circe.syntax._
import play.api.i18n.{ I18nSupport, Messages }
import play.api.libs.circe.Circe
import play.api.libs.json.{ Json => PlayJson }
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, Request, Result }
import play.twirl.api.{ Html, HtmlFormat }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluationSyntax, SmartStringEvaluator }
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.gform.{ ExtraInfo, RenderUnit, SectionRenderingService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.{ PageModel, Repeater }
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.{ Bracket, DataExpanded, FormModel, SectionSelectorType, Singleton }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, NotChecked, Obligations, SmartString }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.summary.AddressRecordLookup
import uk.gov.hmrc.gform.tasklist.TaskListUtils
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

  private val compatibilityVersion =
    7; // This is managed manually. Increase it any time API used by builder extension is changed.

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
                  "version" := compatibilityVersion
                )
              )
            )
            .fold(BadRequest(s"No section for $sectionNumber found in form template $formTemplateId"))(json => Ok(json))
        }
    }

  def originalFormTemplate(formTemplateId: FormTemplateId) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => _ => _ => _ => _ =>
        gformConnector.getFormTemplateRaw(formTemplateId).map { formTemplateRaw =>
          val jsonString: String = PlayJson.stringify(formTemplateRaw)
          val maybeCirceJson: Either[ParsingFailure, Json] =
            io.circe.parser.parse(jsonString)

          val json: Option[Json] = maybeCirceJson.toOption

          json
            .map(json =>
              Json.obj(
                "formTemplate" := json,
                "version" := compatibilityVersion
              )
            )
            .fold(BadRequest(s"Form template not found for $formTemplateId"))(json => Ok(json))
        }
    }

  def originalSummarySection(formTemplateId: FormTemplateId, maybeCoordinates: Option[Coordinates]) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => lang => cache => sse => formModelOptics =>
        gformConnector.getFormTemplateRaw(formTemplateId).flatMap { formTemplateRaw =>
          val jsonString: String = PlayJson.stringify(formTemplateRaw)
          val maybeCirceJson: Either[ParsingFailure, Json] =
            io.circe.parser.parse(jsonString)

          val json: Option[Json] = maybeCirceJson.toOption.flatMap { json =>
            maybeCoordinates match {
              case Some(coordinates) =>
                json.hcursor
                  .downField("sections")
                  .downN(coordinates.taskSectionNumber.value)
                  .downField("tasks")
                  .downN(coordinates.taskNumber.value)
                  .downField("summarySection")
                  .focus
              case None => json.hcursor.downField("summarySection").focus
            }
          }

          json match {
            case None                 => gformConnector.getDefaultSummarySection(cache.formTemplate.formCategory).map(Ok(_))
            case Some(summarySection) => Ok(summarySection).pure[Future]
          }
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
              pageModel.fold { singleton =>
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
              }(checkYouAnswers => Option.empty[Json]) { repeater =>
                val history: List[CursorOp] =
                  List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections")) :::
                    historySuffix
                Some(
                  Json.obj(
                    "atlIterationIndex" := repeater.index,
                    "atlRepeater" := true,
                    "section" := addToListJson,
                    "sectionPath" := CursorOp.opsToPath(
                      history
                    ) // TODO sectionPath is not used, explore possibility of using this instead of using addToListId when updating mongo json
                  )
                )
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
            val validationResult = ValidationResult.empty
            val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel
            val singleton = formModel(sectionNumber).asInstanceOf[Singleton[DataExpanded]]
            val formLevelHeading = renderer.shouldDisplayHeading(singleton, formModelOptics, validationResult)

            Ok(
              Json.obj(
                "sectionHtml" := sanitiseHtml(sectionHtml),
                "html" := sanitiseHtml(html),
                "formLevelHeading" := formLevelHeading
              )
            ).pure[Future]
        }
    }

  def fetchTaskListSectionTitle(formTemplateId: FormTemplateId, sectionNumber: SectionNumber): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      _ => _ => cache => implicit sse => _ =>
        sectionNumber match {
          case SectionNumber.TaskList(Coordinates(taskSectionNumber, _), _) =>
            TaskListUtils.withTaskSection(cache.formTemplate, taskSectionNumber) { taskListSection =>
              Ok(
                Json.obj(
                  "html" := taskListSection.title.value()
                )
              ).pure[Future]
            }
          case _ => badRequest("Task section number not found").pure[Future]
        }
    }

  def fetchTaskTitle(formTemplateId: FormTemplateId, sectionNumber: SectionNumber): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      _ => _ => cache => implicit sse => _ =>
        sectionNumber match {
          case SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), _) =>
            TaskListUtils.withTask(cache.formTemplate, taskSectionNumber, taskNumber) { task =>
              Ok(
                Json.obj(
                  "html" := task.title.value()
                )
              ).pure[Future]
            }
          case _ => badRequest(s"Task section number $sectionNumber not found").pure[Future]
        }
    }

  def fetchSubmitSectionLabel(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      _ => _ => cache => implicit sse => _ =>
        val label = cache.formTemplate.submitSection.map(_.label.value()).getOrElse("")
        Ok(Json.obj("html" := label)).pure[Future]
    }

  def fetchSubmitSectionTaskLabel(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      _ => _ => cache => implicit sse => _ =>
        val taskLabel = cache.formTemplate.submitSection.map(_.taskLabel.value()).getOrElse("")
        Ok(Json.obj("html" := taskLabel)).pure[Future]
    }

  def fetchSectionHeaderHtml(formTemplateId: FormTemplateId, sectionNumber: SectionNumber) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        import i18nSupport._
        val sectionHtml = toSectionHtml(formModelOptics, sectionNumber)

        Ok(sectionHtml).pure[Future]
    }

  def fetchSummarySectionHtml(formTemplateId: FormTemplateId, maybeCoordinates: Option[Coordinates]) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      request => lang => cache => implicit sse => formModelOptics =>
        maybeCoordinates match {
          case Some(coordinates) =>
            TaskListUtils.withTask(
              cache.formTemplate,
              coordinates.taskSectionNumber,
              coordinates.taskNumber
            )(task =>
              task.summarySection.fold(
                badRequest(
                  s"Task of these coordinates: $coordinates doesn't have a summary section. Failed to fetch its json."
                )
                  .pure[Future]
              )(renderSummarySection)
            )
          case None =>
            renderSummarySection(cache.formTemplate.summarySection)
        }

    }

  private def renderSummarySection(summarySection: SummarySection)(implicit sse: SmartStringEvaluator) = {
    val response = Json.obj(
      "title" := summarySection.title.value(),
      "header" := markDownParser(summarySection.header.value()),
      "footer" := markDownParser(summarySection.footer.value())
    )
    Ok(response).pure[Future]
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

  def htmlForSumarySectionFormComponent(
    formTemplateId: FormTemplateId,
    formComponentId: FormComponentId,
    maybeCoordinates: Option[Coordinates]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        import i18nSupport._

        maybeCoordinates match {
          case Some(coordinates) =>
            TaskListUtils.withTask(
              cache.formTemplate,
              coordinates.taskSectionNumber,
              coordinates.taskNumber
            )(task =>
              task.summarySection.fold(
                badRequest(
                  s"Task of these coordinates: $coordinates doesn't have a summary section. Cannot render html for $formComponentId"
                )
                  .pure[Future]
              )(summarySection =>
                renderHtmlForSumarySectionFormComponent(summarySection, formComponentId, formModelOptics, cache)
              )
            )
          case None =>
            renderHtmlForSumarySectionFormComponent(
              cache.formTemplate.summarySection,
              formComponentId,
              formModelOptics,
              cache
            )
        }
    }

  private def renderHtmlForSumarySectionFormComponent(
    summarySection: SummarySection,
    formComponentId: FormComponentId,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    cache: AuthCacheWithForm
  )(implicit
    sse: SmartStringEvaluator,
    request: Request[_],
    messages: Messages,
    l: LangADT
  ) = {
    val formComponents = summarySection.fields
    formComponents match {
      case None =>
        badRequest(s"Summary section doesn't contain any fields. Trying to find: ${formComponentId.value}")
          .pure[Future]
      case Some(nel) =>
        nel.toList.find(_.id === formComponentId) match {
          case None =>
            badRequest(s"Failed to find formComponentId: ${formComponentId.value} in summary section fields")
              .pure[Future]
          case Some(formComponent) =>
            val renderUnit = RenderUnit.Pure(formComponent)

            val validationResult = ValidationResult.empty

            val obligations = NotChecked

            val page = summarySection.toPage
            val singleton = Singleton(page.asInstanceOf[Page[DataExpanded]])

            val ei: ExtraInfo = ExtraInfo(
              singleton = singleton,
              maybeAccessCode = None,
              sectionNumber = cache.formTemplate.sectionNumberZero,
              formModelOptics = formModelOptics,
              formTemplate = cache.formTemplate,
              envelopeId = cache.form.envelopeId,
              envelope = EnvelopeWithMapping.empty,
              formMaxAttachmentSizeMB = 0,
              retrievals = cache.retrievals,
              formLevelHeading = false,
              specialAttributes = Map.empty[String, String],
              addressRecordLookup = AddressRecordLookup.from(cache.form.thirdPartyData)
            )

            val formComponentHtml = renderer.htmlFor(
              renderUnit,
              cache.formTemplate._id,
              ei,
              validationResult,
              obligations,
              UpscanInitiate.empty,
              Map.empty[FormComponentId, UpscanData]
            )
            Ok(Json.obj("html" := sanitiseHtml(formComponentHtml))).pure[Future]
        }
    }
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

  def originalAcknowledgement(formTemplateId: FormTemplateId) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      None,
      OperationWithForm.EditFormAcknowledgement
    ) { implicit request => lang => cache => sse => formModelOptics =>
      gformConnector.getFormTemplateRaw(formTemplateId).flatMap { formTemplateRaw =>
        val jsonString: String = PlayJson.stringify(formTemplateRaw)
        val maybeCirceJson: Either[ParsingFailure, Json] =
          io.circe.parser.parse(jsonString)

        val json: Option[Json] = maybeCirceJson.toOption.flatMap { json =>
          json.hcursor.downField("acknowledgementSection").focus
        }
        json match {
          case None                         => InternalServerError("Unable to retrieve acknowledgement section").pure[Future]
          case Some(acknowledgementSection) => Ok(acknowledgementSection).pure[Future]
        }
      }

    }

  def generateAcknowledgementPanelHtml(formTemplateId: FormTemplateId) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      None,
      OperationWithForm.EditFormAcknowledgement
    ) { implicit request => _ => cache => implicit sse => formModelOptics =>
      import i18nSupport._
      val formTemplate = cache.formTemplate
      val envelopeId = cache.form.envelopeId

      val destinationList = formTemplate.destinations match {
        case destinationList: DestinationList => Some(destinationList)
        case _                                => None
      }

      val formCategory = formTemplate.formCategory
      val heading = renderer.acknowledgementHeading(formCategory)

      destinationList.map { l =>
        val panelTitle = l.acknowledgementSection.panelTitle.map(_.value())
        val showReference = l.acknowledgementSection.showReference
        (panelTitle, showReference)
      } match {
        case Some((panelTitle, showReference)) =>
          val html = renderer.renderAcknowledgementPanel(
            panelTitle,
            showReference,
            formCategory,
            envelopeId,
            heading
          )
          Ok(Json.obj("panelHtml" := sanitiseHtml(html))).pure[Future]
        case _ => badRequest(s"Destination list is not defined").pure[Future]
      }
    }

  def htmlForAcknowledgementFormComponent(
    formTemplateId: FormTemplateId,
    formComponentId: FormComponentId
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      None,
      OperationWithForm.EditFormAcknowledgement
    ) { implicit request => implicit lang => cache => implicit sse => formModelOptics =>
      import i18nSupport._

      val maybeAcknowledgementSection = cache.formTemplate.destinations.fold[Option[AcknowledgementSection]](ds =>
        Option[AcknowledgementSection](ds.acknowledgementSection)
      )(_ => Option.empty[AcknowledgementSection])
      maybeAcknowledgementSection match {
        case Some(acknowledgementSection) =>
          renderHtmlForAcknowledgementFormComponent(
            acknowledgementSection,
            formComponentId,
            formModelOptics,
            cache
          )
        case _ => badRequest("Can not find acknowledgement section in the form").pure[Future]
      }

    }

  private def renderHtmlForAcknowledgementFormComponent(
    acknowledgementSection: AcknowledgementSection,
    formComponentId: FormComponentId,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    cache: AuthCacheWithForm
  )(implicit
    sse: SmartStringEvaluator,
    request: Request[_],
    messages: Messages,
    l: LangADT
  ) = {
    val formComponents = acknowledgementSection.fields
    formComponents match {
      case Nil =>
        badRequest(s"Acknowledgement section doesn't contain any fields. Trying to find: ${formComponentId.value}")
          .pure[Future]
      case nel =>
        nel.find(_.id === formComponentId) match {
          case None =>
            badRequest(s"Failed to find formComponentId: ${formComponentId.value} in acknowledgment section fields")
              .pure[Future]
          case Some(formComponent) =>
            val renderUnit = RenderUnit.Pure(formComponent)

            val validationResult = ValidationResult.empty

            val obligations = NotChecked

            val page = acknowledgementSection.toPage
            val singleton = Singleton(page.asInstanceOf[Page[DataExpanded]])

            val ei: ExtraInfo = ExtraInfo(
              singleton = singleton,
              maybeAccessCode = None,
              sectionNumber = cache.formTemplate.sectionNumberZero,
              formModelOptics = formModelOptics,
              formTemplate = cache.formTemplate,
              envelopeId = cache.form.envelopeId,
              envelope = EnvelopeWithMapping.empty,
              formMaxAttachmentSizeMB = 0,
              retrievals = cache.retrievals,
              formLevelHeading = false,
              specialAttributes = Map.empty[String, String],
              addressRecordLookup = AddressRecordLookup.from(cache.form.thirdPartyData)
            )

            val formComponentHtml = renderer.htmlFor(
              renderUnit,
              cache.formTemplate._id,
              ei,
              validationResult,
              obligations,
              UpscanInitiate.empty,
              Map.empty[FormComponentId, UpscanData]
            )
            Ok(Json.obj("html" := sanitiseHtml(formComponentHtml))).pure[Future]
        }
    }
  }

  def fetchAtlRepeaterHtml(formTemplateId: FormTemplateId, sectionNumber: SectionNumber) =
    fetchAtlRepeater(formTemplateId, sectionNumber) { _ => implicit sse => repeater => bracket =>
      val descriptions: NonEmptyList[SmartString] = bracket.repeaters.map(_.expandedDescription)

      val recordTable: NonEmptyList[Html] = descriptions.zipWithIndex.map { case (description, index) =>
        val html = markDownParser(description)
        html
      }

      val pageHeading: Html = uk.gov.hmrc.gform.views.html
        .page_heading(repeater.expandedTitle.value(), repeater.expandedCaption.map(_.value()))

      Json.obj(
        "pageHeading" := pageHeading,
        "descriptions" := recordTable
      )
    }

  def fetchAtlRepeaterAddAnotherQuestionHtml(formTemplateId: FormTemplateId, sectionNumber: SectionNumber) =
    fetchAtlRepeater(formTemplateId, sectionNumber) { _ => implicit sse => repeater => _ =>
      Json.obj(
        "label" := repeater.addAnotherQuestion.label.value()
      )
    }

  def fetchAtlRepeaterFormComponentHtml(
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    formComponentId: FormComponentId
  ) =
    fetchAtlRepeater(formTemplateId, sectionNumber) { implicit messages => implicit sse => repeater => _ =>
      val infoHtml = repeater.fields.fold(HtmlFormat.empty) { fieldsNel =>
        fieldsNel.toList
          .collectFirst {
            case fc @ IsInformationMessage(info) if fc.id === formComponentId =>
              renderer.htmlForInformationMessage(fc, info.infoType, info.infoText)
          }
          .getOrElse(HtmlFormat.empty)
      }
      Json.obj(
        "html" := sanitiseHtml(infoHtml)
      )
    }

  private def fetchAtlRepeater(formTemplateId: FormTemplateId, sectionNumber: SectionNumber)(
    f: Messages => SmartStringEvaluator => Repeater[DataExpanded] => Bracket.AddToList[DataExpanded] => Json
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => lang => cache => sse => formModelOptics =>
        import i18nSupport._
        val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel
        val pageModel: PageModel[DataExpanded] = formModel(sectionNumber)
        val bracket: Bracket[DataExpanded] = formModel.bracket(sectionNumber)

        bracket match {
          case Bracket.NonRepeatingPage(_, _, _) =>
            badRequest("Expected AddToList bracket, but got NonRepeatingPage").pure[Future]
          case Bracket.RepeatingPage(_, _) =>
            badRequest("Expected AddToList bracket, but got RepeatingPage").pure[Future]
          case bracket @ Bracket.AddToList(_, _) =>
            pageModel
              .fold(singleton => badRequest("Invalid page model. Expected Repeater got Singleton"))(checkYourAnswers =>
                badRequest("Invalid page model. Expected Repeater got CheckYourAnswers")
              ) { repeater =>
                Ok(f(implicitly[Messages])(sse)(repeater)(bracket))
              }
              .pure[Future]
        }
    }
}
