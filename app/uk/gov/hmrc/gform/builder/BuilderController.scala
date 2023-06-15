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
import play.api.i18n.I18nSupport
import play.api.libs.circe.Circe
import play.api.libs.json.{ Json => PlayJson }
import play.api.mvc.{ MessagesControllerComponents, Result }
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.gform.{ ExtraInfo, RenderUnit, SectionRenderingService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel, SectionSelectorType, Singleton }
import uk.gov.hmrc.gform.sharedmodel.{ NotChecked, Obligations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summary.AddressRecordLookup
import uk.gov.hmrc.gform.upscan.UpscanInitiate
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

            val rawSectionNumber: Int = formModel.brackets
              .fold(classic =>
                classic.brackets.zipWithIndex
                  .find { case (b, index) => b.hasSectionNumber(sectionNumber) }
                  .map { case (_, index) =>
                    index
                  }
                  .getOrElse(0)
              )(taskList => 0)

            bracket.fold { nonRepeatingPage =>
              val pageModel = nonRepeatingPage.singleton

              pageModel.allFormComponentIds match {
                case Nil => None
                case head :: _ =>
                  val compName = head.baseComponentId.value
                  val res: Option[(Json, Int)] =
                    json.hcursor.downField("sections").values.flatMap { sections =>
                      sections.zipWithIndex
                        .find { case (section, sectionIndex) =>
                          section.hcursor.downField("fields").values.exists { fields =>
                            fields.exists { field =>
                              field.hcursor.downField("id").focus.contains(Json.fromString(compName))
                            }
                          }
                        }
                    }
                  res.map { case (section, sectionIndex) =>
                    val history: List[CursorOp] =
                      List
                        .fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))
                    Json.obj(
                      "section" := section,
                      "rawSectionNumber" := rawSectionNumber,
                      "sectionPath" := CursorOp.opsToPath(history)
                    )
                  }

              }
            }(repeatingPage => Option.empty[Json]) { addToList =>
              val addAnotherQuestionId = addToList.source.addAnotherQuestion.id.value

              json.hcursor.downField("sections").values.flatMap { sections =>
                sections.zipWithIndex
                  .find { case (section, sectionIndex) =>
                    section.hcursor
                      .downField("addAnotherQuestion")
                      .downField("id")
                      .focus
                      .contains(Json.fromString(addAnotherQuestionId))
                  }
                  .flatMap { case (addToListJson, sectionIndex) =>
                    formModel.pageModelLookup.get(sectionNumber).flatMap { pageModel =>
                      pageModel.allFormComponentIds match {
                        case Nil => None
                        case head :: _ =>
                          val compName = head.baseComponentId.value
                          val res: Option[(Json, Int)] =
                            addToListJson.hcursor.downField("pages").values.flatMap { pages =>
                              pages.zipWithIndex.find { case (page, pageIdex) =>
                                page.hcursor.downField("fields").values.exists { fields =>
                                  fields.exists { field =>
                                    field.hcursor.downField("id").focus.contains(Json.fromString(compName))
                                  }
                                }
                              }
                            }
                          res.map { case (json, pageNumber) =>
                            val history: List[CursorOp] =
                              List.fill(pageNumber)(MoveRight) ::: List(DownArray, DownField("pages")) ::: List
                                .fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))

                            Json.obj(
                              "atlIterationIndex" := head.modelComponentId.maybeIndex.getOrElse(0),
                              "section" := json,
                              "rawSectionNumber" := rawSectionNumber,
                              "sectionPath" := CursorOp.opsToPath(history)
                            )
                          }
                      }
                    }
                  }
              }
            }
          }

          json.fold(BadRequest(s"No section for $sectionNumber found in form template $formTemplateId"))(json =>
            Ok(json)
          )
        }
    }

  def fetchSectionHeaderHtml(formTemplateId: FormTemplateId, sectionNumber: SectionNumber) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        import i18nSupport._
        val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel
        val singleton = formModel(sectionNumber).asInstanceOf[Singleton[DataExpanded]]

        val sectionHeader = singleton.page.sectionHeader()

        val validationResult = ValidationResult.empty

        val formLevelHeading = renderer.shouldDisplayHeading(singleton, formModelOptics, validationResult)

        val sectionHtml = html.form.section_header(sectionHeader, !formLevelHeading)

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
        val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel

        val maybeFormComponent = formModel.fcLookup.get(formComponentId)

        maybeFormComponent match {
          case None => badRequest(s"No component with id = $formComponentId found").pure[Future]
          case Some(formComponent) =>
            val singleton = formModel(sectionNumber).asInstanceOf[Singleton[DataExpanded]]

            val formFieldValidationResult: FormFieldValidationResult = formComponent match {
              case IsOverseasAddress(_) =>
                val data = Map(HtmlFieldId.pure(formComponent.modelComponentId) -> FieldOk(formComponent, ""))

                ComponentField(formComponent, data)
              case IsChoice(_) | IsRevealingChoice(_) =>
                val maybeCheckedOptions: Option[Seq[String]] =
                  formModelOptics.formModelVisibilityOptics.data.many(formComponent.modelComponentId)

                val data = maybeCheckedOptions.fold(Map.empty[HtmlFieldId, FormFieldValidationResult]) {
                  checkedOptions =>
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
            val upscanInitiate: UpscanInitiate = UpscanInitiate.empty

            val formComponentHtml = renderer.htmlFor(
              renderUnit,
              formTemplateId,
              ei,
              validationResult,
              obligations,
              upscanInitiate
            )

            Ok(formComponentHtml).pure[Future]
        }
    }
}
