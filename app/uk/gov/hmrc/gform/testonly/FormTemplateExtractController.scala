/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly

import cats.implicits._
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormTemplateId, IsInformationMessage, IsMiniSummaryList, IsTableComp }
import uk.gov.hmrc.gform.testonly.extract.{ FormTemplateDetail, FormTemplateDetailRow }
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.Aliases.{ HeadCell, Table, Text }
import uk.gov.hmrc.govukfrontend.views.html.components.GovukTable
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.TableRow
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.text.MessageFormat
import scala.concurrent.{ ExecutionContext, Future }

class FormTemplateExtractController(
  auth: AuthenticatedRequestActions,
  controllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(controllerComponents: MessagesControllerComponents) {

  private def ignoredComponent(formComponent: FormComponent) = formComponent match {
    case IsMiniSummaryList(_)    => true
    case IsTableComp(_)          => true
    case IsInformationMessage(_) => true
    case _                       => false
  }

  def extract(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]): Action[AnyContent] = {
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, accessCode, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => _ => _ =>
        val formTemplateDetails: List[FormTemplateDetail] = extractFormTemplate(cache)

        val header: List[HeadCell] = List(
          HeadCell(
            content = Text("Section")
          ),
          HeadCell(
            content = Text("Task")
          ),
          HeadCell(
            content = Text("Details")
          ),
          HeadCell(
            content = Text("Page title")
          ),
          HeadCell(
            content = Text("Page condition")
          ),
          HeadCell(
            content = Text("Field ID")
          ),
          HeadCell(
            content = Text("Field label")
          ),
          HeadCell(
            content = Text("Field format")
          ),
          HeadCell(
            content = Text("Field condition")
          ),
          HeadCell(
            content = Text("Field repeats")
          )
        )

        def evalSmartString(smartString: SmartString) =
          new MessageFormat(smartString.rawDefaultValue)
            .format(
              smartString
                .interpolations(_ => false)
                .map(i => s"$${${i.prettyPrint}}")
                .toArray
            )

        val formTemplateTableRows: List[List[Some[TableRow]]] = formTemplateDetails.map { row =>
          List(
            Some(
              TableRow(
                content = Text(evalSmartString(row.sectionTitle.value)),
                rowspan = Some(row.sectionTitle.rowSpan)
              )
            ),
            Some(
              TableRow(
                content = Text(evalSmartString(row.taskTitle.value)),
                rowspan = Some(row.taskTitle.rowSpan)
              )
            ),
            Some(
              TableRow(
                content = Text(row.shortName.map(evalSmartString).getOrElse("")),
                rowspan = Some(row.pageTitle.rowSpan)
              )
            ),
            Some(
              TableRow(
                content = Text(evalSmartString(row.pageTitle.value)),
                rowspan = Some(row.pageTitle.rowSpan)
              )
            ),
            Some(
              TableRow(
                content = Text(row.pageCondition.map(cond => s"$${${cond.booleanExpr.prettyPrint}}").getOrElse("")),
                rowspan = Some(row.pageTitle.rowSpan)
              )
            ),
            Some(TableRow(content = Text(row.fieldId.value))),
            Some(TableRow(content = Text(evalSmartString(row.fieldLabel)))),
            Some(TableRow(content = Text(row.fieldFormat.showType))),
            Some(
              TableRow(content =
                Text(row.fieldCondition.map(cond => s"$${${cond.booleanExpr.prettyPrint}}").getOrElse(""))
              )
            ),
            Some(TableRow(content = Text(if (row.fieldRepeats) "Y" else "N")))
          )
        }

        def normaliseTableRows(input: List[List[Option[TableRow]]]): List[List[Option[TableRow]]] = {
          val colCount = input.head.length
          val rowspans = Array.fill(colCount)(0)

          def processRow(row: List[Option[TableRow]]): List[Option[TableRow]] =
            row.zipWithIndex.map { case (cell, colIndex) =>
              if (rowspans(colIndex) > 0) {
                rowspans(colIndex) -= 1
                None
              } else {
                cell match {
                  case Some(TableRow(_, _, _, _, Some(rs), _)) if rs > 1 =>
                    rowspans(colIndex) = rs - 1
                  case _ =>
                }
                cell
              }
            }

          input.map(processRow)
        }

        val tableRows = normaliseTableRows(formTemplateTableRows)

        val title = s"Detailed Report for Form Template `${formTemplateId.value}`"

        val table = Table(
          head = Some(header),
          rows = tableRows.map(_.flatten),
          attributes = Map("border" -> "1px solid black"),
          caption = Some(title),
          captionClasses = "govuk-table__caption--m"
        )

        val htmlTable = new GovukTable()(table)
        Ok(html.debug.formTemplateExtract(title, htmlTable)).pure[Future]
    }

  }
  private def extractFormTemplate(cache: AuthCacheWithForm) = {
    cache.formTemplate.formKind
      .fold { classic =>
        val sectionFieldCount = classic.sections
          .flatMap(
            _.fold(page => page.page.fields)(page => page.page.fields)(page => page.pages.toList.flatMap(_.fields))
              .filterNot(ignoredComponent)
          )
          .size

        classic.sections.flatMap { section =>
          section.fold { nonRepeatingPage =>
            val page = nonRepeatingPage.page
            val allFields = page.allFields.filterNot(ignoredComponent)
            val pageFieldCount = allFields.size
            allFields.map { field =>
              FormTemplateDetail(
                FormTemplateDetailRow(SmartString.empty, sectionFieldCount),
                FormTemplateDetailRow(SmartString.empty, sectionFieldCount),
                page.shortName,
                FormTemplateDetailRow(page.title, pageFieldCount),
                page.includeIf,
                field.id,
                field.shortName.getOrElse(field.label),
                field.`type`,
                field.includeIf,
                false
              )
            }
          } { repeatingPage =>
            val page = repeatingPage.page
            val allFields = page.allFields.filterNot(ignoredComponent)
            val pageFieldCount = allFields.size
            allFields.map(field =>
              FormTemplateDetail(
                FormTemplateDetailRow(SmartString.empty, sectionFieldCount),
                FormTemplateDetailRow(SmartString.empty, sectionFieldCount),
                page.shortName,
                FormTemplateDetailRow(page.title, pageFieldCount),
                page.includeIf,
                field.id,
                field.shortName.getOrElse(field.label),
                field.`type`,
                field.includeIf,
                false
              )
            )
          } { atl =>
            atl.pages.toList.flatMap { page =>
              val allFields = page.allFields.filterNot(ignoredComponent)
              val pageFieldCount = allFields.size
              allFields
                .map(field =>
                  FormTemplateDetail(
                    FormTemplateDetailRow(SmartString.empty, sectionFieldCount),
                    FormTemplateDetailRow(SmartString.empty, sectionFieldCount),
                    page.shortName,
                    FormTemplateDetailRow(page.title, pageFieldCount),
                    page.includeIf,
                    field.id,
                    field.shortName.getOrElse(field.label),
                    field.`type`,
                    field.includeIf,
                    true
                  )
                )
            }
          }
        }
      } { taskList =>
        taskList.sections.toList.flatMap { taskSection =>
          val sectionTitle = taskSection.title
          val sectionFieldCount = taskSection.tasks.toList
            .flatMap(
              _.sections.toList
                .flatMap(
                  _.fold(page => page.page.fields)(page => page.page.fields)(page =>
                    page.pages.toList.flatMap(_.fields)
                  ).filterNot(ignoredComponent)
                )
            )
            .size
          taskSection.tasks.toList.flatMap { task =>
            val taskFieldCount = task.sections.toList
              .flatMap(
                _.fold(page => page.page.fields)(page => page.page.fields)(page => page.pages.toList.flatMap(_.fields))
                  .filterNot(ignoredComponent)
              )
              .size
            task.sections.toList.flatMap { section =>
              section.fold { nonRepeatingPage =>
                val page = nonRepeatingPage.page
                val allFields = page.allFields.filterNot(ignoredComponent)
                val pageFieldCount = allFields.size
                allFields.map { field =>
                  FormTemplateDetail(
                    FormTemplateDetailRow(sectionTitle, sectionFieldCount),
                    FormTemplateDetailRow(task.title, taskFieldCount),
                    page.shortName,
                    FormTemplateDetailRow(page.title, pageFieldCount),
                    page.includeIf,
                    field.id,
                    field.shortName.getOrElse(field.label),
                    field.`type`,
                    field.includeIf,
                    false
                  )
                }
              } { repeatingPage =>
                val page = repeatingPage.page
                val allFields = page.allFields.filterNot(ignoredComponent)
                val pageFieldCount = allFields.size
                allFields.map(field =>
                  FormTemplateDetail(
                    FormTemplateDetailRow(sectionTitle, sectionFieldCount),
                    FormTemplateDetailRow(task.title, taskFieldCount),
                    page.shortName,
                    FormTemplateDetailRow(page.title, pageFieldCount),
                    page.includeIf,
                    field.id,
                    field.shortName.getOrElse(field.label),
                    field.`type`,
                    field.includeIf,
                    false
                  )
                )
              } { atl =>
                atl.pages.toList.flatMap { page =>
                  val allFields = page.allFields.filterNot(ignoredComponent)
                  val pageFieldCount = allFields.size
                  allFields
                    .map(field =>
                      FormTemplateDetail(
                        FormTemplateDetailRow(sectionTitle, sectionFieldCount),
                        FormTemplateDetailRow(task.title, taskFieldCount),
                        page.shortName,
                        FormTemplateDetailRow(page.title, pageFieldCount),
                        page.includeIf,
                        field.id,
                        field.shortName.getOrElse(field.label),
                        field.`type`,
                        field.includeIf,
                        true
                      )
                    )
                }
              }
            }
          }
        }
      }
  }
}
