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
import org.apache.poi.ss.usermodel.{ Cell, Row, Sheet, Workbook }
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormTemplateId, IsInformationMessage, IsMiniSummaryList, IsTableComp }
import uk.gov.hmrc.gform.testonly.extract.{ FormTemplateDetail, FormTemplateDetailRow, ReportTableRow }
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.Aliases.{ HeadCell, Table, Text }
import uk.gov.hmrc.govukfrontend.views.html.components.GovukTable
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.TableRow
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.io.ByteArrayOutputStream
import java.text.MessageFormat
import scala.collection.immutable.List
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

  private val headers = List(
    "Section",
    "Task",
    "Details",
    "Page title",
    "Page condition",
    "Field ID",
    "Field label",
    "Field format",
    "Field condition",
    "Field repeats"
  )

  private def evalSmartString(smartString: SmartString)(implicit lang: LangADT) =
    new MessageFormat(smartString.rawDefaultValue)
      .format(
        smartString
          .interpolations(_ => false)
          .map(i => s"$${${i.prettyPrint}}")
          .toArray
      )

  private def normaliseTableRows(input: List[List[Option[ReportTableRow]]]): List[List[Option[ReportTableRow]]] = {
    val colCount = input.head.length
    val rowspans = Array.fill(colCount)(0)

    def processRow(row: List[Option[ReportTableRow]]): List[Option[ReportTableRow]] =
      row.zipWithIndex.map { case (cell, colIndex) =>
        if (rowspans(colIndex) > 0) {
          rowspans(colIndex) -= 1
          None
        } else {
          cell match {
            case Some(ReportTableRow(_, Some(rs))) if rs > 1 =>
              rowspans(colIndex) = rs - 1
            case _ =>
          }
          cell
        }
      }

    input.map(processRow)
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
                field.showFormat,
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
                field.showFormat,
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
                    field.showFormat,
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
                    field.showFormat,
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
                    field.showFormat,
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
                        field.showFormat,
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

  private def makeReportTableRows(cache: AuthCacheWithForm)(implicit lang: LangADT) = {
    val formTemplateDetails = extractFormTemplate(cache)

    val tableRows = formTemplateDetails.map { row =>
      List(
        Some(
          ReportTableRow(
            content = evalSmartString(row.sectionTitle.value),
            rowspan = Some(row.sectionTitle.rowSpan)
          )
        ),
        Some(
          ReportTableRow(
            content = evalSmartString(row.taskTitle.value),
            rowspan = Some(row.taskTitle.rowSpan)
          )
        ),
        Some(
          ReportTableRow(
            content = row.shortName.map(evalSmartString).getOrElse(""),
            rowspan = Some(row.pageTitle.rowSpan)
          )
        ),
        Some(
          ReportTableRow(
            content = evalSmartString(row.pageTitle.value),
            rowspan = Some(row.pageTitle.rowSpan)
          )
        ),
        Some(
          ReportTableRow(
            content = row.pageCondition.map(cond => s"$${${cond.booleanExpr.prettyPrint}}").getOrElse(""),
            rowspan = Some(row.pageTitle.rowSpan)
          )
        ),
        Some(ReportTableRow(content = row.fieldId.value)),
        Some(ReportTableRow(content = evalSmartString(row.fieldLabel))),
        Some(ReportTableRow(content = row.fieldFormat)),
        Some(
          ReportTableRow(content = row.fieldCondition.map(cond => s"$${${cond.booleanExpr.prettyPrint}}").getOrElse(""))
        ),
        Some(ReportTableRow(content = if (row.fieldRepeats) "Y" else "N"))
      )
    }

    normaliseTableRows(tableRows)
  }

  def extract(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, accessCode, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => _ => _ =>
        val header: List[HeadCell] = headers.map(header =>
          HeadCell(
            content = Text(header)
          )
        )

        val tableRows: List[List[Option[TableRow]]] =
          makeReportTableRows(cache).map(
            _.map(_.map(row => TableRow(content = Text(row.content), rowspan = row.rowspan)))
          )

        val title = s"Detailed Report for Form Template `${formTemplateId.value}`"

        val table = Table(
          head = Some(header),
          rows = tableRows.map(_.flatten),
          attributes = Map("border" -> "1px solid black"),
          caption = Some(title),
          captionClasses = "govuk-table__caption--m"
        )

        val htmlTable = new GovukTable()(table)
        Ok(html.debug.formTemplateExtract(cache.formTemplate, accessCode, title, htmlTable)).pure[Future]
    }

  def exportToExcel(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, accessCode, OperationWithForm.EditForm) {
      _ => implicit lang => cache => _ => _ =>
        val workbook: Workbook = new XSSFWorkbook()
        val sheet: Sheet = workbook.createSheet(s"Detailed Report for ${formTemplateId.value}")
        val headerRow: Row = sheet.createRow(0)

        for (i <- headers.indices) {
          val cell: Cell = headerRow.createCell(i)
          cell.setCellValue(headers(i))
        }

        val tableRows = makeReportTableRows(cache).map(_.map(_.map(_.content).getOrElse("")))

        for (i <- tableRows.indices) {
          val row: Row = sheet.createRow(i + 1) // Start from the second row
          for (j <- tableRows(i).indices) {
            val cell: Cell = row.createCell(j)
            cell.setCellValue(tableRows(i)(j))
          }
        }

        for (i <- headers.indices)
          sheet.autoSizeColumn(i)

        val outputStream = new ByteArrayOutputStream()
        workbook.write(outputStream)
        val fileContents = outputStream.toByteArray

        outputStream.close()
        workbook.close()

        Ok(fileContents)
          .as("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
          .withHeaders(
            "Content-Disposition" -> s"attachment; filename=${formTemplateId.value}-details.xlsx"
          )
          .pure[Future]
    }
}
