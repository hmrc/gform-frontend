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
import uk.gov.hmrc.gform.eval.BooleanExprResolver
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, SmartString }
import uk.gov.hmrc.gform.testonly.extract.{ FormTemplateDetail, FormTemplateDetailRow, ReportTableRow }
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.Aliases.{ HeadCell, HtmlContent, Table, Text }
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

  private val headers = List(
    "Section",
    "Task",
    "Details",
    "Page title",
    "Page condition",
    "Field ID",
    "Field mandatory",
    "Field label",
    "Field format",
    "Field format example",
    "Field min length",
    "Field max length",
    "Field value source",
    "Field condition",
    "Field repeats",
    "Specimen link"
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

  private def extractFormTemplate(
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    isCsv: Boolean
  )(implicit sse: SmartStringEvaluator) = {
    import uk.gov.hmrc.gform.testonly.extract.FormComponentHelpers._

    val booleanExprResolver: BooleanExprResolver = formModelOptics.formModelVisibilityOptics.booleanExprResolver

    val pagesWithIndex = formModelOptics.formModelRenderPageOptics.formModel.pagesWithIndex

    def getSpecimenLink(fc: FormComponent) =
      pagesWithIndex
        .find { case (page, _) => page.allFormComponentIds.map(_.baseComponentId).contains(fc.id.baseComponentId) }
        .map { case (page, sn) =>
          val sectionTitle = SectionTitle4Ga.sectionTitle4GaFactory(page, sn).value
          val url = s"/submissions/form/specimen-${cache.formTemplateId.value}/$sectionTitle?n=${sn.value}&se=t&ff=t"
          val html = s"""<a class="govuk-link" href="$url" target="_blank">${sn.value}</a>"""
          if (isCsv) url else html
        }

    cache.formTemplate.formKind
      .fold { classic =>
        val sectionFieldCount = classic.sections
          .flatMap(
            _.fold(page => page.page.allFieldsNested)(page => page.page.allFieldsNested)(page =>
              page.pages.toList.flatMap(_.allFieldsNested)
            )
          )
          .filterNot(ignoredComponent)
          .size

        classic.sections.flatMap { section =>
          section.fold { nonRepeatingPage =>
            val page = nonRepeatingPage.page
            val allFields = page.allFieldsNested.filterNot(ignoredComponent)
            val pageFieldCount = allFields.size
            allFields.map { field =>
              FormTemplateDetail(
                FormTemplateDetailRow(SmartString.empty, sectionFieldCount),
                FormTemplateDetailRow(SmartString.empty, sectionFieldCount),
                page.shortName,
                FormTemplateDetailRow(page.title, pageFieldCount),
                page.includeIf,
                field.id,
                field.mandatory.eval(booleanExprResolver),
                field.shortName.getOrElse(field.label),
                field.showFormat,
                showFormatExample(field),
                getMin(field),
                getMax(field),
                getValue(field),
                field.includeIf,
                false,
                getSpecimenLink(field)
              )
            }
          } { repeatingPage =>
            val page = repeatingPage.page
            val allFields = page.allFieldsNested.filterNot(ignoredComponent)
            val pageFieldCount = allFields.size
            allFields.map(field =>
              FormTemplateDetail(
                FormTemplateDetailRow(SmartString.empty, sectionFieldCount),
                FormTemplateDetailRow(SmartString.empty, sectionFieldCount),
                page.shortName,
                FormTemplateDetailRow(page.title, pageFieldCount),
                page.includeIf,
                field.id,
                field.mandatory.eval(booleanExprResolver),
                field.shortName.getOrElse(field.label),
                field.showFormat,
                showFormatExample(field),
                getMin(field),
                getMax(field),
                getValue(field),
                field.includeIf,
                false,
                getSpecimenLink(field)
              )
            )
          } { atl =>
            atl.pages.toList.flatMap { page =>
              val allFields = page.allFieldsNested.filterNot(ignoredComponent)
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
                    field.mandatory.eval(booleanExprResolver),
                    field.shortName.getOrElse(field.label),
                    field.showFormat,
                    showFormatExample(field),
                    getMin(field),
                    getMax(field),
                    getValue(field),
                    field.includeIf,
                    true,
                    getSpecimenLink(field)
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
                  _.fold(page => page.page.allFieldsNested)(page => page.page.allFieldsNested)(page =>
                    page.pages.toList.flatMap(_.allFieldsNested)
                  )
                )
                .filterNot(ignoredComponent)
            )
            .size
          taskSection.tasks.toList.flatMap { task =>
            val taskFieldCount = task.sections.toList
              .flatMap(
                _.fold(page => page.page.allFieldsNested)(page => page.page.allFieldsNested)(page =>
                  page.pages.toList.flatMap(_.allFieldsNested)
                )
              )
              .filterNot(ignoredComponent)
              .size
            task.sections.toList.flatMap { section =>
              section.fold { nonRepeatingPage =>
                val page = nonRepeatingPage.page
                val allFields = page.allFieldsNested.filterNot(ignoredComponent)
                val pageFieldCount = allFields.size
                allFields.map { field =>
                  FormTemplateDetail(
                    FormTemplateDetailRow(sectionTitle, sectionFieldCount),
                    FormTemplateDetailRow(task.title, taskFieldCount),
                    page.shortName,
                    FormTemplateDetailRow(page.title, pageFieldCount),
                    page.includeIf,
                    field.id,
                    field.mandatory.eval(booleanExprResolver),
                    field.shortName.getOrElse(field.label),
                    field.showFormat,
                    showFormatExample(field),
                    getMin(field),
                    getMax(field),
                    getValue(field),
                    field.includeIf,
                    false,
                    getSpecimenLink(field)
                  )
                }
              } { repeatingPage =>
                val page = repeatingPage.page
                val allFields = page.allFieldsNested.filterNot(ignoredComponent)
                val pageFieldCount = allFields.size
                allFields.map(field =>
                  FormTemplateDetail(
                    FormTemplateDetailRow(sectionTitle, sectionFieldCount),
                    FormTemplateDetailRow(task.title, taskFieldCount),
                    page.shortName,
                    FormTemplateDetailRow(page.title, pageFieldCount),
                    page.includeIf,
                    field.id,
                    field.mandatory.eval(booleanExprResolver),
                    field.shortName.getOrElse(field.label),
                    field.showFormat,
                    showFormatExample(field),
                    getMin(field),
                    getMax(field),
                    getValue(field),
                    field.includeIf,
                    false,
                    getSpecimenLink(field)
                  )
                )
              } { atl =>
                atl.pages.toList.flatMap { page =>
                  val allFields = page.allFieldsNested.filterNot(ignoredComponent)
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
                        field.mandatory.eval(booleanExprResolver),
                        field.shortName.getOrElse(field.label),
                        field.showFormat,
                        showFormatExample(field),
                        getMin(field),
                        getMax(field),
                        getValue(field),
                        field.includeIf,
                        true,
                        getSpecimenLink(field)
                      )
                    )
                }
              }
            }
          }
        }
      }
  }

  private def makeReportTableRows(
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    isCsv: Boolean
  )(implicit lang: LangADT, sse: SmartStringEvaluator) = {
    val formTemplateDetails = extractFormTemplate(cache, formModelOptics, isCsv)

    val tableRows = formTemplateDetails.map { row =>
      List(
        Some(
          ReportTableRow(
            content = Text(evalSmartString(row.sectionTitle.value)),
            rowspan = Some(row.sectionTitle.rowSpan)
          )
        ),
        Some(
          ReportTableRow(
            content = Text(evalSmartString(row.taskTitle.value)),
            rowspan = Some(row.taskTitle.rowSpan)
          )
        ),
        Some(
          ReportTableRow(
            content = Text(row.shortName.map(evalSmartString).getOrElse("")),
            rowspan = Some(row.pageTitle.rowSpan)
          )
        ),
        Some(
          ReportTableRow(
            content = Text(evalSmartString(row.pageTitle.value)),
            rowspan = Some(row.pageTitle.rowSpan)
          )
        ),
        Some(
          ReportTableRow(
            content = Text(row.pageCondition.map(cond => s"$${${cond.booleanExpr.prettyPrint}}").getOrElse("")),
            rowspan = Some(row.pageTitle.rowSpan)
          )
        ),
        Some(ReportTableRow(content = Text(row.fieldId.value))),
        Some(ReportTableRow(content = Text(if (row.fieldMandatory) "Y" else "N"))),
        Some(ReportTableRow(content = Text(evalSmartString(row.fieldLabel)))),
        Some(ReportTableRow(content = Text(row.fieldFormat))),
        Some(ReportTableRow(content = Text(row.fieldFormatExample))),
        Some(ReportTableRow(content = Text(row.fieldMin.getOrElse("")))),
        Some(ReportTableRow(content = Text(row.fieldMax.getOrElse("")))),
        Some(ReportTableRow(content = Text(row.fieldValueSource.getOrElse("")))),
        Some(
          ReportTableRow(content =
            Text(row.fieldCondition.map(cond => s"$${${cond.booleanExpr.prettyPrint}}").getOrElse(""))
          )
        ),
        Some(ReportTableRow(content = Text(if (row.fieldRepeats) "Y" else "N"))),
        Some(ReportTableRow(content = HtmlContent(row.fieldSpecimenLink.getOrElse("Unknown"))))
      )
    }

    normaliseTableRows(tableRows)
  }

  def extract(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, accessCode, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        val header: List[HeadCell] = headers.map(header =>
          HeadCell(
            content = Text(header)
          )
        )

        val tableRows: List[List[Option[TableRow]]] =
          makeReportTableRows(cache, formModelOptics, false).map(
            _.map(_.map(row => TableRow(content = row.content, rowspan = row.rowspan)))
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

  def exportToCSV(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, accessCode, OperationWithForm.EditForm) {
      _ => implicit lang => cache => implicit sse => formModelOptics =>
        val csvContent = (headers +: makeReportTableRows(cache, formModelOptics, true).map(
          _.map(_.map { row =>
            row.content match {
              case HtmlContent(value) => s""""${value.toString}""""
              case Text(value)        => s""""$value""""
              case _                  => "\"\""
            }
          }.getOrElse("\"\""))
        ))
          .map(_.mkString(","))
          .mkString("\n")

        Ok(csvContent)
          .as("text/csv")
          .withHeaders(
            "Content-Disposition" -> s"attachment; filename=${formTemplateId.value}-details.csv"
          )
          .pure[Future]
    }
}
