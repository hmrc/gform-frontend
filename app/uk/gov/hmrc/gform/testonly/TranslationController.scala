/*
 * Copyright 2025 HM Revenue & Customs
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

import kantan.csv._
import kantan.csv.ops._
import play.api.i18n.I18nSupport
import play.api.mvc.MessagesControllerComponents
import play.twirl.api.HtmlFormat
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.{ HeadCell, Table }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.views.html.debug.{ dropzone, orderedList, translation }
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.TableRow

class TranslationController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  frontendAppConfig: FrontendAppConfig,
  gformConnector: GformConnector,
  messagesControllerComponents: MessagesControllerComponents
)(implicit
  ec: ExecutionContext
) extends FrontendController(messagesControllerComponents) {

  type Csv = (String, String)
  type CsvInternal = (String, String, String)
  private val headers = List(
    "en",
    "cy"
  )

  private val headersInternal = List(
    "path",
    "en",
    "cy"
  )

  private def rowCell(content: String): TableRow =
    TableRow(
      content = Text(content),
      classes = "govuk-!-font-size-16"
    )
  private def rowCellNumeric(content: Int): TableRow =
    TableRow(
      content = Text(content.toString + "."),
      classes = "govuk-!-font-size-16 govuk-table__cell--numeric"
    )

  def translationQuick(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]) =
    auth.async[SectionSelectorType.Normal](formTemplateId, accessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import i18nSupport._

        val translationAuditF = gformConnector.translationAudit(formTemplateId)

        for {
          translationAudit <- translationAuditF
        } yield {

          val downloadUntranslatedEnOnly = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
            "Download translation xlsx",
            uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .proxyToGform(s"/gform/translation-excel/${formTemplateId.value}/brief?allEnglish=false")
          )

          val downloadAllEn = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
            "Download all english xlsx",
            uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .proxyToGform(s"/gform/translation-excel/${formTemplateId.value}/brief?allEnglish=true")
          )

          val dropzoneHtml = dropzone(translationAudit)

          val govukTabs = new GovukTabs()(
            Tabs(
              items = Seq(
                TabItem(
                  id = Some("upload-translation"),
                  label = "Upload translation",
                  panel = TabPanel(
                    content = HtmlContent(
                      HtmlFormat.fill(
                        List(downloadUntranslatedEnOnly, HtmlFormat.raw("<br>"), downloadAllEn, dropzoneHtml)
                      )
                    )
                  )
                )
              )
            )
          )

          Ok(translation(cache.formTemplate, accessCode, frontendAppConfig, govukTabs))
        }
    }

  def translationDebug(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]) =
    auth.async[SectionSelectorType.Normal](formTemplateId, accessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import i18nSupport._

        val csvDataF = gformConnector.translationCsv(formTemplateId)
        val csvBriefDataF = gformConnector.translationCsvBrief(formTemplateId)
        val csvInternalDataF = gformConnector.translationCsvInternal(formTemplateId)
        val enTextBreakdownF = gformConnector.translationEnTextBreakdown(formTemplateId)
        val translationAuditF = gformConnector.translationAudit(formTemplateId)

        for {
          translationAudit <- translationAuditF
          csvData          <- csvDataF
          csvBriefData     <- csvBriefDataF
          csvInternalData  <- csvInternalDataF
          enTextBreakdown  <- enTextBreakdownF
        } yield {

          val iteratorAll: CsvReader[ReadResult[Csv]] = csvData.body.asCsvReader[Csv](rfc.withHeader)
          val iteratorBrief: CsvReader[ReadResult[String]] = csvBriefData.body.asCsvReader[String](rfc.withoutHeader)
          val iteratorInternal: CsvReader[ReadResult[CsvInternal]] =
            csvInternalData.body.asCsvReader[CsvInternal](rfc.withHeader)

          val tableAllRows: List[List[TableRow]] =
            iteratorAll.collect { case Right((en, cy)) =>
              List(
                rowCell(en),
                rowCell(cy)
              )
            }.toList

          val tableBriefRows: List[List[TableRow]] =
            iteratorBrief.zipWithIndex.collect { case (Right(en), index) =>
              List(
                rowCellNumeric(index + 1),
                rowCell(en)
              )
            }.toList

          val tableInternalRows: List[List[TableRow]] =
            iteratorInternal.collect { case Right((path, en, cy)) =>
              List(
                rowCell(path),
                rowCell(en),
                rowCell(cy)
              )
            }.toList

          val breakdownRows: List[List[TableRow]] =
            enTextBreakdown.breakdowns.map { breakdown =>
              List(
                rowCell(breakdown.original),
                TableRow(
                  content = HtmlContent(orderedList(breakdown.breakdown)),
                  classes = "govuk-!-font-size-16"
                )
              )
            }.toList

          val header: List[HeadCell] = headers.map(header =>
            HeadCell(
              content = Text(header)
            )
          )

          val downloadFormTranslations = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
            "Download as xlsx",
            uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .proxyToGform(s"/gform/translation-excel/${formTemplateId.value}")
          )

          def stats(rowsCount: Int) = uk.gov.hmrc.gform.views.html.hardcoded.pages.p(
            s"Number of rows $rowsCount"
          )

          val allStats = stats(tableAllRows.size)
          val briefStats = stats(tableBriefRows.size)
          val internalStats = stats(tableInternalRows.size)

          val breakdownStats = uk.gov.hmrc.gform.views.html.hardcoded.pages.p(
            s"Number of items ${enTextBreakdown.breakdowns.size}"
          )

          val table = Table(
            head = Some(header),
            rows = tableAllRows
          )

          val downloadUntranslatedEnOnly = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
            "Download as xlsx",
            uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .proxyToGform(s"/gform/translation-excel/${formTemplateId.value}/brief?allEnglish=false")
          )

          val tableBrief = Table(
            rows = tableBriefRows
          )

          val headerInternal: List[HeadCell] = headersInternal.map(header =>
            HeadCell(
              content = Text(header)
            )
          )

          val tableInternal = Table(
            head = Some(headerInternal),
            rows = tableInternalRows
          )

          val breakdownTable = Table(
            head = Some(
              List(
                HeadCell(
                  content = Text("original")
                ),
                HeadCell(
                  content = Text("breakdowns")
                )
              )
            ),
            rows = breakdownRows
          )

          val dropzoneHtml = dropzone(translationAudit)

          val htmlTable = HtmlFormat.fill(List(downloadFormTranslations, allStats, new GovukTable()(table)))
          val htmlBriefTable =
            HtmlFormat.fill(List(downloadUntranslatedEnOnly, briefStats, new GovukTable()(tableBrief)))
          val htmlInternalTable = HtmlFormat.fill(List(internalStats, new GovukTable()(tableInternal)))

          val htmlBreakdownTable = HtmlFormat.fill(List(breakdownStats, new GovukTable()(breakdownTable)))

          val govukTabs = new GovukTabs()(
            Tabs(
              items = Seq(
                TabItem(
                  id = Some("upload-translation"),
                  label = "Upload translation",
                  panel = TabPanel(
                    content = HtmlContent(dropzoneHtml)
                  )
                ),
                TabItem(
                  id = Some("untranslated-entries"),
                  label = "Untranslated entries",
                  panel = TabPanel(
                    content = HtmlContent(htmlBriefTable)
                  )
                ),
                TabItem(
                  id = Some("all-entries"),
                  label = "All entries",
                  panel = TabPanel(
                    content = HtmlContent(htmlTable)
                  )
                ),
                TabItem(
                  id = Some("internal"),
                  label = "Internal",
                  panel = TabPanel(
                    content = HtmlContent(htmlInternalTable)
                  )
                ),
                TabItem(
                  id = Some("breakdown"),
                  label = "Breakdown",
                  panel = TabPanel(
                    content = HtmlContent(htmlBreakdownTable)
                  )
                )
              )
            )
          )

          Ok(translation(cache.formTemplate, accessCode, frontendAppConfig, govukTabs))
        }

    }
}
