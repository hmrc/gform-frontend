/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.bulkupload

import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import play.api.data.{ Form, FormError, Forms }
import play.api.data.format.Formatter
import play.api.http.HttpEntity
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, Call, MessagesControllerComponents, Request }
import play.twirl.api.Html
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.bulkupload.BulkUploadConstant._
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.gform.FastForwardService
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.{ FastForward, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BulkUpload, ColumnType, FormComponentId, FormTemplateId, SectionNumber, SectionTitle4Ga, SpreadsheetColumn, SuppressErrors }
import uk.gov.hmrc.gform.validation.FieldOk
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.views.html.bulkupload

class BulkUploadController(
  auth: AuthenticatedRequestActions,
  bulkUploadService: BulkUploadService[Future],
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  messagesControllerComponents: MessagesControllerComponents,
  formControllerRequestHandler: FormControllerRequestHandler,
  fastForwardService: FastForwardService,
  gformConnector: GformConnector
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  private val xlsxContentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

  def downloadSpreadsheet(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => l => cache => sse => formModelOptics =>
        val (_, bulkUpload, _) = formModelOptics.formModelVisibilityOptics.getBulkUploadFor(formComponentId)

        gformConnector.downloadBulkUploadSpreadsheet(formTemplateId, formComponentId, bulkUpload).map { response =>
          val result = Status(response.status).sendEntity(
            HttpEntity.Streamed(
              response.bodyAsSource,
              response.header(CONTENT_LENGTH).map(_.toLong),
              Some(xlsxContentType)
            )
          )
          response.header(CONTENT_DISPOSITION).fold(result)(cd => result.withHeaders(CONTENT_DISPOSITION -> cd))
        }
    }

  def confirmUploadedData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => sse => formModelOptics =>
        fastForwardService
          .redirectFastForward[SectionSelectorType.Normal](
            cache,
            maybeAccessCode,
            formModelOptics,
            Some(sectionNumber),
            SuppressErrors.Yes,
            fastForward
          )
    }

  def renderUploadedData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        val (formComponent, bulkUpload, caption) =
          formModelOptics.formModelVisibilityOptics.getBulkUploadFor(formComponentId)

        val data: List[List[String]] =
          cache.form.thirdPartyData.bulkUploadLookup
            .flatMap(_.get(formComponentId))
            .fold(List.empty[List[String]])(_.merge())

        val validationErrors = BulkUploadValidator.validate(bulkUpload, data)

        if (validationErrors.isEmpty) {
          val confirmDataPage =
            new ConfirmDataPage(formComponent, FieldOk(formComponent, ""), data, bulkUpload)
          val formAction: Call = uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.confirmUploadedData(
            cache.formTemplate._id,
            maybeAccessCode,
            formComponentId,
            sectionNumber,
            fastForward
          )
          val backHref: Call =
            goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode, fastForward)
          val result = bulkupload.confirm_data(
            caption.map(_.value()),
            cache.formTemplate,
            frontendAppConfig,
            confirmDataPage,
            formAction,
            backHref,
            maybeAccessCode
          )
          Future.successful(Ok(result))
        } else {
          val errorsPage = new BulkUploadErrorsPage(
            validationErrors,
            data.size,
            bulkUpload,
            formTemplateId,
            maybeAccessCode,
            formComponentId,
            sectionNumber,
            fastForward
          )
          val formAction: Call = uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.confirmUploadedData(
            cache.formTemplate._id,
            maybeAccessCode,
            formComponentId,
            sectionNumber,
            fastForward
          )
          val backHref: Call =
            goToSectionNumberLink(cache, formModelOptics, sectionNumber, maybeAccessCode, fastForward)
          val result = bulkupload.confirm_data_errors(
            caption.map(_.value()),
            cache.formTemplate,
            frontendAppConfig,
            errorsPage,
            formAction,
            backHref,
            maybeAccessCode
          )
          Future.successful(Ok(result))
        }

    }

  private def renderChangeRowPage(
    dataToFix: String,
    textForm: Form[String],
    bigDecimalForm: Form[BigDecimal],
    dateForm: Form[BulkUploadDate],
    formModelOptics: FormModelOptics,
    cache: AuthCacheWithForm,
    formComponentId: FormComponentId,
    columnIndex: ColumnIndex,
    rowIndex: RowIndex,
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    fastForward: List[FastForward]
  )(implicit request: Request[_], l: LangADT, sse: SmartStringEvaluator): Html = {
    val (formComponent, bulkUpload, caption) =
      formModelOptics.formModelVisibilityOptics.getBulkUploadFor(formComponentId)

    val column: SpreadsheetColumn = bulkUpload.columnFor(columnIndex)

    val backHref: Call = uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.renderUploadedData(
      cache.formTemplate._id,
      maybeAccessCode,
      formComponentId,
      sectionNumber,
      fastForward
    )

    val formAction: Call = uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.changeRow(
      cache.formTemplate._id,
      maybeAccessCode,
      formComponentId,
      sectionNumber,
      columnIndex,
      rowIndex,
      fastForward
    )

    val editPage = new EditPage(formComponent, bulkUpload, dataToFix, column, textForm, bigDecimalForm, dateForm)

    bulkupload.confirm_edit(
      editPage,
      caption.map(_.value()),
      cache.formTemplate,
      frontendAppConfig,
      formAction,
      backHref,
      maybeAccessCode
    )
  }

  def changeRow(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    columnIndex: ColumnIndex,
    rowIndex: RowIndex,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        val (formComponent, bulkUpload, caption) =
          formModelOptics.formModelVisibilityOptics.getBulkUploadFor(formComponentId)

        val column: SpreadsheetColumn = bulkUpload.columnFor(columnIndex)

        column.`type` match {
          case ColumnType.Text =>
            editTextForm
              .bindFromRequest()
              .fold(
                errorForm => {
                  val dataToFix = errorForm.data.getOrElse(BULK_UPLOAD_TEXT, "")
                  val result = renderChangeRowPage(
                    dataToFix,
                    errorForm,
                    editBigDecimalForm,
                    editDateForm,
                    formModelOptics,
                    cache,
                    formComponentId,
                    columnIndex,
                    rowIndex,
                    sectionNumber,
                    maybeAccessCode,
                    fastForward
                  )
                  Future.successful(Ok(result))
                },
                validText =>
                  bulkUploadService
                    .changeCell(validText, columnIndex, rowIndex, formComponentId, cache.form, maybeAccessCode)
                    .map(_ =>
                      Redirect(
                        uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.renderUploadedData(
                          cache.formTemplate._id,
                          maybeAccessCode,
                          formComponentId,
                          sectionNumber,
                          fastForward
                        )
                      )
                    )
              )
          case ColumnType.Date =>
            editDateForm
              .bindFromRequest()
              .fold(
                errorForm => {

                  val day = errorForm.data.getOrElse(BULK_UPLOAD_DAY, "")
                  val month = errorForm.data.getOrElse(BULK_UPLOAD_MONTH, "")
                  val year = errorForm.data.getOrElse(BULK_UPLOAD_YEAR, "")

                  val dataToFix = day + "/" + month + "/" + year

                  val result = renderChangeRowPage(
                    dataToFix,
                    editTextForm,
                    editBigDecimalForm,
                    errorForm,
                    formModelOptics,
                    cache,
                    formComponentId,
                    columnIndex,
                    rowIndex,
                    sectionNumber,
                    maybeAccessCode,
                    fastForward
                  )

                  Future.successful(Ok(result))
                },
                validDate => {
                  val validData: String = validDate.toLocalDate.format(bulkUploadDateFormatter)

                  bulkUploadService
                    .changeCell(validData, columnIndex, rowIndex, formComponentId, cache.form, maybeAccessCode)
                    .map(_ =>
                      Redirect(
                        uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.renderUploadedData(
                          cache.formTemplate._id,
                          maybeAccessCode,
                          formComponentId,
                          sectionNumber,
                          fastForward
                        )
                      )
                    )
                }
              )
          case ColumnType.Number =>
            val validated: Validated[(Form[BigDecimal], String), String] = editBigDecimalForm
              .bindFromRequest()
              .fold(
                errorForm => {
                  val currentValue = errorForm.data.getOrElse(BULK_UPLOAD_NUMBER, "")
                  if (!column.mandatory && currentValue.trim.isEmpty) Valid("") else Invalid(errorForm -> currentValue)
                },
                validBigDecimal => Valid(validBigDecimal.bigDecimal.toPlainString)
              )

            validated match {
              case Valid(value) =>
                bulkUploadService
                  .changeCell(
                    value,
                    columnIndex,
                    rowIndex,
                    formComponentId,
                    cache.form,
                    maybeAccessCode
                  )
                  .map(_ =>
                    Redirect(
                      uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.renderUploadedData(
                        cache.formTemplate._id,
                        maybeAccessCode,
                        formComponentId,
                        sectionNumber,
                        fastForward
                      )
                    )
                  )
              case Invalid((errorForm, value)) =>
                val result = renderChangeRowPage(
                  value,
                  editTextForm,
                  errorForm,
                  editDateForm,
                  formModelOptics,
                  cache,
                  formComponentId,
                  columnIndex,
                  rowIndex,
                  sectionNumber,
                  maybeAccessCode,
                  fastForward
                )
                Future.successful(Ok(result))
            }
        }
    }

  val editTextForm: Form[String] = Form(
    Forms.single(
      BULK_UPLOAD_TEXT -> Forms.nonEmptyText
    )
  )

  val editBigDecimalForm: Form[BigDecimal] = Form(
    Forms.single(
      BULK_UPLOAD_NUMBER -> Forms.bigDecimal
    )
  )

  // Reports a non-existent date (e.g. 31/02/2026) against the day field, a plain verifying would yield a global error
  private val editDateFormatter: Formatter[BulkUploadDate] = new Formatter[BulkUploadDate] {
    private val mapping = Forms.mapping(
      BULK_UPLOAD_DAY   -> Forms.number(min = 1, max = 31),
      BULK_UPLOAD_MONTH -> Forms.number(min = 1, max = 12),
      BULK_UPLOAD_YEAR  -> Forms.number(min = 1000, max = 9999)
    )(BulkUploadDate.apply)(BulkUploadDate.unapply)

    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], BulkUploadDate] =
      mapping
        .bind(data)
        .filterOrElse(date => Try(date.toLocalDate).isSuccess, Seq(FormError(BULK_UPLOAD_DAY, "error.max")))

    override def unbind(key: String, value: BulkUploadDate): Map[String, String] = mapping.unbind(value)
  }

  val editDateForm: Form[BulkUploadDate] = Form(Forms.of(editDateFormatter))

  def changeRowPage(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    columnIndex: ColumnIndex,
    rowIndex: RowIndex,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        val dataToFix = cache.form.thirdPartyData.bulkUploadForCell(formComponentId, columnIndex, rowIndex)

        val result = renderChangeRowPage(
          dataToFix,
          editTextForm,
          editBigDecimalForm,
          editDateForm,
          formModelOptics,
          cache,
          formComponentId,
          columnIndex,
          rowIndex,
          sectionNumber,
          maybeAccessCode,
          fastForward
        )

        Future.successful(Ok(result))
    }

  private val confirmRowRemoveForm: Form[String] = Form(
    Forms.single(
      BULK_UPLOAD_REMOVE_ROW -> Forms.nonEmptyText
    )
  )

  def removeRow(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    rowIndex: RowIndex,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        confirmRowRemoveForm
          .bindFromRequest()
          .fold(
            errorForm =>
              Future.successful(
                Ok(
                  renderRemoveRowPage(
                    formModelOptics,
                    rowIndex,
                    formComponentId,
                    cache,
                    maybeAccessCode,
                    sectionNumber,
                    fastForward,
                    errorForm
                  )
                )
              ),
            removeDecision =>
              removeDecision match {
                case "no" =>
                  Future.successful(
                    Redirect(
                      uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.renderUploadedData(
                        cache.formTemplate._id,
                        maybeAccessCode,
                        formComponentId,
                        sectionNumber,
                        fastForward
                      )
                    )
                  )
                case "yes" =>
                  val (_, bulkUpload, _) = formModelOptics.formModelVisibilityOptics.getBulkUploadFor(formComponentId)
                  val primaryColumnData = getPrimaryRowData(bulkUpload, rowIndex, formComponentId, cache)
                  bulkUploadService
                    .removeRow(rowIndex, formComponentId, cache.form, maybeAccessCode)
                    .map(_ =>
                      Redirect(
                        uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.renderUploadedData(
                          cache.formTemplate._id,
                          maybeAccessCode,
                          formComponentId,
                          sectionNumber,
                          fastForward
                        )
                      ).flashing("success" -> primaryColumnData)
                    )
                case unknown =>
                  throw new Exception(
                    s"Error when removing row '$rowIndex' from '$formComponentId' bulkUpload - expected 'yes' or 'no', but got '$unknown'"
                  )
              }
          )

    }

  private def getPrimaryRowData(
    bulkUpload: BulkUpload,
    rowIndex: RowIndex,
    formComponentId: FormComponentId,
    cache: AuthCacheWithForm
  ): String = {
    val primaryIndex = bulkUpload.spreadsheet.indexWhere(_.isPrimary())

    cache.form.thirdPartyData.bulkUploadForCell(formComponentId, ColumnIndex(primaryIndex), rowIndex)
  }

  def askConfirmationToRemoveRow(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    rowIndex: RowIndex,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        Future.successful(
          Ok(
            renderRemoveRowPage(
              formModelOptics,
              rowIndex,
              formComponentId,
              cache,
              maybeAccessCode,
              sectionNumber,
              fastForward,
              confirmRowRemoveForm
            )
          )
        )
    }

  private def renderRemoveRowPage(
    formModelOptics: FormModelOptics,
    rowIndex: RowIndex,
    formComponentId: FormComponentId,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    fastForward: List[FastForward],
    form: Form[String]
  )(implicit request: Request[_], l: LangADT, sse: SmartStringEvaluator): Html = {
    val (_, bulkUpload, caption) = formModelOptics.formModelVisibilityOptics.getBulkUploadFor(formComponentId)
    val primaryColumnData = getPrimaryRowData(bulkUpload, rowIndex, formComponentId, cache)

    val backHref: Call = uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.renderUploadedData(
      cache.formTemplate._id,
      maybeAccessCode,
      formComponentId,
      sectionNumber,
      fastForward
    )

    val formAction: Call = uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController.removeRow(
      cache.formTemplate._id,
      maybeAccessCode,
      formComponentId,
      sectionNumber,
      rowIndex,
      fastForward
    )

    val confirmRemovePage = new ConfirmRemovePage(form)

    bulkupload.confirm_remove_row(
      request.messages.messages("bulk.upload.row.remove.confirmation", primaryColumnData),
      caption.map(_.value()),
      cache.formTemplate,
      frontendAppConfig,
      confirmRemovePage,
      formAction,
      backHref,
      maybeAccessCode
    )
  }

  private def goToSectionNumberLink(
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics,
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    fastForward: List[FastForward]
  )(implicit sse: SmartStringEvaluator): Call = {

    val sectionTitle4Ga = SectionTitle4Ga.sectionTitle4GaFactory(
      formModelOptics.formModelVisibilityOptics.formModel(sectionNumber),
      sectionNumber
    )

    uk.gov.hmrc.gform.gform.routes.FormController.form(
      cache.formTemplate._id,
      maybeAccessCode,
      sectionNumber,
      sectionTitle4Ga,
      SuppressErrors.Yes,
      fastForward
    )
  }

}
