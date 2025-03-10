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

package uk.gov.hmrc.gform.gformbackend

import cats.data.NonEmptyList
import cats.implicits.none
import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.show._
import org.apache.commons.text.StringEscapeUtils
import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.slf4j.LoggerFactory
import play.api.libs.json.{ JsString, JsValue, Json }
import uk.gov.hmrc.crypto.Crypted
import uk.gov.hmrc.gform.gform.{ CustomerId, DataRetrieveConnectorBlueprint }
import uk.gov.hmrc.gform.objectStore.Envelope
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.email.ConfirmationCodeWithEmailService
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.retrieval.FormAuthRetrievals
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.testonly.snapshot._
import uk.gov.hmrc.gform.testonly.{ EnTextBreakdowns, ExpressionsLookup }
import uk.gov.hmrc.gform.upscan.{ UpscanConfirmation, UpscanReference }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HttpReads.Implicits.readFromJson
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpReadsInstances, HttpResponse, UpstreamErrorResponse }

import scala.concurrent.{ ExecutionContext, Future }

class GformConnector(ws: WSHttp, baseUrl: String) {

  private val logger = LoggerFactory.getLogger(getClass)

  implicit val legacyRawReads: HttpReads[HttpResponse] =
    HttpReadsInstances.throwOnFailure(HttpReadsInstances.readEitherOf(HttpReadsInstances.readRaw))

  /** ****form******
    */
  //TODO: remove userId since this information will be passed using HeaderCarrier
  def newForm(
    formTemplateId: FormTemplateId,
    userId: UserId,
    affinityGroup: Option[AffinityGroup],
    queryParams: QueryParams
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormIdData] = {
    val ag = affinityGroup.map(a => "/" + AffinityGroupUtil.affinityGroupName(a)).getOrElse("")
    ws.POST[QueryParams, FormIdData](s"$baseUrl/new-form/${formTemplateId.value}/${userId.value}$ag", queryParams)
  }

  def getAllForms(userId: UserId, formTemplateId: FormTemplateId)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[List[FormOverview]] =
    ws.GET[List[FormOverview]](s"$baseUrl/forms/all/${userId.value}/${formTemplateId.value}")

  def getForm(formIdData: FormIdData)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Form] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/${accessCode.value}"
      }
    ws.GET[Form](url)
  }

  private def maybeForm(formIdData: FormIdData)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Option[Form]] = getForm(formIdData).map(Some(_)).recoverWith {
    case UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode === StatusCodes.NotFound.intValue =>
      Option.empty[Form].pure[Future]
  }

  def maybeForm(formIdData: FormIdData, formTemplate: FormTemplate)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Option[Form]] =
    getForm(formIdData)
      .flatMap { form =>
        if (form.formTemplateVersion.contains(formTemplate.version)) {
          Some(form).pure[Future]
        } else if (allowNonVersionedFormIfTemplateIsOnFirstVersion(formTemplate, form)) {
          Some(form).pure[Future]
        } else {
          logger.info(
            s"FormTemplate ${formTemplate._id.value} version and Form version mismatch. FormTemplate version: ${formTemplate.version}, Form version: ${form.formTemplateVersion}"
          )
          getFormByLegacyFormTemplate(formTemplate: FormTemplate, form: Form, formIdData: FormIdData)
        }
      }
      .recoverWith {
        case UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode === StatusCodes.NotFound.intValue =>
          formTemplate.legacyFormIds.fold(Future.successful(none[Form]))(getFormByLegacyIds(formIdData))
      }

  // Once all "forms" contains version this can be dropped
  // GFORMS-2314 introduced template version to be mandatory. Ultimately all
  // user form data will contain version. At that point this check will be no-op
  private def allowNonVersionedFormIfTemplateIsOnFirstVersion(formTemplate: FormTemplate, form: Form): Boolean =
    formTemplate.version === FormTemplateVersion(1) && form.formTemplateVersion.isEmpty

  private def getFormByLegacyFormTemplate(
    formTemplate: FormTemplate,
    form: Form,
    formIdData: FormIdData
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Option[Form]] =
    formTemplate.legacyFormIds.fold(Option.empty[Form].pure[Future]) { legacyFormIds =>
      val legacyFormTemplateId = legacyFormIds.head
      logger.info(
        s"FormTemplate ${formTemplate._id.value} checking legacy FormTemplateId: ${legacyFormTemplateId.value}"
      )
      maybeFormTemplate(legacyFormTemplateId).flatMap {
        case Some(legacyFormTemplate) =>
          logger.info(s"Legacy FormTemplate ${legacyFormTemplateId.value} exists. Trying to load legacy form.")
          val legacyFormIdData = formIdData.withTemplateId(legacyFormTemplateId)
          maybeForm(legacyFormIdData).flatMap {
            case None =>
              if (form.formTemplateVersion.contains(legacyFormTemplate.version)) {
                if (form.status === Submitted) {
                  logger.info(
                    s"Legacy form for FormTemplate ${legacyFormTemplateId.value} not found, but form status is Submitted"
                  )
                  Some(
                    form.copy(
                      formTemplateId = legacyFormTemplateId
                    )
                  ).pure[Future]
                } else {
                  logger.info(
                    s"Legacy form for FormTemplate ${legacyFormTemplateId.value} not found, transferring form data..."
                  )
                  changeFormTemplateIdVersion(formIdData, legacyFormTemplateId).map { newForm =>
                    logger.info(
                      s"Form data transferred from ${formIdData.formTemplateId.value} to ${legacyFormTemplateId.value}"
                    )
                    Some(newForm)
                  }
                }
              } else {
                getFormByLegacyFormTemplate(legacyFormTemplate, form, formIdData)
              }
            case Some(form) =>
              logger.info(
                s"Legacy form for FormTemplate ${legacyFormTemplateId.value} found (status = ${form.status})"
              )
              Some(form).pure[Future]
          }
        case None =>
          logger.info(
            s"Legacy FormTemplate: ${legacyFormTemplateId.value} do not exists. Trying to resolve legacyFormIds: $legacyFormIds"
          )
          getFormByLegacyIds(formIdData)(legacyFormIds).map {
            case Some(form) if form.formTemplateVersion.contains(formTemplate.version) => Some(form)
            case _                                                                     => None
          }
      }
    }

  def getFormByLegacyIds(
    formIdData: FormIdData
  )(legacyIds: NonEmptyList[FormTemplateId])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Form]] = {
    val NonEmptyList(legacyFormId, tail) = legacyIds
    val legacyFormIdData = formIdData.withTemplateId(legacyFormId)
    logger.info(
      s"Attempt to access form $formIdData, but form not found in MongoDB, attempt to look for $legacyFormIdData as a fallback."
    )

    getForm(legacyFormIdData)
      .flatMap { _ =>
        logger.info(
          s"Attempt to access form $formIdData, but form not found in MongoDB, attempt to look for $legacyFormIdData as a fallback succeeded and form saved."
        )

        createFormFromLegacy(legacyFormIdData, formIdData).map(Some(_))
      }
      .recoverWith {
        case UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode === StatusCodes.NotFound.intValue =>
          NonEmptyList
            .fromList(tail)
            .map(getFormByLegacyIds(formIdData))
            .getOrElse(Future.successful(None))
      }
  }

  def updateUserData(formIdData: FormIdData, userData: UserData)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/${accessCode.value}"
      }
    ws.PUT[UserData, HttpResponse](url, userData).void
  }

  def changeFormTemplateIdVersion(formIdData: FormIdData, formTemplateId: FormTemplateId)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Form] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/version/${userId.value}/${formTemplateId.value}"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/version/${userId.value}/${formTemplateId.value}/${accessCode.value}"
      }
    ws.PUT[FormTemplateId, Form](url, formTemplateId)
  }

  private def createFormFromLegacy(formIdData: FormIdData, newFormId: FormIdData)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Form] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/from-legacy"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/${accessCode.value}/from-legacy"
      }
    ws.POST[FormIdData, Form](url, newFormId)
  }

  def forceUpdateFormStatus(formId: FormIdData, status: FormStatus)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] =
    ws.PUT[String, HttpResponse](s"$baseUrl/formBundles/${urlFragment(formId)}/$status/forceStatus", "").void

  def deleteForm(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.POSTEmpty[HttpResponse](baseUrl + s"/forms/${formId.value}/delete").void

  /** ****submission******
    */
  def createSubmission(
    formId: FormId,
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId,
    customerId: String,
    noOfAttachments: Int
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Submission] =
    ws.POST[String, Submission](
      baseUrl + s"/forms/${formId.value}/${formTemplateId.value}/${envelopeId.value}/$noOfAttachments/createSubmission",
      "",
      Seq(("customerId", StringEscapeUtils.escapeHtml4(customerId)))
    )

  def submitForm(
    formIdData: FormIdData,
    customerId: CustomerId,
    submissionData: SubmissionData,
    affinityGroup: Option[AffinityGroup]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/submitForm"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/${accessCode.value}/submitForm"
      }
    mkPost(customerId, submissionData, affinityGroup)(url)
  }

  /** ****test-only******
    */

  def getExpressions(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ExpressionsLookup] =
    ws.GET[ExpressionsLookup](s"$baseUrl/test-only/expressions/${formTemplateId.value}")

  def renderHandlebarPayload(
    formTemplateId: FormTemplateId,
    formId: FormId,
    destinationId: DestinationId,
    customerId: CustomerId,
    submissionData: SubmissionData,
    affinityGroup: Option[AffinityGroup]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    mkPost(customerId, submissionData, affinityGroup)(
      s"$baseUrl/test-only/${formTemplateId.value}/${formId.value}/${destinationId.id}"
    )

  def renderHandlebarModel(
    formTemplateId: FormTemplateId,
    formId: FormId,
    customerId: CustomerId,
    submissionData: SubmissionData,
    affinityGroup: Option[AffinityGroup]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    mkPost(customerId, submissionData, affinityGroup)(
      s"$baseUrl/test-only/handlebars-model/${formTemplateId.value}/${formId.value}"
    )

  def handlebarPayloadSource(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[String] = {
    val url = s"$baseUrl/handlebarstemplates/${formTemplateId.value}-${destinationId.id}/raw"
    ws.GET[HttpResponse](url)(HttpReads.Implicits.readRaw, hc, ec).map(_.body)
  }

  private def mkPost(customerId: CustomerId, submissionData: SubmissionData, affinityGroup: Option[AffinityGroup])(
    url: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    ws.POST[SubmissionData, HttpResponse](
      url,
      submissionData,
      Seq(
        "customerId"    -> StringEscapeUtils.escapeHtml4(customerId.id),
        "affinityGroup" -> affinityGroupNameO(affinityGroup)
      )
    )

  def submissionDetails(
    formIdData: FormIdData,
    envelopeId: EnvelopeId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Submission] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/submissionDetails/${userId.value}/${formTemplateId.value}/${envelopeId.value}"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/submissionDetails/${userId.value}/${formTemplateId.value}/${accessCode.value}/${envelopeId.value}"
      }
    ws.GET[Submission](url)
  }

  def maybeSubmissionDetails(
    formIdData: FormIdData,
    envelopeId: EnvelopeId
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Option[Submission]] = submissionDetails(formIdData, envelopeId).map(Some(_)).recoverWith {
    case UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode === StatusCodes.NotFound.intValue =>
      Option.empty[Submission].pure[Future]
  }

  def getSubmissionByLegacyIds(
    formIdData: FormIdData,
    envelopeId: EnvelopeId
  )(
    legacyIds: NonEmptyList[FormTemplateId]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Submission]] = {
    val NonEmptyList(legacyFormId, tail) = legacyIds
    val legacyFormIdData = formIdData.withTemplateId(legacyFormId)

    maybeSubmissionDetails(legacyFormIdData, envelopeId)
      .flatMap {
        case None =>
          NonEmptyList
            .fromList(tail)
            .map(getSubmissionByLegacyIds(formIdData, envelopeId))
            .getOrElse(Future.successful(Option.empty[Submission]))
        case Some(submission) => Future.successful(Some(submission))
      }
  }

  def maybeOneOfSubmissionDetails(
    formIdData1: FormIdData,
    formIdData2: FormIdData,
    envelopeId: EnvelopeId
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Option[Submission]] =
    maybeSubmissionDetails(
      formIdData1,
      envelopeId
    ).flatMap { maybeCurrentSubmission =>
      maybeCurrentSubmission.fold {
        maybeSubmissionDetails(
          formIdData2,
          envelopeId
        )
      } { currentSubmission =>
        Some(currentSubmission).pure[Future]
      }
    }

  /** ****formTemplate******
    */
  def upsertTemplate(template: JsValue)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.POST[JsValue, HttpResponse](
      s"$baseUrl/formtemplates",
      template,
      Seq("Content-Type" -> ContentType.`application/json`.value)
    ).void

  def maybeFormTemplate(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext) =
    ws.GET[FormTemplate](s"$baseUrl/formtemplates/${formTemplateId.value}/internal")
      .map(Some(_))
      .recoverWith {
        case UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode === StatusCodes.NotFound.intValue =>
          Option.empty[FormTemplate].pure[Future]
      }

  def getFormTemplate(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplate] =
    ws.GET[FormTemplate](s"$baseUrl/formtemplates/${formTemplateId.value}/internal")

  def getFormTemplateRaw(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[JsValue] =
    ws.GET[JsValue](s"$baseUrl/formtemplates/${formTemplateId.value}")

  def getFormTemplateContext(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplateContext] =
    ws.GET[FormTemplateContext](s"$baseUrl/formtemplates-with-redirects/${formTemplateId.value}")

  def getLatestFormTemplate(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplate] =
    ws.GET[FormTemplate](s"$baseUrl/formtemplates/${formTemplateId.value}/latest")

  /** ****file-upload******
    */
  def deleteFile(formId: FormId, fileId: FileId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.DELETE[HttpResponse](s"$baseUrl/forms/${formId.value}/deleteFile/${fileId.value}").void

  private val desOrganisationWithPlaceholders = s"$baseUrl/des/organisation/{{utr}}"
  private val organisationB = new DataRetrieveConnectorBlueprint(ws, desOrganisationWithPlaceholders, "organisation")

  def getDesOrganisation(dataRetrieve: DataRetrieve, request: DataRetrieve.Request)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[ServiceCallResponse[DataRetrieve.Response]] =
    organisationB.post(dataRetrieve, request)

  private val desAgentDetailsWithPlaceholders = s"$baseUrl/des/personal-details/arn/{{agentReferenceNumber}}"
  private val agentDetailsB =
    new DataRetrieveConnectorBlueprint(ws, desAgentDetailsWithPlaceholders, "personal-details")

  def getDesAgentDetails(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ServiceCallResponse[DataRetrieve.Response]] =
    agentDetailsB.get(dataRetrieve, request)

  /** **** Tax Period *****
    */
  def getAllTaxPeriods(
    htps: NonEmptyList[HmrcTaxPeriodWithEvaluatedId]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[NonEmptyList[ServiceCallResponse[TaxResponse]]] = {
    import JsonUtils._
    ws.POST[NonEmptyList[HmrcTaxPeriodWithEvaluatedId], NonEmptyList[ServiceCallResponse[TaxResponse]]](
      s"$baseUrl/obligation/tax-period",
      htps
    )
  }

  private val urlWithPlaceholders = s"$baseUrl/des-employments/{{nino}}/{{taxYear}}"
  private val employmentsProfileB = new DataRetrieveConnectorBlueprint(ws, urlWithPlaceholders, "employments")

  def getEmployments(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ServiceCallResponse[DataRetrieve.Response]] =
    employmentsProfileB.get(dataRetrieve, request)

  /** **** Form Bundles *****
    */
  def getFormBundle(
    rootFormId: FormIdData
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[NonEmptyList[FormIdData]] = {
    import JsonUtils._
    ws.GET[NonEmptyList[FormIdData]](show"$baseUrl/formBundles/${urlFragment(rootFormId)}")
  }

  def submitFormBundle(rootFormId: FormIdData, bundle: NonEmptyList[BundledFormSubmissionData])(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] = {
    import JsonUtils._
    ws.POST[NonEmptyList[BundledFormSubmissionData], HttpResponse](
      show"$baseUrl/formBundles/${urlFragment(rootFormId)}/submitAfterReview",
      bundle
    ).void
  }

  def sendEmail(
    notifierConfirmationCode: ConfirmationCodeWithEmailService
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] =
    ws.POST[ConfirmationCodeWithEmailService, HttpResponse](
      show"$baseUrl/email",
      notifierConfirmationCode,
      Seq("Content-Type" -> ContentType.`application/json`.value)
    ).void

  def dbLookup(id: String, collectionName: CollectionName, hc: HeaderCarrier)(implicit
    ec: ExecutionContext
  ): Future[Boolean] = {
    val url = s"$baseUrl/dblookup/$id/${collectionName.name}"
    ws.GET[HttpResponse](url)(HttpReads.Implicits.readRaw, hc, ec).map {
      _.status match {
        case 200 => true
        case _   => false
      }
    }
  }

  def upscanEncrypt(formIdData: FormIdData)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Crypted] = {
    val url = s"$baseUrl/upscan/encrypt"
    ws.POST[FormIdData, HttpResponse](url, formIdData).map(httpResponse => Crypted(httpResponse.body))
  }

  def retrieveConfirmation(
    reference: UpscanReference
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[UpscanConfirmation]] = {
    import uk.gov.hmrc.http.HttpReads.Implicits._
    val url = s"$baseUrl/upscan/${reference.value}"
    ws.GET[Option[UpscanConfirmation]](url)
  }

  def deleteUpscanReference(
    reference: UpscanReference
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = {
    val url = s"$baseUrl/upscan/${reference.value}"
    ws.DELETE[HttpResponse](url)(HttpReads.Implicits.readRaw, hc, ec).void
  }

  private def urlFragment(formIdData: FormIdData): String =
    formIdData match {
      case FormIdData.Plain(userId, formTemplateId) =>
        s"${userId.value}/${formTemplateId.value}"
      case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
        s"${userId.value}/${formTemplateId.value}/${accessCode.value}"
    }

  def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Envelope] = {
    import uk.gov.hmrc.http.HttpReads.Implicits._
    val url = s"$baseUrl/envelopes/${envelopeId.value}"
    ws.GET[Envelope](url)
  }

  def getMaybeEnvelope(
    envelopeId: EnvelopeId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Envelope]] =
    getEnvelope(envelopeId).map(Some(_)).recover {
      case UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode == StatusCodes.NotFound.intValue => None
    }

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] = {
    logger.info(s"delete file, envelopeId: '${envelopeId.value}', fileId: '${fileId.value}'")
    ws.DELETE[HttpResponse](s"$baseUrl/envelopes/${envelopeId.value}/files/${fileId.value}")
      .map(_ => ())
  }

  def deleteFiles(envelopeId: EnvelopeId, fileIds: Set[FileId])(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] = {
    logger.info(s"deleting files, envelopeId: '${envelopeId.value}', fileIds: '$fileIds'")
    ws.POST[Set[FileId], HttpResponse](s"$baseUrl/envelopes/${envelopeId.value}/files", fileIds)
      .map(_ => ())
  }

  def getDefaultSummarySection(formCategory: FormCategory)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[JsValue] =
    Json.toJson(formCategory) match {
      case JsString(category) =>
        val url = s"$baseUrl/builder/default-summary-section/$category"
        ws.GET[JsValue](url)
      case unknown => Future.failed(new Exception("Invalid form category, expected JsString got " + unknown))
    }

  def restoreForm(
    savedId: String,
    restoreId: String,
    useOriginalTemplate: Boolean
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SnapshotOverview] = {
    val url = s"$baseUrl/test-only/restore-form/$savedId/$restoreId?useOriginalTemplate=$useOriginalTemplate"
    ws.GET[SnapshotOverview](url)
  }

  def getSnapshots(
    payload: SnapshotFilter
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[List[SnapshotOverview]] = {
    val url = s"$baseUrl/test-only/snapshots"
    ws.POST[SnapshotFilter, List[SnapshotOverview]](
      url,
      payload,
      Seq("Content-Type" -> ContentType.`application/json`.value)
    )
  }

  def saveFormPage(formId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[JsValue] = {
    val url = s"$baseUrl/test-only/save-form/$formId"
    ws.GET[JsValue](url)
  }

  def saveForm(
    payload: SaveRequest
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SnapshotOverview] = {
    val url = s"$baseUrl/test-only/save-form"
    ws.POST[SaveRequest, SnapshotOverview](
      url,
      payload,
      Seq("Content-Type" -> ContentType.`application/json`.value)
    )
  }
  def updateSnapshot(
    payload: UpdateSnapshotRequest
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SnapshotOverview] = {
    val url = s"$baseUrl/test-only/update-snapshot"
    ws.POST[UpdateSnapshotRequest, SnapshotOverview](
      url,
      payload,
      Seq("Content-Type" -> ContentType.`application/json`.value)
    )
  }

  def snapshotOverview(
    snapshotId: SnapshotId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SnapshotOverview] = {
    val url = s"$baseUrl/test-only/snapshot-data/${snapshotId.value}"
    ws.GET[SnapshotOverview](url)
  }

  def updateFormData(
    payload: UpdateFormDataRequest
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SaveReply] = {
    val url = s"$baseUrl/test-only/update-form-data"
    ws.POST[UpdateFormDataRequest, SaveReply](
      url,
      payload,
      Seq("Content-Type" -> ContentType.`application/json`.value)
    )
  }

  def restoreSnapshotTemplate(
    snapshotId: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/test-only/restore-snapshot-template"
    ws.PUT[JsString, HttpResponse](url, JsString(snapshotId))
  }

  def deleteSnapshot(
    snapshotId: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/test-only/snapshot/$snapshotId"
    ws.DELETE[HttpResponse](url)
  }

  def deleteGeneratedFiles(
    envelopeId: EnvelopeId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/test-only/generated-files/${envelopeId.value}"
    ws.DELETE[HttpResponse](url)
  }

  def translationCsv(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/translation/${formTemplateId.value}"
    ws.GET[HttpResponse](url)
  }

  def translationCsvBrief(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/translation/${formTemplateId.value}/brief"
    ws.GET[HttpResponse](url)
  }

  def translationCsvInternal(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/translation/${formTemplateId.value}/internal"
    ws.GET[HttpResponse](url)
  }

  def translationEnTextBreakdown(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[EnTextBreakdowns] = {
    val url = s"$baseUrl/translation/${formTemplateId.value}/breakdown"
    ws.GET[EnTextBreakdowns](url)
  }

  def upsertFormAuthRetrievals(
    retrievals: FormAuthRetrievals
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.POST[FormAuthRetrievals, HttpResponse](s"$baseUrl/retrieval", retrievals)
      .map(_ => ())
}
