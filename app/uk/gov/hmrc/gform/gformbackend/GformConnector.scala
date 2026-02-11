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
import com.fasterxml.jackson.databind.JsonMappingException
import org.apache.commons.text.StringEscapeUtils
import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.slf4j.LoggerFactory
import org.typelevel.ci.CIString
import play.api.libs.json.{ JsString, JsValue, Json }
import uk.gov.hmrc.crypto.Crypted
import uk.gov.hmrc.gform.auth.models.{ EmailRetrievals, MaterialisedRetrievals }
import uk.gov.hmrc.gform.gform.{ CustomerId, DataRetrieveConnectorBlueprint }
import uk.gov.hmrc.gform.models.EmailId
import uk.gov.hmrc.gform.objectStore.Envelope
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.email.ConfirmationCodeWithEmailService
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.retrieval.{ AuthRetrievals, AuthRetrievalsByFormIdData }
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.testonly.snapshot._
import uk.gov.hmrc.gform.testonly.translation.TranslationAuditOverview
import uk.gov.hmrc.gform.testonly.{ EnTextBreakdowns, ExpressionsLookup }
import uk.gov.hmrc.gform.upscan.{ UpscanConfirmation, UpscanReference }
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps, UpstreamErrorResponse }

import scala.concurrent.{ ExecutionContext, Future }

class GformConnector(httpClient: HttpClientV2, baseUrl: String) {

  private val logger = LoggerFactory.getLogger(getClass)

  /** ****form******
    */
  //TODO: remove userId since this information will be passed using HeaderCarrier
  def newForm(
    formTemplateId: FormTemplateId,
    userId: UserId,
    affinityGroup: Option[AffinityGroup],
    queryParams: QueryParams
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormIdData] = {
    val ag = affinityGroup.map(a => AffinityGroupUtil.affinityGroupName(a)).getOrElse("")
    httpClient
      .post(url"$baseUrl/new-form/${formTemplateId.value}/${userId.value}/$ag")
      .withBody(Json.toJson(queryParams))
      .execute[FormIdData]
  }

  def getAllForms(userId: UserId, formTemplateId: FormTemplateId)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[List[FormOverview]] =
    httpClient.get(url"$baseUrl/forms/all/${userId.value}/${formTemplateId.value}").execute[List[FormOverview]]

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
    httpClient.get(url"$url").execute[Form]
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
    httpClient.put(url"$url").withBody(Json.toJson(userData)).execute[HttpResponse].void
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
    httpClient.put(url"$url").withBody(Json.toJson(formTemplateId)).execute[Form]
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
    httpClient.put(url"$url").withBody(Json.toJson(newFormId)).execute[Form]
  }

  def forceUpdateFormStatus(formId: FormIdData, status: FormStatus)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] =
    httpClient
      .put(url"$baseUrl/formBundles/${urlFragment(formId)}/$status/forceStatus")
      .withBody("")
      .execute[HttpResponse]
      .void

  def deleteForm(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    httpClient.post(url"$baseUrl/forms/${formId.value}/delete").execute[HttpResponse].void

  /** ****submission******
    */
  def createSubmission(
    formId: FormId,
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId,
    customerId: String,
    noOfAttachments: Int
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Submission] =
    httpClient
      .post(
        url"$baseUrl/forms/${formId.value}/${formTemplateId.value}/${envelopeId.value}/$noOfAttachments/createSubmission"
      )
      .withBody("")
      .setHeader("customerId" -> StringEscapeUtils.escapeHtml4(customerId))
      .execute[Submission]

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
    httpClient.get(url"$baseUrl/test-only/expressions/${formTemplateId.value}").execute[ExpressionsLookup]

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
    httpClient.get(url"$url").execute[HttpResponse].map(_.body)
  }

  private def mkPost(customerId: CustomerId, submissionData: SubmissionData, affinityGroup: Option[AffinityGroup])(
    url: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    httpClient
      .post(url"$url")
      .withBody(Json.toJson(submissionData))
      .setHeader("customerId" -> StringEscapeUtils.escapeHtml4(customerId.id))
      .setHeader("affinityGroup" -> affinityGroupNameO(affinityGroup))
      .execute[HttpResponse]

  def submissionDetails(
    formIdData: FormIdData,
    envelopeId: EnvelopeId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Submission]] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/submissionDetails/${userId.value}/${formTemplateId.value}/${envelopeId.value}"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/submissionDetails/${userId.value}/${formTemplateId.value}/${accessCode.value}/${envelopeId.value}"
      }
    httpClient.get(url"$url").execute[Submission].map(Some(_)).recoverWith {
      case UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode === StatusCodes.NotFound.intValue =>
        Option.empty[Submission].pure[Future]
    }
  }

  def getSubmissionByLegacyIds(
    formIdData: FormIdData,
    envelopeId: EnvelopeId
  )(
    legacyIds: NonEmptyList[FormTemplateId]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Submission]] = {
    val NonEmptyList(legacyFormId, tail) = legacyIds
    val legacyFormIdData = formIdData.withTemplateId(legacyFormId)

    submissionDetails(legacyFormIdData, envelopeId)
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
    submissionDetails(
      formIdData1,
      envelopeId
    ).flatMap { maybeCurrentSubmission =>
      maybeCurrentSubmission.fold {
        submissionDetails(
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
    httpClient
      .post(url"$baseUrl/formtemplates")
      .withBody(template)
      .setHeader("Content-Type" -> ContentType.`application/json`.value)
      .execute[HttpResponse]
      .void

  def maybeFormTemplate(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext) =
    httpClient
      .get(url"$baseUrl/formtemplates/${formTemplateId.value}/internal")
      .execute[FormTemplate]
      .map(Some(_))
      .recoverWith {
        case UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode === StatusCodes.NotFound.intValue =>
          Option.empty[FormTemplate].pure[Future]
      }

  def getFormTemplate(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplate] =
    httpClient.get(url"$baseUrl/formtemplates/${formTemplateId.value}/internal").execute[FormTemplate]

  def getFormTemplateRaw(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[JsValue] =
    httpClient.get(url"$baseUrl/formtemplates/${formTemplateId.value}").execute[JsValue]

  def getFormTemplateContext(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplateContext] =
    httpClient.get(url"$baseUrl/formtemplates-with-redirects/${formTemplateId.value}").execute[FormTemplateContext]

  def getFormTemplateBehavior(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplateBehavior] =
    httpClient.get(url"$baseUrl/formtemplates/${formTemplateId.value}/behavior").execute[FormTemplateBehavior]

  def getLatestFormTemplate(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplate] =
    httpClient.get(url"$baseUrl/formtemplates/${formTemplateId.value}/latest").execute[FormTemplate]

  /** ****file-upload******
    */
  def deleteFile(formId: FormId, fileId: FileId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    httpClient.delete(url"$baseUrl/forms/${formId.value}/deleteFile/${fileId.value}").execute[HttpResponse].void

  private val desOrganisationWithPlaceholders = s"$baseUrl/des/organisation/{{utr}}"
  private val organisationB =
    new DataRetrieveConnectorBlueprint(httpClient, desOrganisationWithPlaceholders, "organisation")

  def getDesOrganisation(dataRetrieve: DataRetrieve, request: DataRetrieve.Request)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[ServiceCallResponse[DataRetrieve.Response]] =
    organisationB.post(dataRetrieve, request)

  private val desAgentDetailsWithPlaceholders = s"$baseUrl/des/personal-details/arn/{{agentReferenceNumber}}"
  private val agentDetailsB =
    new DataRetrieveConnectorBlueprint(httpClient, desAgentDetailsWithPlaceholders, "personal-details")

  def getDesAgentDetails(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ServiceCallResponse[DataRetrieve.Response]] =
    agentDetailsB.get(dataRetrieve, request)

  private val urlNiClaimValidation = s"$baseUrl/hip/ni-claim-validation/{{nino}}/{{claimReference}}"
  private val claimValidationB =
    new DataRetrieveConnectorBlueprint(httpClient, urlNiClaimValidation, "ni claim validation")

  def getNiClaimValidation(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ServiceCallResponse[DataRetrieve.Response]] =
    claimValidationB.getEmptyIfNotFound(
      dataRetrieve,
      request,
      request.correlationId.fold(Seq.empty[(String, String)])(cId => Seq("correlationId" -> cId))
    )

  private val urlCaseflowCaseDetails = s"$baseUrl/hip/caseflow-case-details/{{caseId}}"
  private val caseflowCaseDetailsB = new DataRetrieveConnectorBlueprint(httpClient, urlCaseflowCaseDetails, "caseflow case details")

  def getCaseflowCaseDetails(
                              dataRetrieve: DataRetrieve,
                              request: DataRetrieve.Request
                            )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ServiceCallResponse[DataRetrieve.Response]] = {
    caseflowCaseDetailsB.getEmptyIfNotFound(
      dataRetrieve,
      request,
      request.correlationId.fold(Seq.empty[(String, String)])(cId => Seq("correlationId" -> cId))
    )
  }

  /** **** Tax Period *****
    */
  def getAllTaxPeriods(
    htps: NonEmptyList[HmrcTaxPeriodWithEvaluatedId]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[NonEmptyList[ServiceCallResponse[TaxResponse]]] = {
    import JsonUtils._
    httpClient
      .post(url"$baseUrl/obligation/tax-period")
      .withBody(Json.toJson(htps))
      .execute[NonEmptyList[ServiceCallResponse[TaxResponse]]]
  }

  private val urlWithPlaceholders = s"$baseUrl/hip/ni-employments/{{nino}}/{{taxYear}}"
  private val employmentsProfileB = new DataRetrieveConnectorBlueprint(httpClient, urlWithPlaceholders, "employments")

  def getEmployments(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ServiceCallResponse[DataRetrieve.Response]] =
    employmentsProfileB.getEmptyIfNotFound(
      dataRetrieve,
      request,
      request.correlationId.fold(Seq.empty[(String, String)])(cId => Seq("correlationId" -> cId))
    )

  /** **** Form Bundles *****
    */
  def getFormBundle(
    rootFormId: FormIdData
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[NonEmptyList[FormIdData]] = {
    import JsonUtils._
    httpClient.get(url"$baseUrl/formBundles/${urlFragment(rootFormId)}").execute[NonEmptyList[FormIdData]]
  }

  def submitFormBundle(rootFormId: FormIdData, bundle: NonEmptyList[BundledFormSubmissionData])(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] = {
    import JsonUtils._
    httpClient
      .post(url"$baseUrl/formBundles/${urlFragment(rootFormId)}/submitAfterReview")
      .withBody(Json.toJson(bundle))
      .execute[HttpResponse]
      .void
  }

  def sendEmail(
    notifierConfirmationCode: ConfirmationCodeWithEmailService
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] =
    httpClient
      .post(url"$baseUrl/email")
      .withBody(Json.toJson(notifierConfirmationCode))
      .setHeader("Content-Type" -> ContentType.`application/json`.value)
      .execute[HttpResponse]
      .void

  def dbLookup(id: String, collectionName: CollectionName, hc: HeaderCarrier)(implicit
    ec: ExecutionContext
  ): Future[Boolean] = {
    val url = s"$baseUrl/dblookup/$id/${collectionName.name}"
    httpClient.get(url"$url")(hc).execute[HttpResponse].map {
      _.status match {
        case 200 => true
        case _   => false
      }
    }
  }

  def upscanEncrypt(formIdData: FormIdData)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Crypted] = {
    val url = s"$baseUrl/upscan/encrypt"
    httpClient
      .post(url"$url")
      .withBody(Json.toJson(formIdData))
      .execute[HttpResponse]
      .map(httpResponse => Crypted(httpResponse.body))
  }

  def retrieveConfirmation(
    reference: UpscanReference
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[UpscanConfirmation]] = {
    val url = s"$baseUrl/upscan/${reference.value}"
    httpClient.get(url"$url").execute[Option[UpscanConfirmation]]
  }

  def deleteUpscanReference(
    reference: UpscanReference
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = {
    val url = s"$baseUrl/upscan/${reference.value}"
    httpClient.delete(url"$url").execute[HttpResponse].void
  }

  private def urlFragment(formIdData: FormIdData): String =
    formIdData match {
      case FormIdData.Plain(userId, formTemplateId) =>
        s"${userId.value}/${formTemplateId.value}"
      case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
        s"${userId.value}/${formTemplateId.value}/${accessCode.value}"
    }

  def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Envelope] = {
    val url = s"$baseUrl/envelopes/${envelopeId.value}"
    httpClient.get(url"$url").execute[Envelope]
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
    httpClient
      .delete(url"$baseUrl/envelopes/${envelopeId.value}/files/${fileId.value}")
      .execute[HttpResponse]
      .map(_ => ())
  }

  def deleteFiles(envelopeId: EnvelopeId, fileIds: Set[FileId])(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] = {
    logger.info(s"deleting files, envelopeId: '${envelopeId.value}', fileIds: '$fileIds'")
    httpClient
      .post(url"$baseUrl/envelopes/${envelopeId.value}/files")
      .withBody(Json.toJson(fileIds))
      .execute[HttpResponse]
      .map(_ => ())
  }

  def getDefaultSummarySection(formCategory: FormCategory)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[JsValue] =
    Json.toJson(formCategory) match {
      case JsString(category) =>
        val url = s"$baseUrl/builder/default-summary-section/$category"
        httpClient.get(url"$url").execute[JsValue]
      case unknown => Future.failed(new Exception("Invalid form category, expected JsString got " + unknown))
    }

  def restoreForm(
    snapshotId: SnapshotId,
    useOriginalTemplate: Boolean
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[RestoredSnapshot] = {
    val url = s"$baseUrl/test-only/restore-form/${snapshotId.value}/$useOriginalTemplate"
    httpClient.get(url"$url").execute[RestoredSnapshot]
  }

  def getSnapshots(
    payload: SnapshotFilter
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[List[SnapshotOverview]] = {
    val url = s"$baseUrl/test-only/snapshots"
    httpClient
      .post(url"$url")
      .withBody(Json.toJson(payload))
      .setHeader("Content-Type" -> ContentType.`application/json`.value)
      .execute[List[SnapshotOverview]]
  }

  def saveFormPage(formId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[JsValue] = {
    val url = s"$baseUrl/test-only/save-form/$formId"
    httpClient.get(url"$url").execute[JsValue]
  }

  def saveForm(
    payload: SaveRequest
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SnapshotOverview] = {
    val url = s"$baseUrl/test-only/save-form"
    httpClient
      .post(url"$url")
      .withBody(Json.toJson(payload))
      .setHeader("Content-Type" -> ContentType.`application/json`.value)
      .execute[SnapshotOverview]
  }
  def updateSnapshot(
    payload: UpdateSnapshotRequest
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SnapshotOverview] = {
    val url = s"$baseUrl/test-only/update-snapshot"
    httpClient
      .post(url"$url")
      .withBody(Json.toJson(payload))
      .setHeader("Content-Type" -> ContentType.`application/json`.value)
      .execute[SnapshotOverview]
  }

  def snapshotOverview(
    snapshotId: SnapshotId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SnapshotOverview] = {
    val url = s"$baseUrl/test-only/snapshot-data/${snapshotId.value}"
    httpClient.get(url"$url").execute[SnapshotOverview]
  }

  def loadSnapshotData(
    payload: UpdateFormDataRequest
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Form] = {
    val url = s"$baseUrl/test-only/snapshot-load-data"
    httpClient
      .post(url"$url")
      .withBody(Json.toJson(payload))
      .setHeader("Content-Type" -> ContentType.`application/json`.value)
      .execute[Form]
  }

  def deleteSnapshot(
    snapshotId: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/test-only/snapshot/$snapshotId"
    httpClient.delete(url"$url").execute[HttpResponse]
  }

  def deleteGeneratedFiles(
    envelopeId: EnvelopeId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/test-only/generated-files/${envelopeId.value}"
    httpClient.delete(url"$url").execute[HttpResponse]
  }

  def translationCsv(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/translation/${formTemplateId.value}"
    httpClient.get(url"$url").execute[HttpResponse]
  }

  def translationCsvBrief(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/translation/${formTemplateId.value}/brief"
    httpClient.get(url"$url").execute[HttpResponse]
  }

  def translationCsvInternal(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url = s"$baseUrl/translation/${formTemplateId.value}/internal"
    httpClient.get(url"$url").execute[HttpResponse]
  }

  def translationEnTextBreakdown(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[EnTextBreakdowns] = {
    val url = s"$baseUrl/translation/${formTemplateId.value}/breakdown"
    httpClient.get(url"$url").execute[EnTextBreakdowns]
  }

  def upsertAuthRetrievals(
    retrievals: AuthRetrievals
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    httpClient
      .post(url"$baseUrl/retrieval")
      .withBody(Json.toJson(retrievals))
      .execute[HttpResponse]
      .map(_ => ())

  def upsertAuthRetrievalsByFormIdData(
    retrievals: AuthRetrievalsByFormIdData
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    httpClient
      .post(url"$baseUrl/retrieval-by-form")
      .withBody(Json.toJson(retrievals))
      .execute[HttpResponse]
      .map(_ => ())

  def translationAudit(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[TranslationAuditOverview]] = {
    val url = s"$baseUrl/translation-audit/overview/${formTemplateId.value}"
    httpClient.get(url"$url").execute[Option[TranslationAuditOverview]]
  }

  def validateFormHtml(
    rawTemplateJson: JsValue
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[JsValue] =
    httpClient
      .post(url"$baseUrl/formtemplates/validate-html")
      .withBody(rawTemplateJson)
      .execute[JsValue]
      .recover { case _: JsonMappingException =>
        Json.parse("""{"valid":"No HTML validation errors detected"}""")
      }

  def migrateEmailToGG(
    formIdData: FormIdData,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = {
    val emailFormData: Option[EmailRetrievals] =
      retrievals.getGGEmail.map(ggEmail => EmailRetrievals(EmailId(CIString(ggEmail))))

    emailFormData match {
      case None => Future.successful(())
      case Some(email) =>
        formIdData match {
          case p @ FormIdData.Plain(_, _) =>
            val emailToGGMigration = EmailToGGMigration(p, email.groupId, formTemplate._id)
            val url = s"$baseUrl/email-migration"
            httpClient
              .post(url"$url")
              .withBody(Json.toJson(emailToGGMigration))
              .execute[HttpResponse]
              .void
          case FormIdData.WithAccessCode(_, _, _) =>
            Future.successful(())

        }
    }

  }

  def changeOverrides(formTemplateId: FormTemplateId, overrides: Overrides)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[HttpResponse] = {
    val url = s"$baseUrl/builder/update-overrides/${formTemplateId.value}"
    httpClient
      .post(url"$url")
      .withBody(Json.toJson(overrides))
      .execute[HttpResponse]
  }

}
