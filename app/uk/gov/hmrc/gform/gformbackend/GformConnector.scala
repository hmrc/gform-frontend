/*
 * Copyright 2021 HM Revenue & Customs
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

import akka.http.scaladsl.model.StatusCodes
import cats.data.NonEmptyList
import cats.instances.future._
import cats.instances.string._
import cats.syntax.functor._
import cats.syntax.show._
import org.slf4j.LoggerFactory
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse }
import uk.gov.hmrc.gform.sharedmodel.email.ConfirmationCodeWithEmailService
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.upscan.{ UpscanFileStatus, UpscanReference }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpReadsInstances, HttpResponse, UpstreamErrorResponse }
import uk.gov.hmrc.http.HttpReads.Implicits.readFromJson

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

  def maybeForm(formIdData: FormIdData, formTemplate: FormTemplate)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Option[Form]] =
    getForm(formIdData).map(Some(_)).recoverWith {
      case UpstreamErrorResponse.WithStatusCode(statusCode, _) if statusCode == StatusCodes.NotFound.intValue =>
        val formIdDataOriginal = formIdData.withOriginalTemplateId(formTemplate)
        logger.info(
          s"Attempt to access form $formIdData, but form not found in MongoDB, attempting to look for $formIdDataOriginal as a fallback."
        )
        getForm(formIdDataOriginal)
          .flatMap { form =>
            logger.info(
              s"Attempt to access form $formIdData, but form not found in MongoDB, attempt to look for $formIdDataOriginal as a fallback succeeded."
            )

            /*
             *  Note! We are not replacing form in mongo, but updating it. And to update the form we need to first find it with original formTemplateId
             *  in its name. Backend is making sure that update operation is lowercasing formTemplateId after form is found and before it is saved.
             */
            updateUserData(
              formIdDataOriginal,
              UserData(
                form.formData,
                form.status,
                form.visitsIndex,
                form.thirdPartyData,
                form.componentIdToFileId
              )
            ).map { _ =>
              logger.info(
                s"Attempt to access form $formIdData, but form not found in MongoDB, attempt to look for $formIdDataOriginal as a fallback succeeded and form saved with $formIdData."
              )
              Some(form)
            }

          }
          .recover {
            case UpstreamErrorResponse.WithStatusCode(statusCode, _) if statusCode == StatusCodes.NotFound.intValue =>
              logger.info(
                s"Attempt to access form $formIdData, but form not found in MongoDB, attempt to look for $formIdDataOriginal as a fallback failed."
              )
              None
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
      Seq(("customerId", customerId))
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

  private def mkPost(customerId: CustomerId, submissionData: SubmissionData, affinityGroup: Option[AffinityGroup])(
    url: String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    ws.POST[SubmissionData, HttpResponse](
      url,
      submissionData,
      Seq("customerId" -> customerId.id, "affinityGroup" -> affinityGroupNameO(affinityGroup))
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

  /** ****formTemplate******
    */
  def upsertTemplate(template: JsValue)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.POST[JsValue, HttpResponse](
      s"$baseUrl/formtemplates",
      template,
      Seq("Content-Type" -> ContentType.`application/json`.value)
    ).void

  def getFormTemplate(
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplate] =
    ws.GET[FormTemplate](s"$baseUrl/formtemplates/${formTemplateId.value}")

  /** ****file-upload******
    */
  def deleteFile(formId: FormId, fileId: FileId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.DELETE[HttpResponse](s"$baseUrl/forms/${formId.value}/deleteFile/${fileId.value}").void

  /** ******Validators*****
    */
  def validatePostCodeUtr(utr: String, desRegistrationRequest: DesRegistrationRequest)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[ServiceCallResponse[DesRegistrationResponse]] =
    ws.POST[DesRegistrationRequest, ServiceCallResponse[DesRegistrationResponse]](
      s"$baseUrl/validate/des/$utr",
      desRegistrationRequest
    )

  def validateBankModulus(accountNumber: String, sortCode: String)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Boolean] =
    ws.POST[Account, HttpResponse](s"$baseUrl/validate/bank", Account(sortCode, accountNumber)).map(_ => true).recover {
      case UpstreamErrorResponse.WithStatusCode(statusCode, _) if statusCode == StatusCodes.NotFound.intValue => false
    }

  //TODO other formTemplate endpoints
  //TODO move this file to gform and make it's origin there

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

  def retrieveConfirmation(
    reference: UpscanReference
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[UpscanFileStatus]] = {
    import uk.gov.hmrc.http.HttpReads.Implicits._
    val url = s"$baseUrl/upscan/${reference.value}"
    ws.GET[Option[UpscanFileStatus]](url)
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
}
