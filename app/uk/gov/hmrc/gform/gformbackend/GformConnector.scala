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

import cats.data.NonEmptyList
import cats.instances.future._
import cats.instances.string._
import cats.syntax.functor._
import cats.syntax.show._
import play.api.libs.json.JsValue
import uk.gov.hmrc.auth.core.AffinityGroup
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
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpReadsInstances, HttpResponse, NotFoundException }

import scala.concurrent.{ ExecutionContext, Future }

class GformConnector(ws: WSHttp, baseUrl: String) {

  /******form*******/
  //TODO: remove userId since this information will be passed using HeaderCarrier
  def newForm(
    formTemplateId: FormTemplateId,
    userId: UserId,
    affinityGroup: Option[AffinityGroup],
    queryParams: QueryParams
  )(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[FormIdData] = {
    val ag = affinityGroup.map(a => "/" + AffinityGroupUtil.affinityGroupName(a)).getOrElse("")
    implicit val httpJsonReads: HttpReads[FormIdData] = HttpReads.Implicits.throwOnFailure(
      HttpReadsInstances.readEitherOf(HttpReadsInstances.readJsValue.map(_.as[FormIdData])))
    ws.POST[QueryParams, FormIdData](s"$baseUrl/new-form/${formTemplateId.value}/${userId.value}$ag", queryParams)
  }

  def getAllForms(userId: UserId, formTemplateId: FormTemplateId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[List[FormOverview]] = {
    implicit val httpJsonReads: HttpReads[List[FormOverview]] = HttpReads.Implicits.throwOnFailure(
      HttpReadsInstances.readEitherOf(HttpReadsInstances.readJsValue.map(_.as[List[FormOverview]])))
    ws.GET[List[FormOverview]](s"$baseUrl/forms/all/${userId.value}/${formTemplateId.value}")
  }

  def getForm(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Form] = {
    implicit val httpJsonReads: HttpReads[Form] = HttpReads.Implicits.throwOnFailure(
      HttpReadsInstances.readEitherOf(HttpReadsInstances.readJsValue.map(_.as[Form])))
    ws.GET[Form](s"$baseUrl/forms/${formId.value}")
  }

  def getForm(formIdData: FormIdData)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Form] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/${accessCode.value}"
      }
    implicit val httpJsonReads: HttpReads[Form] = HttpReads.Implicits.throwOnFailure(
      HttpReadsInstances.readEitherOf(HttpReadsInstances.readJsValue.map(_.as[Form])))
    ws.GET[Form](url)
  }

  def maybeForm(formIdData: FormIdData)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Form]] =
    getForm(formIdData).map(Some(_)).recover {
      case e: NotFoundException => None
    }

  def updateUserData(formIdData: FormIdData, userData: UserData)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Unit] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/${accessCode.value}"
      }
    implicit val httpJsonReads: HttpReads[HttpResponse] = HttpReadsInstances.readRaw
    ws.PUT[UserData, HttpResponse](url, userData).void
  }

  def forceUpdateFormStatus(formId: FormIdData, status: FormStatus)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Unit] = {
    implicit val httpRawReads: HttpReads[HttpResponse] = HttpReadsInstances.readRaw
    ws.PUT[String, HttpResponse](s"$baseUrl/formBundles/${urlFragment(formId)}/$status/forceStatus", "").void
  }

  def deleteForm(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = {
    implicit val httpRawReads: HttpReads[HttpResponse] = HttpReadsInstances.readRaw
    ws.POSTEmpty[HttpResponse](baseUrl + s"/forms/${formId.value}/delete").void
  }

  /******submission*******/
  def createSubmission(
    formId: FormId,
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId,
    customerId: String,
    noOfAttachments: Int)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Submission] = {
    implicit val httpJsonReads: HttpReads[Submission] = HttpReads.Implicits.throwOnFailure(
      HttpReadsInstances.readEitherOf(HttpReadsInstances.readJsValue.map(_.as[Submission])))
    ws.POST[String, Submission](
      baseUrl + s"/forms/${formId.value}/${formTemplateId.value}/${envelopeId.value}/$noOfAttachments/createSubmission",
      "",
      Seq(("customerId", customerId))
    )
  }

  def submitForm(
    formIdData: FormIdData,
    customerId: CustomerId,
    submissionData: SubmissionData,
    affinityGroup: Option[AffinityGroup])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/submitForm"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/${accessCode.value}/submitForm"
      }
    mkPost(customerId, submissionData, affinityGroup)(url)
  }

  /******test-only*******/
  def renderHandlebarPayload(
    formTemplateId: FormTemplateId,
    formId: FormId,
    destinationId: DestinationId,
    customerId: CustomerId,
    submissionData: SubmissionData,
    affinityGroup: Option[AffinityGroup])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    mkPost(customerId, submissionData, affinityGroup)(
      s"$baseUrl/test-only/${formTemplateId.value}/${formId.value}/${destinationId.id}")

  def renderHandlebarModel(
    formTemplateId: FormTemplateId,
    formId: FormId,
    customerId: CustomerId,
    submissionData: SubmissionData,
    affinityGroup: Option[AffinityGroup])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    mkPost(customerId, submissionData, affinityGroup)(
      s"$baseUrl/test-only/handlebars-model/${formTemplateId.value}/${formId.value}")

  private def mkPost(customerId: CustomerId, submissionData: SubmissionData, affinityGroup: Option[AffinityGroup])(
    url: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    implicit val httpRawReads: HttpReads[HttpResponse] = HttpReadsInstances.readRaw
    ws.POST[SubmissionData, HttpResponse](
      url,
      submissionData,
      Seq("customerId" -> customerId.id, "affinityGroup" -> affinityGroupNameO(affinityGroup)))
  }

  def submissionDetails(
    formIdData: FormIdData)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Submission] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/submissionDetails/${userId.value}/${formTemplateId.value}"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/submissionDetails/${userId.value}/${formTemplateId.value}/${accessCode.value}"
      }
    implicit val httpJsonReads: HttpReads[Submission] = HttpReads.Implicits.throwOnFailure(
      HttpReadsInstances.readEitherOf(HttpReadsInstances.readJsValue.map(_.as[Submission])))
    ws.GET[Submission](url)
  }

  /******formTemplate*******/
  def upsertTemplate(template: JsValue)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = {
    implicit val httpRawReads: HttpReads[HttpResponse] = HttpReadsInstances.readRaw
    ws.POST[JsValue, HttpResponse](
        s"$baseUrl/formtemplates",
        template,
        Seq("Content-Type" -> ContentType.`application/json`.value))
      .void
  }

  def getFormTemplate(
    formTemplateId: FormTemplateId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplate] = {
    implicit val httpJsonReads: HttpReads[FormTemplate] = HttpReads.Implicits.throwOnFailure(
      HttpReadsInstances.readEitherOf(HttpReadsInstances.readJsValue.map(_.as[FormTemplate])))
    ws.GET[FormTemplate](s"$baseUrl/formtemplates/${formTemplateId.value}")
  }

  /******file-upload*******/
  def deleteFile(formId: FormId, fileId: FileId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = {
    implicit val httpRawReads: HttpReads[HttpResponse] = HttpReadsInstances.readRaw
    ws.DELETE[HttpResponse](s"$baseUrl/forms/${formId.value}/deleteFile/${fileId.value}").void
  }

  /********Validators******/
  def validatePostCodeUtr(utr: String, desRegistrationRequest: DesRegistrationRequest)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[ServiceCallResponse[DesRegistrationResponse]] = {
    implicit val httpJsonReads: HttpReads[ServiceCallResponse[DesRegistrationResponse]] =
      HttpReads.Implicits.throwOnFailure(
        HttpReadsInstances.readEitherOf(
          HttpReadsInstances.readJsValue.map(_.as[ServiceCallResponse[DesRegistrationResponse]])))
    ws.POST[DesRegistrationRequest, ServiceCallResponse[DesRegistrationResponse]](
      s"$baseUrl/validate/des/$utr",
      desRegistrationRequest)
  }

  def validateBankModulus(accountNumber: String, sortCode: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] = {
    implicit val httpRawReads: HttpReads[HttpResponse] = HttpReadsInstances.readRaw
    ws.POST[Account, HttpResponse](s"$baseUrl/validate/bank", Account(sortCode, accountNumber)).map(_ => true).recover {
      case _: NotFoundException => false
    }
  }

  //TODO other formTemplate endpoints
  //TODO move this file to gform and make it's origin there

  /****** Tax Period ******/
  def getAllTaxPeriods(htps: NonEmptyList[HmrcTaxPeriodWithEvaluatedId])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[NonEmptyList[ServiceCallResponse[TaxResponse]]] = {
    import JsonUtils._
    implicit val httpJsonReads: HttpReads[NonEmptyList[ServiceCallResponse[TaxResponse]]] =
      HttpReads.Implicits.throwOnFailure(
        HttpReadsInstances.readEitherOf(
          HttpReadsInstances.readJsValue.map(_.as[NonEmptyList[ServiceCallResponse[TaxResponse]]])))
    ws.POST[NonEmptyList[HmrcTaxPeriodWithEvaluatedId], NonEmptyList[ServiceCallResponse[TaxResponse]]](
      s"$baseUrl/obligation/tax-period",
      htps)
  }

  /****** Form Bundles ******/
  def getFormBundle(
    rootFormId: FormIdData)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[NonEmptyList[FormIdData]] = {
    import JsonUtils._
    implicit val httpJsonReads: HttpReads[NonEmptyList[FormIdData]] = HttpReads.Implicits.throwOnFailure(
      HttpReadsInstances.readEitherOf(HttpReadsInstances.readJsValue.map(_.as[NonEmptyList[FormIdData]])))
    ws.GET[NonEmptyList[FormIdData]](show"$baseUrl/formBundles/${urlFragment(rootFormId)}")
  }

  def submitFormBundle(rootFormId: FormIdData, bundle: NonEmptyList[BundledFormSubmissionData])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Unit] = {
    import JsonUtils._
    implicit val httpRawReads: HttpReads[HttpResponse] = HttpReadsInstances.readRaw
    ws.POST[NonEmptyList[BundledFormSubmissionData], HttpResponse](
        show"$baseUrl/formBundles/${urlFragment(rootFormId)}/submitAfterReview",
        bundle)
      .void
  }

  def sendEmail(
    notifierConfirmationCode: ConfirmationCodeWithEmailService
  )(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] = {
    implicit val httpRawReads: HttpReads[HttpResponse] = HttpReadsInstances.readRaw
    ws.POST[ConfirmationCodeWithEmailService, HttpResponse](
        show"$baseUrl/email",
        notifierConfirmationCode,
        Seq("Content-Type" -> ContentType.`application/json`.value))
      .void
  }

  def dbLookup(id: String, collectionName: CollectionName, hc: HeaderCarrier)(
    implicit ec: ExecutionContext): Future[Boolean] = {
    implicit val hc_ = hc

    val url = s"$baseUrl/dblookup/$id/${collectionName.name}"
    ws.doGet(url, hc.extraHeaders) map { response =>
      response.status match {
        case 200 => true
        case _   => false
      }
    }
  }

  private def urlFragment(formIdData: FormIdData): String =
    formIdData match {
      case FormIdData.Plain(userId, formTemplateId) =>
        s"${userId.value}/${formTemplateId.value}"
      case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
        s"${userId.value}/${formTemplateId.value}/${accessCode.value}"
    }
}
