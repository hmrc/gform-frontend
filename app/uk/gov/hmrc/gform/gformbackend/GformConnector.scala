/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.config.{ ContentType, ExposedConfig }
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse }
import uk.gov.hmrc.gform.sharedmodel.notifier.{ NotifierConfirmationCode, NotifierEmailAddress }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, NotFoundException }

import scala.concurrent.{ ExecutionContext, Future }

class GformConnector(ws: WSHttp, baseUrl: String) {

  /******form*******/
  //TODO: remove userId since this information will be passed using HeaderCarrier
  def newForm(formTemplateId: FormTemplateId, userId: UserId, affinityGroup: Option[AffinityGroup])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[FormIdData] = {
    val ag = affinityGroup.map(a => "/" + AffinityGroupUtil.affinityGroupName(a)).getOrElse("")
    ws.POSTEmpty[FormIdData](s"$baseUrl/new-form/${formTemplateId.value}/${userId.value}$ag")
  }

  def getAllForms(userId: UserId, formTemplateId: FormTemplateId)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[List[FormOverview]] =
    ws.GET[List[FormOverview]](s"$baseUrl/forms/all/${userId.value}/${formTemplateId.value}")

  def getForm(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Form] =
    ws.GET[Form](s"$baseUrl/forms/${formId.value}")

  def getForm(formIdData: FormIdData)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Form] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/forms/${userId.value}/${formTemplateId.value}/${accessCode.value}"
      }
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
    ws.PUT[UserData, HttpResponse](url, userData).void
  }

  def forceUpdateFormStatus(formId: FormIdData, status: FormStatus)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Unit] =
    ws.PUT[String, HttpResponse](s"$baseUrl/formBundles/${urlFragment(formId)}/$status/forceStatus", "").void

  //TODO: now returns string, but it should return list of validations
  def validateSection(formId: FormId, sectionNumber: SectionNumber)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[String] =
    ws.GET[String](s"$baseUrl/forms/${formId.value}/validate-section/${sectionNumber.value}")

  def deleteForm(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.POSTEmpty[HttpResponse](baseUrl + s"/forms/${formId.value}/delete").void

  /******submission*******/
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

  private def mkPost(customerId: CustomerId, submissionData: SubmissionData, affinityGroup: Option[AffinityGroup])(
    url: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    ws.POST[SubmissionData, HttpResponse](
      url,
      submissionData,
      Seq("customerId" -> customerId.id, "affinityGroup" -> affinityGroupNameO(affinityGroup)))

  def submissionDetails(
    formIdData: FormIdData)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Submission] = {
    val url =
      formIdData match {
        case FormIdData.Plain(userId, formTemplateId) =>
          s"$baseUrl/submissionDetails/${userId.value}/${formTemplateId.value}"
        case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
          s"$baseUrl/submissionDetails/${userId.value}/${formTemplateId.value}/${accessCode.value}"
      }
    ws.GET[Submission](url)
  }

  /******formTemplate*******/
  def upsertTemplate(template: JsValue)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.POST[JsValue, HttpResponse](
        s"$baseUrl/formtemplates",
        template,
        Seq("Content-Type" -> ContentType.`application/json`.value))
      .void

  def getFormTemplate(
    formTemplateId: FormTemplateId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplate] =
    ws.GET[FormTemplate](s"$baseUrl/formtemplates/${formTemplateId.value}")

  /******file-upload*******/
  def deleteFile(formId: FormId, fileId: FileId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.DELETE[HttpResponse](s"$baseUrl/forms/${formId.value}/deleteFile/${fileId.value}").void

  /********Validators******/
  def validatePostCodeUtr(utr: String, desRegistrationRequest: DesRegistrationRequest)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[ServiceCallResponse[DesRegistrationResponse]] =
    ws.POST[DesRegistrationRequest, ServiceCallResponse[DesRegistrationResponse]](
      s"$baseUrl/validate/des/$utr",
      desRegistrationRequest)

  def validateBankModulus(accountNumber: String, sortCode: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    ws.POST[Account, HttpResponse](s"$baseUrl/validate/bank", Account(sortCode, accountNumber)).map(_ => true).recover {
      case _: NotFoundException => false
    }

  //TODO other formTemplate endpoints
  //TODO move this file to gform and make it's origin there

  /****** Tax Period ******/
  def getAllTaxPeriods(htps: NonEmptyList[HmrcTaxPeriodWithEvaluatedId])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[NonEmptyList[TaxResponse]] = {
    import JsonUtils._
    ws.POST[NonEmptyList[HmrcTaxPeriodWithEvaluatedId], NonEmptyList[TaxResponse]](
      s"$baseUrl/obligation/tax-period",
      htps)
  }

  /****** Form Bundles ******/
  def getFormBundle(
    rootFormId: FormIdData)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[NonEmptyList[FormIdData]] = {
    import JsonUtils._
    ws.GET[NonEmptyList[FormIdData]](show"$baseUrl/formBundles/${urlFragment(rootFormId)}")
  }

  def submitFormBundle(rootFormId: FormIdData, bundle: NonEmptyList[BundledFormSubmissionData])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Unit] = {
    import JsonUtils._
    ws.POST[NonEmptyList[BundledFormSubmissionData], HttpResponse](
        show"$baseUrl/formBundles/${urlFragment(rootFormId)}/submitAfterReview",
        bundle)
      .void
  }

  def sendEmail(
    emailAddress: NotifierEmailAddress,
    notifierConfirmationCode: NotifierConfirmationCode
  )(
    implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Unit] =
    ws.POST[NotifierConfirmationCode, HttpResponse](
        show"$baseUrl/email/$emailAddress",
        notifierConfirmationCode,
        Seq("Content-Type" -> ContentType.`application/json`.value))
      .void

  private def urlFragment(formIdData: FormIdData): String =
    formIdData match {
      case FormIdData.Plain(userId, formTemplateId) =>
        s"${userId.value}/${formTemplateId.value}"
      case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
        s"${userId.value}/${formTemplateId.value}/${accessCode.value}"
    }
}
