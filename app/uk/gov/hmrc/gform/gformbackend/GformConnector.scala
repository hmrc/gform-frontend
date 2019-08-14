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
import play.api.libs.json.JsValue
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.config.{ ContentType, ExposedConfig }
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.submission.{ Submission, SubmissionRef }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse, NotFoundException }

import scala.concurrent.{ ExecutionContext, Future }

/**
  * This connector originates in GFORM project.
  * Edit it there first and propagate it from there.
  */
class GformConnector(ws: WSHttp, baseUrl: String) {

  /******form*******/
  //TODO: remove userId since this information will be passed using HeaderCarrier
  def newForm(formTemplateId: FormTemplateId, userId: UserId, affinityGroup: Option[AffinityGroup])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[NewFormData] = {
    val ag = affinityGroup.map(a => "/" + AffinityGroupUtil.affinityGroupName(a)).getOrElse("")
    ws.POSTEmpty[NewFormData](s"$baseUrl/new-form/${formTemplateId.value}/${userId.value}$ag")
  }

  def getForm(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Form] =
    ws.GET[Form](s"$baseUrl/forms/${formId.value}")

  def maybeForm(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Form]] =
    ws.GET[Form](s"$baseUrl/forms/${formId.value}").map(Some(_)).recover {
      case e: NotFoundException => None
    }

  def updateUserData(formId: FormId, userData: UserData)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Unit] =
    ws.PUT[UserData, HttpResponse](s"$baseUrl/forms/${formId.value}", userData).map(_ => ())

  //TODO: now returns string, but it should return list of validations
  def validateSection(formId: FormId, sectionNumber: SectionNumber)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[String] =
    ws.GET[String](s"$baseUrl/forms/${formId.value}/validate-section/${sectionNumber.value}")

  def deleteForm(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.POSTEmpty[HttpResponse](baseUrl + s"/forms/${formId.value}/delete").map(_ => ())

  /******submission*******/
  def submitForm(
    formId: FormId,
    customerId: CustomerId,
    submissionData: SubmissionData,
    affinityGroup: Option[AffinityGroup])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    mkPost(customerId, submissionData, affinityGroup)(s"$baseUrl/forms/${formId.value}/submitForm")

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

  def submissionDetails(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Submission] =
    ws.GET[Submission](s"$baseUrl/forms/${formId.value}/submissionDetails")

  /******formTemplate*******/
  def upsertTemplate(template: JsValue)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.POST[JsValue, HttpResponse](
        s"$baseUrl/formtemplates",
        template,
        Seq("Content-Type" -> ContentType.`application/json`.value))
      .map(_ => ())

  def getFormTemplate(
    formTemplateId: FormTemplateId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[FormTemplate] =
    ws.GET[FormTemplate](s"$baseUrl/formtemplates/${formTemplateId.value}")

  /******exposed-config*******/
  def getExposedConfig(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ExposedConfig] =
    ws.GET[ExposedConfig](s"$baseUrl/exposed-config")

  /******file-upload*******/
  def deleteFile(formId: FormId, fileId: FileId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    ws.DELETE[HttpResponse](s"$baseUrl/forms/${formId.value}/deleteFile/${fileId.value}").map(_ => ())

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

}
