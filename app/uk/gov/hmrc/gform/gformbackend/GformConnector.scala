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

import akka.util.ByteString
import play.api.libs.json.JsValue
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.config.{ ContentType, ExposedConfig }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, Account, UserId, ValAddress }
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse, NotFoundException }

/**
  * This connector originates in GFORM project.
  * Edit it there first and propagate it from there.
  */
class GformConnector(ws: WSHttp, baseUrl: String) {

  /******form*******/
  //TODO: remove userId since this information will be passed using HeaderCarrier
  def newForm(formTemplateId: FormTemplateId, userId: UserId, accessCode: Option[AccessCode])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[FormId] = {
    val ac = accessCode.map("/" + _.value).getOrElse("")
    ws.POSTEmpty[FormId](s"$baseUrl/new-form/${formTemplateId.value}/${userId.value}$ac")
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
  def submitForm(formId: FormId, customerId: String, affinityGroup: Option[AffinityGroup])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[HttpResponse] = {
    implicit val hcNew = hc
      .withExtraHeaders("customerId" -> customerId)
      .withExtraHeaders("affinityGroup" -> affinityGroupNameO(affinityGroup))
    ws.POSTEmpty[HttpResponse](s"$baseUrl/forms/${formId.value}/submission")(
      implicitly[HttpReads[HttpResponse]],
      hcNew,
      ec)
  }

  def submitFormWithPdf(formId: FormId, customerId: String, htmlForm: String, affinityGroup: Option[AffinityGroup])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[HttpResponse] = {
    implicit val hcNew = hc
      .withExtraHeaders("customerId" -> customerId)
      .withExtraHeaders("affinityGroup" -> affinityGroupNameO(affinityGroup))
    ws.POSTString[HttpResponse](s"$baseUrl/forms/${formId.value}/submission-pdf", htmlForm)(
      implicitly[HttpReads[HttpResponse]],
      hcNew,
      ec)
  }

  def submissionStatus(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Submission] =
    ws.GET[Submission](s"$baseUrl/forms/${formId.value}/submission")

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
  def validatePostCodeUtr(utr: String, postCode: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    ws.POST[ValAddress, HttpResponse](s"$baseUrl/validate/des", ValAddress(utr, postCode)).map(_ => true).recover {
      case _: NotFoundException => false
    }

  def validateBankModulus(accountNumber: String, sortCode: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    ws.POST[Account, HttpResponse](s"$baseUrl/validate/bank", Account(sortCode, accountNumber)).map(_ => true).recover {
      case _: NotFoundException => false
    }

  //TODO other formTemplate endpoints
  //TODO move this file to gform and make it's origin there

  /*** White Listing ***/ //TODO remove once internal Users have been through system.
  def whiteList(
    currentUserEmail: Option[String])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    currentUserEmail.fold(
      Future.successful(Option.empty[String])
    )(
      email =>
        ws.POST[String, String](s"$baseUrl/white-list/users", email)
          .map(Some(_))
          .recover { case e: NotFoundException => None }
    )

}
