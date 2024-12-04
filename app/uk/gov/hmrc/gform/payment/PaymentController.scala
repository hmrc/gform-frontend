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

package uk.gov.hmrc.gform.payment

import org.slf4j.LoggerFactory
import play.api.libs.json.{ JsObject, Json }
import uk.gov.hmrc.gform.wshttp.WSHttp

import scala.concurrent.ExecutionContext
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.http.HttpReads.Implicits.readFromJson
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.util.{ Failure, Success, Try }

class PaymentController(
  auth: AuthenticatedRequestActions,
  ws: WSHttp,
  messagesControllerComponents: MessagesControllerComponents,
  appConfig: AppConfig,
  serviceConfig: ServicesConfig
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  private val logger = LoggerFactory.getLogger(getClass)

  def pay(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    reference: PaymentReference,
    amountInPence: String,
    backPath: String
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.EditFormAcknowledgement
    ) { implicit request => lang => cache => sse => formModelOptics =>
      val url = s"${serviceConfig.baseUrl("pay-api")}/pay-api/vat-consumer-to-consumer/journey/start"
      val backUrl = s"${appConfig.`gform-frontend-base-url`}$backPath"

      val amountInPenceLong = Try(amountInPence.toDouble.toLong) match {
        case Success(value) => value
        case Failure(exception) =>
          throw new ArithmeticException(s"Failed to convert '$amountInPence' to Long, ${exception.getMessage}")
      }

      val vatCToCRequest = Json.obj(
        "reference"     -> reference.value,
        "amountInPence" -> amountInPenceLong,
        "returnUrl"     -> "https://www.gov.uk ", //will be removed once OPS-13088 is completed.
        "backUrl"       -> backUrl
      )

      ws.POST[JsObject, VatCToCResponse](url, vatCToCRequest)
        .map { response =>
          logger.info(
            s"Redirecting to payment journey with formTemplateId: ${formTemplateId.value}, reference: ${reference.value}, journeyId: ${response.journeyId}"
          )
          Redirect(response.nextUrl)
        }
        .recover { case ex =>
          val errorMsg =
            s"Unknown problem when calling pay-api with formTemplateId: ${formTemplateId.value}, reference: ${reference.value} error: $ex"
          logger.error(errorMsg, ex)
          throw new RuntimeException(errorMsg)
        }
    }
}
