/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.controllers

import javax.inject.{ Inject, Singleton }

import cats.instances.all._
import cats.kernel.Monoid
import cats.syntax.either._
import cats.syntax.traverse._
import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, Result }
import uk.gov.hmrc.gform.controllers.GformSession.userId

import play.api.mvc.{ Action, AnyContent, Result }
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.ValidationUtil._
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.service.{ DeleteService, RepeatingComponentService, RetrieveService, SaveService }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.service.{ RetrieveService, SaveService }
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.service.{ RetrieveService, SaveService }
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, SaveService }
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable
import scala.concurrent.{ ExecutionContext, Future }

@Singleton
class FormGen @Inject() (val messagesApi: MessagesApi, val sec: SecuredActions, repeatService: RepeatingComponentService, validationModule: ValidationModule, fileUploadModule: FileUploadModule)(implicit ec: ExecutionContext, authConnector: AuthConnector)
    extends FrontendController with I18nSupport {
  import GformSession._

  private lazy val validationService = validationModule.validationService
  private lazy val fileUploadService = fileUploadModule.fileUploadService
}

