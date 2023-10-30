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

package uk.gov.hmrc.gform.gform

import play.api.Environment
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate.LinkCtx
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.nio.file.Paths
import scala.concurrent.{ Future }

class ImageController(
  messagesControllerComponents: MessagesControllerComponents,
  auth: AuthenticatedRequestActions,
  environment: Environment
) extends FrontendController(messagesControllerComponents) {

  def image(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    fileName: String
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ViewImageByInternalLink
    ) { _ => _ => _ => _ => formModelOptics =>
      val formModel = formModelOptics.formModelRenderPageOptics.formModel
      val allExprs = formModel.brackets.toBracketsPlains.toList.flatMap(_.allExprs(formModel))

      if (!allExprs.contains(LinkCtx(InternalLink.Image(fileName)))) {
        Future.failed(
          new NotFoundException(
            s"link.image.$fileName expr does not exist in $formTemplateId form"
          )
        )
      } else {
        val file = environment.getFile(s"conf/resources/$fileName")
        if (file.exists()) {
          val path = Paths.get(file.getAbsolutePath)
          val image = java.nio.file.Files.readAllBytes(path)
          Future.successful(
            Ok(image).as("image/png")
          )
        } else {
          Future.failed(new NotFoundException(s"File $fileName does not exist"))
        }
      }
    }
}
