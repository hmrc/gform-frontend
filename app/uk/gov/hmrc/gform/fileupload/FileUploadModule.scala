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

package uk.gov.hmrc.gform.fileupload

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class FileUploadModule(
  wSHttpModule: WSHttpModule,
  configModule: ConfigModule
)(implicit
  ec: ExecutionContext
) {

  private val fileUploadBaseUrl = {
    val baseUrl = configModule.serviceConfig.baseUrl("file-upload")
    val pathPrefix = configModule.serviceConfig.getConfString("file-upload.path-prefix", "")
    baseUrl + pathPrefix + "/file-upload"
  }

  private val fileUploadFrontendBaseUrl = {
    val baseUrl = configModule.serviceConfig.baseUrl("file-upload-frontend")
    baseUrl + "/file-upload"
  }

  private val fileUploadConnector = new FileUploadConnector(wSHttpModule.auditableWSHttp, fileUploadBaseUrl)

  private val fileUploadFrontendConnector =
    new FileUploadFrontendConnector(wSHttpModule.auditableWSHttp, fileUploadFrontendBaseUrl)

  val fileUploadService = new FileUploadService(fileUploadConnector)

  val fileUploadFrontendService = new FileUploadFrontendService(fileUploadFrontendConnector)

}
