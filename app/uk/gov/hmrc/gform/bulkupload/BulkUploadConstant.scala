/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.bulkupload

import java.time.format.DateTimeFormatter

object BulkUploadConstant {

  val bulkUploadDateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("d/M/yyyy")

  val BULK_UPLOAD_REMOVE_ROW = "bulk.upload.row.remove"

  val BULK_UPLOAD_TEXT = "bulk.upload.text"
  val BULK_UPLOAD_NUMBER = "bulk.upload.number"

  val BULK_UPLOAD_DAY = "bulk.upload.day"
  val BULK_UPLOAD_MONTH = "bulk.upload.month"
  val BULK_UPLOAD_YEAR = "bulk.upload.year"
}
