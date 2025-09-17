/*
 * Copyright 2025 HM Revenue & Customs
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
import play.api.http.Status._
import play.api.mvc.{ AnyContent, MessagesControllerComponents, Request }
import play.api.test.{ FakeRequest, Helpers }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.http.{ BadRequestException, NotFoundException }

import java.io.File

class DownloadControllerSpec extends Spec {

  trait TestFixture {
    val messagesControllerComponents: MessagesControllerComponents = Helpers.stubMessagesControllerComponents()
    val environment: Environment = Environment.simple()

    val downloadController = new DownloadController(messagesControllerComponents, environment)(ec)
    val request: Request[AnyContent] = FakeRequest("GET", "/")
  }

  "DownloadController" should "successfully download a valid xlsx file" in new TestFixture {
    val filename = "test-file.xlsx"
    val testFile = new File(s"conf/resources/$filename")

    testFile.getParentFile.mkdirs()
    testFile.createNewFile()
    testFile.deleteOnExit()

    try {
      val future = downloadController.downloadFile(filename)(request)

      whenReady(future) { result =>
        result.header.status shouldBe OK
        result.header.headers.get("Content-Type") shouldBe Some(
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
        result.header.headers.get("Content-Disposition") shouldBe Some(s"""inline; filename="$filename"""")
        result.header.headers.get("Content-Length") shouldBe Some(testFile.length.toString)
      }
    } finally {
      val _ = testFile.delete()
    }
  }

  it should "reject empty filename" in new TestFixture {
    val filename = ""

    val future = downloadController.downloadFile(filename)(request)

    whenReady(future.failed) { exception =>
      exception shouldBe an[IllegalArgumentException]
      exception.getMessage should include("Download file: Invalid filename")
    }
  }

  it should "reject filename with path traversal sequences" in new TestFixture {
    val filenames = List(
      "../test.xlsx",
      "../../test.xlsx",
      "../../../etc/passwd",
      "..\\test.xlsx",
      "test/../other.xlsx"
    )

    filenames.foreach { filename =>
      val future = downloadController.downloadFile(filename)(request)

      whenReady(future.failed) { exception =>
        exception shouldBe an[IllegalArgumentException]
        exception.getMessage should include("Download file: Invalid filename")
      }
    }
  }

  it should "reject unsupported file extensions" in new TestFixture {
    val unsupportedFiles = List(
      "test.txt",
      "test.pdf",
      "test.doc",
      "test.exe",
      "test.sh",
      "test.bat",
      "test.jar",
      "test.zip",
      "test.conf",
      "test"
    )

    unsupportedFiles.foreach { filename =>
      val future = downloadController.downloadFile(filename)(request)

      whenReady(future.failed) { exception =>
        exception shouldBe an[BadRequestException]
        exception.getMessage should include("Download file: File type not supported")
      }
    }
  }

  it should "return 404 when file does not exist" in new TestFixture {
    val filename = "non-existent-file.xlsx"

    val future = downloadController.downloadFile(filename)(request)

    whenReady(future.failed) { exception =>
      exception shouldBe a[NotFoundException]
      exception.getMessage should include("Download file: File non-existent-file.xlsx does not exist")
    }
  }

  it should "handle case insensitive file extensions" in new TestFixture {
    val filename = "test-file.XLSX"
    val testFile = new File(s"conf/resources/$filename")

    testFile.getParentFile.mkdirs()
    testFile.createNewFile()
    testFile.deleteOnExit()

    try {
      val future = downloadController.downloadFile(filename)(request)

      whenReady(future) { result =>
        result.header.status shouldBe OK
        result.header.headers.get("Content-Type") shouldBe Some(
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
      }
    } finally {
      val _ = testFile.delete()
    }
  }

  it should "accept valid filenames with allowed characters" in new TestFixture {
    val validFilenames = List(
      "simple.xlsx",
      "file-name.xlsx",
      "file_name.xlsx",
      "file.name.xlsx",
      "123456.xlsx",
      "file123.xlsx"
    )

    validFilenames.foreach { filename =>
      val testFile = new File(s"conf/resources/$filename")
      testFile.getParentFile.mkdirs()
      testFile.createNewFile()
      testFile.deleteOnExit()

      try {
        val future = downloadController.downloadFile(filename)(request)

        whenReady(future) { result =>
          result.header.status shouldBe OK
        }
      } finally {
        val _ = testFile.delete()
      }
    }
  }

  it should "handle edge case where filename has no extension" in new TestFixture {
    val filename = "noextension"

    val future = downloadController.downloadFile(filename)(request)

    whenReady(future.failed) { exception =>
      exception shouldBe an[BadRequestException]
      exception.getMessage should include("Download file: File type not supported")
    }
  }
}
