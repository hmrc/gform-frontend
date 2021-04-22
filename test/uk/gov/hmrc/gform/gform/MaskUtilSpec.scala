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

package uk.gov.hmrc.gform.gform

import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.gform.MaskUtil.maskEmail

class MaskUtilSpec extends FlatSpec with Matchers {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  "maskEmail" should "mark the latter half of the username part" in {

    val table = Table(
      ("input", "expected", "description"),
      ("a@b.com", "*@b.com", "Single char in user name"),
      ("ab@c.com", "a*@c.com", "Multiple chars in user name"),
      ("@a.com", "@a.com", "No chars in user name"),
      ("a", "a", "No @ symbol"),
      ("", "", "Empty email")
    )

    forAll(table) { (input, expected, description) =>
      logger.info(description)
      maskEmail(input) shouldBe expected
    }
  }
}
