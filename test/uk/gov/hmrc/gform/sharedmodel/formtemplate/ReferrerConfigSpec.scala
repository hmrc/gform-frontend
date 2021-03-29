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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import munit.{ FunSuite, Location }

class ReferrerConfigSpec extends FunSuite {

  def check(referrerUrlPatterns: List[String], referrer: String, expectedResult: Boolean, description: String)(implicit
    loc: Location
  ) =
    test("isAllowed should" + description) {
      assertEquals(
        ReferrerConfig(referrerUrlPatterns.map(ReferrerUrlPattern(_)), "")
          .isAllowed(referrer),
        expectedResult
      )
    }

  check(List.empty, "http://host", true, "return true when allowedReferrerUrls is empty")
  check(
    List("http://host"),
    "http://host",
    true,
    "return true when exact match"
  )
  check(
    List("http://*.host.com"),
    "http://www.host.com",
    true,
    "return true pattern match (host)"
  )
  check(
    List("http://host.com/*"),
    "http://host.com/path",
    true,
    "return true when pattern matches path"
  )

  check(
    List("http://host.com/*"),
    "http://host.com/path1/path2",
    true,
    "return true when pattern matches path(multi path)"
  )

  check(
    List("http://host-other.com/*", "http://host.com/*"),
    "http://host.com/path",
    true,
    "return true when pattern match path on at-least one pattern"
  )

  check(
    List("http://host1.com", "http://host2.com"),
    "http://host.com",
    false,
    "return false when no match (exact)"
  )

  check(
    List("http://host1.com/*", "http://host2.com/*"),
    "http://host.com/path",
    false,
    "return false when no match (pattern)"
  )

}
