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

package uk.gov.hmrc.gform.views.html

import org.scalatest._

class ViewsHelperSpec extends FlatSpec with Matchers {
  "localisation method" should "handle empty string" in {
    localisation("") should be("")
  }

  it should "handle simple string" in {
    localisation("abc") should be("abc")
  }

  it should "handle localised string" in {
    localisation("abc | def") should be("abc")
  }

  it should "handle string made only from delimiters" in {
    localisation("|||") should be("")
  }

  it should "handle string with more delimiters" in {
    localisation("a|b|c|d") should be("a")
  }

  "summaryTextArea method" should "handle escape html entities and replace new lines with <br/>" in {
    summaryTextArea("hello\r\nworld").body should be("hello<br/>world")
    summaryTextArea("\rhello\r\nworld\n").body should be("<br/>hello<br/>world<br/>")
    summaryTextArea("hello\rworld").body should be("hello<br/>world")
    summaryTextArea("hello\nworld").body should be("hello<br/>world")
    summaryTextArea("<b>hello\nworld</b>").body should be("&lt;b&gt;hello<br/>world&lt;/b&gt;")
  }
}
