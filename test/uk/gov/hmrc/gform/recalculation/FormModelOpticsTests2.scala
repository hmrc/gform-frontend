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

package uk.gov.hmrc.gform.recalculation

import uk.gov.hmrc.auth.core.retrieve.ItmpAddress
import uk.gov.hmrc.gform.auth.models.{ ItmpRetrievals, OtherRetrievals }
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroup, DataRetrieve }
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.{ Many, One }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField, QueryParamValue, QueryParams, ThirdPartyData }
import uk.gov.hmrc.gform.recalculation.EvaluationStatus._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, QueryParam }

object FormModelOpticsTests2 extends DependencyGraphFixture {

  val data = List(
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty.copy(retrievals =
        authenticatedRetrievals
          .copy(affinityGroup = AffinityGroup.Individual, otherRetrievals = OtherRetrievals(Some("user@test.com")))
      ),
      AnswerMap(
        "agentEmail"        -> Hidden,
        "emailAddressAgent" -> Hidden
      ),
      List(
        "email = user@test.com"
      ),
      "or-else-2.json individual"
    ),
    (
      MongoUserData(
        "agentEmail"        -> Many(List("Different")),
        "emailAddressAgent" -> One("agent@test.com")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(retrievals =
        authenticatedRetrievals
          .copy(affinityGroup = AffinityGroup.Agent, otherRetrievals = OtherRetrievals(Some("user@test.com")))
      ),
      AnswerMap(
        "agentEmail"        -> OptionResult(List("Different")),
        "emailAddressAgent" -> StringResult("agent@test.com")
      ),
      List(
        "To user@test.com",
        "user@test.com",
        "email = agent@test.com"
      ),
      "or-else-2.json agent"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_item"       -> Many(List("game")),
        "2_addAnother" -> Many(List("0")),
        "2_item"       -> Many(List("film")),
        "3_addAnother" -> Many(List("1")),
        "3_item"       -> Many(List("none"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_item" -> OptionResult(List("game")),
        "2_item" -> OptionResult(List("film")),
        "3_item" -> OptionResult(List("none")),
        "secret" -> Hidden
      ),
      List(
        "numbered list choices selected: <ol class=\"govuk-list govuk-list--number\"><li>Film</li><li>None</li></ol>",
        """|<p>numbered list choices selected:</p>
           |<ol class="govuk-list govuk-list--number">
           | <li>Film</li>
           | <li>None</li>
           |</ol>
           |<p></p>""".stripMargin,
        "bulleted list choices selected: <ul class=\"govuk-list govuk-list--bullet\"><li>Film</li><li>None</li></ul>",
        """|<p>bulleted list choices selected:</p>
           |<ul class="govuk-list govuk-list--bullet">
           | <li>Film</li>
           | <li>None</li>
           |</ul>
           |<p></p>""".stripMargin,
        "1 - Game",
        "<p>1 - Game</p>",
        "numbered list choices selected: <ol class=\"govuk-list govuk-list--number\"><li>Game</li><li>None</li></ol>",
        """|<p>numbered list choices selected:</p>
           |<ol class="govuk-list govuk-list--number">
           | <li>Game</li>
           | <li>None</li>
           |</ol>
           |<p></p>""".stripMargin,
        "bulleted list choices selected: <ul class=\"govuk-list govuk-list--bullet\"><li>Game</li><li>None</li></ul>",
        """|<p>bulleted list choices selected:</p>
           |<ul class="govuk-list govuk-list--bullet">
           | <li>Game</li>
           | <li>None</li>
           |</ul>
           |<p></p>""".stripMargin,
        "2 - Film",
        "<p>2 - Film</p>",
        "numbered list choices selected: <ol class=\"govuk-list govuk-list--number\"><li>Game</li><li>Film</li></ol>",
        """|<p>numbered list choices selected:</p>
           |<ol class="govuk-list govuk-list--number">
           | <li>Game</li>
           | <li>Film</li>
           |</ol>
           |<p></p>""".stripMargin,
        "bulleted list choices selected: <ul class=\"govuk-list govuk-list--bullet\"><li>Game</li><li>Film</li></ul>",
        """|<p>bulleted list choices selected:</p>
           |<ul class="govuk-list govuk-list--bullet">
           | <li>Game</li>
           | <li>Film</li>
           |</ul>
           |<p></p>""".stripMargin,
        "3 - None",
        "<p>3 - None</p>",
        "numbered list choices selected outside ATL: <ol class=\"govuk-list govuk-list--number\"><li>Game</li><li>Film</li><li>None</li></ol>",
        """|<p>numbered list choices selected outside ATL:</p>
           |<ol class="govuk-list govuk-list--number">
           | <li>Game</li>
           | <li>Film</li>
           | <li>None</li>
           |</ol>
           |<p></p>""".stripMargin,
        "bulleted list choices selected outside ATL: <ul class=\"govuk-list govuk-list--bullet\"><li>Game</li><li>Film</li><li>None</li></ul>",
        """|<p>bulleted list choices selected outside ATL:</p>
           |<ul class="govuk-list govuk-list--bullet">
           | <li>Game</li>
           | <li>Film</li>
           | <li>None</li>
           |</ul>
           |<p></p>""".stripMargin
      ),
      "numbered-bulleted-list-choices-selected.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_item"       -> One("Car"),
        "2_addAnother" -> Many(List("0")),
        "2_item"       -> One("Page"),
        "3_addAnother" -> Many(List("1")),
        "3_item"       -> One("Cat")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_item" -> StringResult("Car"),
        "2_item" -> StringResult("Page"),
        "3_item" -> StringResult("Cat"),
        "secret" -> Hidden
      ),
      List(
        "1 - Car",
        "<p>1 - Car</p>",
        "2 - Page",
        "<p>2 - Page</p>",
        "3 - Cat",
        "<p>3 - Cat</p>",
        "numbered list: <ol class=\"govuk-list govuk-list--number\"><li>Car</li><li>Page</li><li>Cat</li></ol>",
        """|<p>numbered list:</p>
           |<ol class="govuk-list govuk-list--number">
           | <li>Car</li>
           | <li>Page</li>
           | <li>Cat</li>
           |</ol>
           |<p></p>""".stripMargin,
        "bulleted list: <ul class=\"govuk-list govuk-list--bullet\"><li>Car</li><li>Page</li><li>Cat</li></ul>",
        """|<p>bulleted list:</p>
           |<ul class="govuk-list govuk-list--bullet">
           | <li>Car</li>
           | <li>Page</li>
           | <li>Cat</li>
           |</ul>
           |<p></p>""".stripMargin
      ),
      "numbered-bulleted-list.json Generated"
    ),
    (
      MongoUserData(
        "1_idType"      -> Many(List("other")),
        "1_otherType"   -> One("EPASS"),
        "1_partnerList" -> Many(List("0")),
        "1_secret"      -> Many(List("reveal")),
        "2_idType"      -> Many(List("pass")),
        "2_partnerList" -> Many(List("1"))
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_idType"    -> OptionResult(List("other")),
        "1_otherType" -> StringResult("EPASS"),
        "1_secret"    -> OptionResult(List("reveal")),
        "2_idType"    -> OptionResult(List("pass")),
        "2_otherType" -> Hidden,
        "2_secret"    -> Hidden
      ),
      List(
        "Other - EPASS",
        "1 - Other - EPASS - other",
        "<p>1 - Other - EPASS - other</p>",
        "Passport",
        "2 - Passport - pass",
        "<p>2 - Passport - pass</p>"
      ),
      "choices-revealed-field.json Generated"
    ),
    (
      MongoUserData(
        "op1" -> One("1.23"),
        "op2" -> One("2.36")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "op1"        -> NumberResult(1.23),
        "op2"        -> NumberResult(2.36),
        "secretMul1" -> Hidden,
        "secretMul2" -> Hidden,
        "secretDiv1" -> Empty,
        "secretDiv2" -> Hidden
      ),
      List(
        "1.23 + 2.36 = 3.59",
        "2.36 + 1.23 = 3.59",
        "1.23 - 2.36 = -1.13",
        "2.36 - 1.23 = 1.13",
        "1.23 * 2.36 = 2.9",
        "2.36 * 1.23 = 2.9",
        "1.23 / 2.36 = 0.52",
        "2.36 / 1.23 = 1.91",
        "2.36 * 1.23 (Up) = £2.91",
        "2.36 * 1.23 (Down) = £2.90",
        "2.36 * 1.23 (Ceiling) = £2.91",
        "2.36 * 1.23 (Floor) = £2.90",
        "2.36 * 1.23 (HalfEven) = £2.90",
        "2.36 * 1.23 (HalfUp) = £2.90",
        "2.36 * 1.23 (HalfDown) = £2.90",
        "2.36 / 1.23 (Up) = £0.53",
        "2.36 / 1.23 (Down) = £0.52",
        "2.36 / 1.23 (Ceiling) = £0.53",
        "2.36 / 1.23 (Floor) = £0.52",
        "2.36 / 1.23 (HalfEven) = £0.52",
        "2.36 / 1.23 (HalfUp) = £0.52",
        "2.36 / 1.23 (HalfDown) = £0.52"
      ),
      "arithmetic.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_employer"   -> One("Foo"),
        "2_addAnother" -> Many(List("0")),
        "2_employer"   -> One("Bar"),
        "3_addAnother" -> Many(List("1")),
        "3_employer"   -> One("Baz"),
        "date-day"     -> One("1"),
        "date-month"   -> One("1"),
        "date-year"    -> One("2000"),
        "dummy"        -> One("dummy"),
        "nino"         -> One("AA333333A"),
        "selectedEmployer" -> Many(
          List("employer_1", "employer_2", "employer_3", "employments_0", "employments_1", "employments_2", "other")
        ),
        "visibilityOptions" -> Many(List("showAll"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "n2",
        "n3",
        "n4"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer" -> StringResult("Foo"),
        "2_employer" -> StringResult("Bar"),
        "3_employer" -> StringResult("Baz"),
        "selectedEmployer" -> OptionResult(
          List("employer_1", "employer_2", "employer_3", "employments_0", "employments_1", "employments_2", "other")
        ),
        "visibilityOptions" -> OptionResult(List("showAll")),
        "secret"            -> Empty
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Foo",
        "Bar",
        "Baz",
        "",
        "Choices selected: 7",
        "<p>Choices selected: 7</p>"
      ),
      "choices-selected-conditional.json show all options"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_employer"   -> One("Foo"),
        "2_addAnother" -> Many(List("0")),
        "2_employer"   -> One("Bar"),
        "3_addAnother" -> Many(List("1")),
        "3_employer"   -> One("Baz"),
        "date-day"     -> One("1"),
        "date-month"   -> One("1"),
        "date-year"    -> One("2000"),
        "dummy"        -> One("dummy"),
        "nino"         -> One("AA333333A"),
        "selectedEmployer" -> Many(
          List("employments_0", "employments_1", "employments_2", "other")
        ),
        "visibilityOptions" -> Many(List("hideAtlBased"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "n2",
        "n3",
        "n4"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer" -> StringResult("Foo"),
        "2_employer" -> StringResult("Bar"),
        "3_employer" -> StringResult("Baz"),
        "selectedEmployer" -> OptionResult(
          List("employments_0", "employments_1", "employments_2", "other")
        ),
        "visibilityOptions" -> OptionResult(List("hideAtlBased")),
        "secret"            -> Hidden
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "",
        "Choices selected: 4",
        "<p>Choices selected: 4</p>"
      ),
      "choices-selected-conditional.json hide atl based options"
    ),
    (
      MongoUserData(
        "1_addAnother"      -> Many(List("0")),
        "1_employer"        -> One("Foo"),
        "2_addAnother"      -> Many(List("0")),
        "2_employer"        -> One("Bar"),
        "3_addAnother"      -> Many(List("1")),
        "3_employer"        -> One("Baz"),
        "date-day"          -> One("1"),
        "date-month"        -> One("1"),
        "date-year"         -> One("2026"),
        "nino"              -> One("AA222222A"),
        "visibilityOptions" -> Many(List("showAll"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employments" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Smith Holdings"
                )
              )
          )
        )
      ),
      AnswerMap(
        "secret"            -> Empty,
        "selectedEmployer"  -> Empty,
        "1_employer"        -> StringResult("Foo"),
        "2_employer"        -> StringResult("Bar"),
        "3_employer"        -> StringResult("Baz"),
        "visibilityOptions" -> OptionResult(List("showAll"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Foo",
        "Bar",
        "Baz",
        "Acme",
        "Smith Holdings",
        "Choices count: 6",
        "<p>Choices count: 6</p>"
      ),
      "choices-count-conditional.json show atl based, show data retrieve based, show other"
    ),
    (
      MongoUserData(
        "1_addAnother"      -> Many(List("0")),
        "1_employer"        -> One("Foo"),
        "2_addAnother"      -> Many(List("0")),
        "2_employer"        -> One("Bar"),
        "3_addAnother"      -> Many(List("1")),
        "3_employer"        -> One("Baz"),
        "date-day"          -> One("1"),
        "date-month"        -> One("1"),
        "date-year"         -> One("2026"),
        "nino"              -> One("AA222222A"),
        "visibilityOptions" -> Many(List("hideAtlBased"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employments" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Smith Holdings"
                )
              )
          )
        )
      ),
      AnswerMap(
        "secret"            -> Hidden,
        "selectedEmployer"  -> Empty,
        "1_employer"        -> StringResult("Foo"),
        "2_employer"        -> StringResult("Bar"),
        "3_employer"        -> StringResult("Baz"),
        "visibilityOptions" -> OptionResult(List("hideAtlBased"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Acme",
        "Smith Holdings",
        "Choices count: 3",
        "<p>Choices count: 3</p>"
      ),
      "choices-count-conditional.json hide atl based, show data retrieve based, show other"
    ),
    (
      MongoUserData(
        "1_addAnother"      -> Many(List("0")),
        "1_employer"        -> One("Foo"),
        "2_addAnother"      -> Many(List("0")),
        "2_employer"        -> One("Bar"),
        "3_addAnother"      -> Many(List("1")),
        "3_employer"        -> One("Baz"),
        "date-day"          -> One("1"),
        "date-month"        -> One("1"),
        "date-year"         -> One("2026"),
        "nino"              -> One("AA222222A"),
        "visibilityOptions" -> Many(List("hideDataRetrieveBased"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employments" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Smith Holdings"
                )
              )
          )
        )
      ),
      AnswerMap(
        "secret"            -> Hidden,
        "selectedEmployer"  -> Empty,
        "1_employer"        -> StringResult("Foo"),
        "2_employer"        -> StringResult("Bar"),
        "3_employer"        -> StringResult("Baz"),
        "visibilityOptions" -> OptionResult(List("hideDataRetrieveBased"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Foo",
        "Bar",
        "Baz",
        "Choices count: 4",
        "<p>Choices count: 4</p>"
      ),
      "choices-count-conditional.json show atl based, hide data retrieve based, show other"
    ),
    (
      MongoUserData(
        "1_addAnother"      -> Many(List("0")),
        "1_employer"        -> One("Foo"),
        "2_addAnother"      -> Many(List("0")),
        "2_employer"        -> One("Bar"),
        "3_addAnother"      -> Many(List("1")),
        "3_employer"        -> One("Baz"),
        "date-day"          -> One("1"),
        "date-month"        -> One("1"),
        "date-year"         -> One("2026"),
        "nino"              -> One("AA222222A"),
        "visibilityOptions" -> Many(List("hideOther"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employments" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Smith Holdings"
                )
              )
          )
        )
      ),
      AnswerMap(
        "secret"            -> Hidden,
        "selectedEmployer"  -> Empty,
        "1_employer"        -> StringResult("Foo"),
        "2_employer"        -> StringResult("Bar"),
        "3_employer"        -> StringResult("Baz"),
        "visibilityOptions" -> OptionResult(List("hideOther"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Foo",
        "Bar",
        "Baz",
        "Acme",
        "Smith Holdings",
        "Choices count: 5",
        "<p>Choices count: 5</p>"
      ),
      "choices-count-conditional.json show atl based, show data retrieve based, hide other"
    ),
    (
      MongoUserData(
        "1_addAnother"      -> Many(List("0")),
        "1_employer"        -> One("Foo"),
        "2_addAnother"      -> Many(List("0")),
        "2_employer"        -> One("Bar"),
        "3_addAnother"      -> Many(List("1")),
        "3_employer"        -> One("Baz"),
        "date-day"          -> One("1"),
        "date-month"        -> One("1"),
        "date-year"         -> One("2026"),
        "nino"              -> One("AA222222A"),
        "visibilityOptions" -> Many(List("hideAtlBased", "hideDataRetrieveBased"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employments" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Smith Holdings"
                )
              )
          )
        )
      ),
      AnswerMap(
        "secret"            -> Hidden,
        "selectedEmployer"  -> Empty,
        "1_employer"        -> StringResult("Foo"),
        "2_employer"        -> StringResult("Bar"),
        "3_employer"        -> StringResult("Baz"),
        "visibilityOptions" -> OptionResult(List("hideAtlBased", "hideDataRetrieveBased"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Choices count: 1",
        "<p>Choices count: 1</p>"
      ),
      "choices-count-conditional.json hide atl based, hide data retrieve based, show other"
    ),
    (
      MongoUserData(
        "1_addAnother"     -> Many(List("0")),
        "1_employer"       -> One("Star"),
        "2_addAnother"     -> Many(List("0")),
        "2_employer"       -> One("Click"),
        "3_addAnother"     -> Many(List("1")),
        "3_employer"       -> One("Visit"),
        "selectedEmployer" -> Many(List("employer_2"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"       -> StringResult("Star"),
        "2_employer"       -> StringResult("Click"),
        "3_employer"       -> StringResult("Visit"),
        "selectedEmployer" -> OptionResult(List("employer_2"))
      ),
      List(
        "1 - Star",
        "<p>1 - Star</p>",
        "2 - Click",
        "<p>2 - Click</p>",
        "3 - Visit",
        "<p>3 - Visit</p>",
        "Star",
        "Click",
        "Visit",
        "Choices count: 4",
        "<p>Choices count: 4</p>"
      ),
      "choices-count-atl-based.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"   -> Many(List("0")),
        "1_typeOfIncome" -> Many(List("tips")),
        "2_addAnother"   -> Many(List("0")),
        "2_typeOfIncome" -> Many(List("commission", "otherEarned")),
        "3_addAnother"   -> Many(List("1")),
        "3_typeOfIncome" -> Many(List("otherNotEarned"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        // "1_addAnother"   -> StringResult("0"),
        // "1_typeOfIncome" -> StringResult("tips"),
        // "2_addAnother"   -> StringResult("0"),
        // "2_typeOfIncome" -> StringResult("commission,otherEarned"),
        // "3_addAnother"   -> StringResult("1"),
        // "3_typeOfIncome" -> StringResult("otherNotEarned")
      ),
      List(
        "1 - Tips - 0 - 1 - 4",
        "<p>1 - Tips - 0 - 1 - 4</p>",
        "2 - Commission, Other income (earned) - 0 - 2 - 4",
        "<p>2 - Commission, Other income (earned) - 0 - 2 - 4</p>",
        "3 - Other income (not earned) - 0 - 1 - 4",
        "<p>3 - Other income (not earned) - 0 - 1 - 4</p>"
      ),
      "choices-selected.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnotherAddress" -> Many(List("0")),
        "1_overseasAddress"   -> One("Czech Republic"),
        "2_addAnotherAddress" -> Many(List("1")),
        "2_overseasAddress"   -> One("India")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty.copy(
        lookupRegistry = lookupRegistry
      ),
      AnswerMap(
        "secret"            -> Hidden,
        "1_overseasAddress" -> StringResult("Czech Republic"),
        "2_overseasAddress" -> StringResult("India")
      ),
      List(
        "Czech Republic",
        "Address 1: Czech Republic",
        "<p>Address 1: Czech Republic</p>",
        "India",
        "Address 2: India",
        "<p>Address 2: India</p>",
        "EU count: 1",
        "<p>EU count: 1</p>"
      ),
      "column-count.json secret is hidden"
    ),
    (
      MongoUserData(
        "1_addAnotherAddress" -> Many(List("0")),
        "1_overseasAddress"   -> One("Czech Republic"),
        "2_addAnotherAddress" -> Many(List("0")),
        "2_overseasAddress"   -> One("France"),
        "3_addAnotherAddress" -> Many(List("1")),
        "3_overseasAddress"   -> One("India")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3"
      ),
      EvaluationContext.empty.copy(
        lookupRegistry = lookupRegistry
      ),
      AnswerMap(
        "secret"            -> Empty,
        "1_overseasAddress" -> StringResult("Czech Republic"),
        "2_overseasAddress" -> StringResult("France"),
        "3_overseasAddress" -> StringResult("India")
      ),
      List(
        "Czech Republic",
        "Address 1: Czech Republic",
        "<p>Address 1: Czech Republic</p>",
        "France",
        "Address 2: France",
        "<p>Address 2: France</p>",
        "India",
        "Address 3: India",
        "<p>Address 3: India</p>",
        "EU count: 2",
        "<p>EU count: 2</p>"
      ),
      "column-count.json secret is visible"
    ),
    (
      MongoUserData(
        "overseasAddress-city"     -> One("Town"),
        "overseasAddress-country"  -> One("France"),
        "overseasAddress-line1"    -> One("Line 1"),
        "overseasAddress-line2"    -> One(""),
        "overseasAddress-line3"    -> One(""),
        "overseasAddress-postcode" -> One("34567")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "overseasAddress-city"     -> StringResult("Town"),
        "overseasAddress-country"  -> StringResult("France"),
        "overseasAddress-line1"    -> StringResult("Line 1"),
        "overseasAddress-line2"    -> StringResult(""),
        "overseasAddress-line3"    -> StringResult(""),
        "overseasAddress-postcode" -> StringResult("34567"),
        "secret"                   -> Hidden
      ),
      List(
        "Line 1: LINE 1",
        "<p>Line 1: LINE 1</p>"
      ),
      "address-lens-visibility-overseas-address.json secret is hidden"
    ),
    (
      MongoUserData(
        "overseasAddress-city"     -> One("Town"),
        "overseasAddress-country"  -> One("France"),
        "overseasAddress-line1"    -> One("Line 1"),
        "overseasAddress-line2"    -> One(""),
        "overseasAddress-line3"    -> One(""),
        "overseasAddress-postcode" -> One("ZZ3456")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "overseasAddress-city"     -> StringResult("Town"),
        "overseasAddress-country"  -> StringResult("France"),
        "overseasAddress-line1"    -> StringResult("Line 1"),
        "overseasAddress-line2"    -> StringResult(""),
        "overseasAddress-line3"    -> StringResult(""),
        "overseasAddress-postcode" -> StringResult("ZZ3456"),
        "secret"                   -> Empty
      ),
      List(
        "Line 1: LINE 1",
        "<p>Line 1: LINE 1</p>"
      ),
      "address-lens-visibility-overseas-address.json secret is visible"
    ),
    (
      MongoUserData(
        "address-country"  -> One(""),
        "address-postcode" -> One("AA11 1AA"),
        "address-street1"  -> One("Line 1"),
        "address-street2"  -> One(""),
        "address-street3"  -> One(""),
        "address-street4"  -> One(""),
        "address-uk"       -> One("true")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "address-country"  -> StringResult(""),
        "address-postcode" -> StringResult("AA11 1AA"),
        "address-street1"  -> StringResult("Line 1"),
        "address-street2"  -> StringResult(""),
        "address-street3"  -> StringResult(""),
        "address-street4"  -> StringResult(""),
        "address-uk"       -> StringResult("true"),
        "secret"           -> Hidden
      ),
      List(
        "Line 1: LINE 1",
        "<p>Line 1: LINE 1</p>"
      ),
      "address-lens-visibility-address.json secret is hidden"
    ),
    (
      MongoUserData(
        "address-country"  -> One(""),
        "address-postcode" -> One("ZZ11 1AA"),
        "address-street1"  -> One("Line 1"),
        "address-street2"  -> One(""),
        "address-street3"  -> One(""),
        "address-street4"  -> One(""),
        "address-uk"       -> One("true")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "address-country"  -> StringResult(""),
        "address-postcode" -> StringResult("ZZ11 1AA"),
        "address-street1"  -> StringResult("Line 1"),
        "address-street2"  -> StringResult(""),
        "address-street3"  -> StringResult(""),
        "address-street4"  -> StringResult(""),
        "address-uk"       -> StringResult("true"),
        "secret"           -> Empty
      ),
      List(
        "Line 1: LINE 1",
        "<p>Line 1: LINE 1</p>"
      ),
      "address-lens-visibility-address.json secret is visible"
    ),
    (
      MongoUserData(
        "overseasAddress-city"     -> One("Town"),
        "overseasAddress-country"  -> One("France"),
        "overseasAddress-line1"    -> One("Line 1"),
        "overseasAddress-line2"    -> One(""),
        "overseasAddress-line3"    -> One(""),
        "overseasAddress-postcode" -> One("")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(
        lookupRegistry = lookupRegistry
      ),
      AnswerMap(
        "overseasAddress-city"     -> StringResult("Town"),
        "overseasAddress-country"  -> StringResult("France"),
        "overseasAddress-line1"    -> StringResult("Line 1"),
        "overseasAddress-line2"    -> StringResult(""),
        "overseasAddress-line3"    -> StringResult(""),
        "overseasAddress-postcode" -> StringResult(""),
        "secret"                   -> Empty
      ),
      List(
        "EU residency: Yes",
        "<p>EU residency: Yes</p>"
      ),
      "column-overseas-address.json address in EU"
    ),
    (
      MongoUserData(
        "overseasAddress-city"     -> One("Town"),
        "overseasAddress-country"  -> One("India"),
        "overseasAddress-line1"    -> One("Line 1"),
        "overseasAddress-line2"    -> One(""),
        "overseasAddress-line3"    -> One(""),
        "overseasAddress-postcode" -> One("")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(
        lookupRegistry = lookupRegistry
      ),
      AnswerMap(
        "overseasAddress-city"     -> StringResult("Town"),
        "overseasAddress-country"  -> StringResult("India"),
        "overseasAddress-line1"    -> StringResult("Line 1"),
        "overseasAddress-line2"    -> StringResult(""),
        "overseasAddress-line3"    -> StringResult(""),
        "overseasAddress-postcode" -> StringResult(""),
        "secret"                   -> Hidden
      ),
      List(
        "EU residency: No",
        "<p>EU residency: No</p>"
      ),
      "column-overseas-address.json address not in EU"
    ),
    (
      MongoUserData(
        "dutyClaimBack" -> One("12.00"),
        "vatClaimBack"  -> One("56.78")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "dutyClaimBack" -> NumberResult(12),
        "readonly"      -> StringResult("12"),
        "secret"        -> Empty
      ),
      List(
        "£12",
        "£56.78",
        "£68.78",
        "£68.78"
      ),
      "hide-zero-decimals.json handle double"
    ),
    (
      MongoUserData(
        "dutyClaimBack" -> One("12"),
        "vatClaimBack"  -> One("56.78")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "dutyClaimBack" -> NumberResult(12),
        "readonly"      -> StringResult("12"),
        "secret"        -> Empty
      ),
      List(
        "£12",
        "£56.78",
        "£68.78",
        "£68.78"
      ),
      "hide-zero-decimals.json handle long"
    ),
    (
      MongoUserData(
        "address-country"               -> One(""),
        "address-postcode"              -> One("AA11 1AA"),
        "address-street1"               -> One("Address line 1"),
        "address-street2"               -> One("Address line 2"),
        "address-street3"               -> One("Address town"),
        "address-street4"               -> One("Address county"),
        "address-uk"                    -> One("true"),
        "overseasAddress-city"          -> One("Overseas address town"),
        "overseasAddress-country"       -> One("France"),
        "overseasAddress-line1"         -> One("Overseas address line 1"),
        "overseasAddress-line2"         -> One("Overseas address line 2"),
        "overseasAddress-line3"         -> One("Overseas address line 3"),
        "overseasAddress-postcode"      -> One("34567"),
        "postcodeLookup-filter"         -> One(""),
        "postcodeLookup-postcode"       -> One("ZZ9Z 9TT"),
        "postcodeLookupManual-postcode" -> One("AA11 1AA")
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          selectedAddresses = Some(
            Map(
              FormComponentId("postcodeLookup") -> "GB990091234558"
            )
          ),
          postcodeLookup = Some(
            Map(
              FormComponentId("postcodeLookup") -> mkAddressLookupResult(
                id = "GB990091234558",
                lines = List("1 Testing Lane"),
                town = "Royal Madeuptown",
                postcode = "ZZ9Z 9TT"
              )
            )
          ),
          enteredAddresses = Some(
            Map(
              FormComponentId("postcodeLookupManual") -> FormData(
                List(
                  FormField(FormComponentId("postcodeLookupManual-uk").modelComponentId, "true"),
                  FormField(FormComponentId("postcodeLookupManual-postcode").modelComponentId, "BB11 1BB"),
                  FormField(
                    FormComponentId("postcodeLookupManual-street1").modelComponentId,
                    "Postcode lookup manual line 1"
                  ),
                  FormField(
                    FormComponentId("postcodeLookupManual-street2").modelComponentId,
                    "Postcode lookup manual line 2"
                  ),
                  FormField(
                    FormComponentId("postcodeLookupManual-street3").modelComponentId,
                    "Postcode lookup manual town"
                  ),
                  FormField(FormComponentId("postcodeLookupManual-country").modelComponentId, "")
                )
              )
            )
          )
        )
      ),
      AnswerMap(
      ),
      List(
        "postcodeLookup.line1: 1 Testing Lane",
        "postcodeLookup.line2: ",
        "postcodeLookup.line3: ",
        "postcodeLookup.line4: Royal Madeuptown",
        "postcodeLookup.postcode: ZZ9Z 9TT",
        "postcodeLookup.country: ",
        "postcodeLookupManual.line1: Postcode lookup manual line 1",
        "postcodeLookupManual.line2: Postcode lookup manual line 2",
        "postcodeLookupManual.line3: Postcode lookup manual town",
        "postcodeLookupManual.line4: ",
        "postcodeLookupManual.postcode: BB11 1BB",
        "postcodeLookupManual.country: ",
        "address.line1: Address line 1",
        "address.line2: Address line 2",
        "address.line3: Address town",
        "address.line4: Address county",
        "address.postcode: AA11 1AA",
        "address.country: ",
        "overseasAddress.line1: Overseas address line 1",
        "overseasAddress.line2: Overseas address line 2",
        "overseasAddress.line3: Overseas address line 3",
        "overseasAddress.line4: Overseas address town",
        "overseasAddress.postcode: 34567",
        "overseasAddress.country: France"
      ),
      "address-lens.json Generated"
    ),
    (
      MongoUserData(
        "addressInUK"                      -> Many(List("yes")),
        "correspondenceAddressUK"          -> One(""),
        "correspondenceAddressUK-filter"   -> One(""),
        "correspondenceAddressUK-postcode" -> One("ZZ9Z 9TT")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          selectedAddresses = Some(
            Map(
              FormComponentId("correspondenceAddressUK") -> "GB990091234558"
            )
          ),
          postcodeLookup = Some(
            Map(
              FormComponentId("correspondenceAddressUK") -> mkAddressLookupResult(
                id = "GB990091234558",
                lines = List("1 Testing Lane"),
                town = "Royal Madeuptown",
                postcode = "ZZ9Z 9TT"
              )
            )
          )
        )
      ),
      AnswerMap(
        "addressInUK"                       -> OptionResult(List("yes")),
        "correspondenceAddressUK-filter"    -> Hidden,
        "correspondenceAddressUK-postcode"  -> Hidden,
        "correspondenceAddressINT-city"     -> Hidden,
        "correspondenceAddressINT-country"  -> Hidden,
        "correspondenceAddressINT-line1"    -> Hidden,
        "correspondenceAddressINT-line2"    -> Hidden,
        "correspondenceAddressINT-line3"    -> Hidden,
        "correspondenceAddressINT-postcode" -> Hidden,
        "correspondenceAddressUK-filter"    -> StringResult(""),
        "correspondenceAddressUK-postcode"  -> StringResult("ZZ9Z 9TT"),
        "secret"                            -> Empty
      ),
      List(
        "Line 1: 1 TESTING LANE",
        "<p>Line 1: 1 TESTING LANE</p>"
      ),
      "address-lens-visibility-postcode-lookup.json secret is visible"
    ),
    (
      MongoUserData(
        "addressInUK"                       -> Many(List("no")),
        "correspondenceAddressINT-city"     -> One("Town"),
        "correspondenceAddressINT-country"  -> One("France"),
        "correspondenceAddressINT-line1"    -> One("Line 1"),
        "correspondenceAddressINT-line2"    -> One("Line 2"),
        "correspondenceAddressINT-line3"    -> One("Line 3"),
        "correspondenceAddressINT-postcode" -> One("34567"),
        "dummy"                             -> One("AAA")
      ),
      List(
        "n0",
        "n2",
        "n3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "addressInUK"                       -> OptionResult(List("no")),
        "correspondenceAddressINT-city"     -> StringResult("Town"),
        "correspondenceAddressINT-country"  -> StringResult("France"),
        "correspondenceAddressINT-line1"    -> StringResult("Line 1"),
        "correspondenceAddressINT-line2"    -> StringResult("Line 2"),
        "correspondenceAddressINT-line3"    -> StringResult("Line 3"),
        "correspondenceAddressINT-postcode" -> StringResult("34567"),
        "correspondenceAddressUK-filter"    -> Hidden,
        "correspondenceAddressUK-postcode"  -> Hidden,
        "secret"                            -> Hidden
      ),
      List(
        "Line 1: LINE 1",
        "<p>Line 1: LINE 1</p>"
      ),
      "address-lens-visibility-postcode-lookup.json secret is hidden"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          queryParams = QueryParams(
            Map(
              QueryParam("requestType") -> QueryParamValue("2"),
              QueryParam("pspName")     -> QueryParamValue("psp-name"),
              QueryParam("psaName")     -> QueryParamValue("psa-name"),
              QueryParam("nino")        -> QueryParamValue("AA333333A"),
              QueryParam("utr")         -> QueryParamValue("2222222222")
            )
          )
        )
      ),
      AnswerMap(
        "secret" -> Hidden
      ),
      List(
        "user name: psp-name",
        "<p>user name: psp-name</p>",
        "param.utr: 2222222222",
        "<p>param.utr: 2222222222</p>",
        "param.nino: AA333333A",
        "<p>param.nino: AA333333A</p>"
      ),
      "query-params.json request type 2"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          queryParams = QueryParams(
            Map(
              QueryParam("requestType") -> QueryParamValue("3"),
              QueryParam("pspName")     -> QueryParamValue("psp-name"),
              QueryParam("psaName")     -> QueryParamValue("psa-name")
            )
          )
        )
      ),
      AnswerMap(
        "secret" -> Empty
      ),
      List(
        "user name: psa-name",
        "<p>user name: psa-name</p>"
      ),
      "query-params.json request type 3"
    ),
    (
      MongoUserData(
        "pageA"     -> One("1"),
        "fooChoice" -> Many(List("1"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> OptionResult(List("1")),
        "pageA"     -> NumberResult(1),
        "pageB"     -> Empty
      ),
      List.empty[String],
      "revealing-choice-hidden-option-index-based.json Generated" // This json template needs to be rejected on POST
    ),
    (
      MongoUserData(
        "pageA"     -> One("100"),
        "fooChoice" -> Many(List("1"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> Empty,
        "pageA"     -> NumberResult(100),
        "pageB"     -> Hidden
      ),
      List.empty[String],
      "revealing-choice-hidden-option-index-based.json Generated" // This json template needs to be rejected on POST
    ),
    (
      MongoUserData(
        "pageA"     -> One("100"),
        "fooChoice" -> Many(List("0", "1", "2"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> OptionResult(List("0", "2")),
        "pageA"     -> NumberResult(100),
        "pageB"     -> Hidden
      ),
      List.empty[String],
      "revealing-choice-hidden-option-index-based.json Generated" // This json template needs to be rejected on POST
    ),
    (
      MongoUserData(
        "crnRepresentative" -> One("11111111"),
        "dummy"             -> One("dummy"),
        "nationality"       -> One("Czech")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "companyRepresentative" ->
              Map(
                DataRetrieve.Attribute("sicCode1") -> "01220",
                DataRetrieve.Attribute("sicCode2") -> "10390",
                DataRetrieve.Attribute("sicCode3") -> "20110",
                DataRetrieve.Attribute("sicCode4") -> "46450"
              )
          )
        ),
        lookupRegistry = lookupRegistry
      ),
      AnswerMap(
        "getSicDescriptionSecret" -> Empty,
        "getCountrySecret"        -> Empty,
        "nationality"             -> StringResult("Czech")
      ),
      List(
        """|SicCode 1 - 01220 - Growing of tropical and subtropical fruits
           |
           |SicCode 2 - 10390 - Other processing and preserving of fruit and vegetables
           |
           |SicCode 3 - 20110 - Manufacture of industrial gases
           |
           |SicCode 4 - 46450 - Wholesale of perfume and cosmetics
           |
           |Nationality ID: N0530
           |
           |CountryCode: CZ
           |
           |getCountry: Czech Republic""".stripMargin,
        """<p>SicCode 1 - 01220 - Growing of tropical and subtropical fruits</p>
          |<p>SicCode 2 - 10390 - Other processing and preserving of fruit and vegetables</p>
          |<p>SicCode 3 - 20110 - Manufacture of industrial gases</p>
          |<p>SicCode 4 - 46450 - Wholesale of perfume and cosmetics</p>
          |<p>Nationality ID: N0530</p>
          |<p>CountryCode: CZ</p>
          |<p>getCountry: Czech Republic</p>""".stripMargin
      ),
      "lookup-ops.json show sicDescription secret page"
    ),
    (
      MongoUserData(
        "crnRepresentative" -> One("11111111"),
        "dummy"             -> One("dummy"),
        "nationality"       -> One("Czech")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "companyRepresentative" ->
              Map(
                DataRetrieve.Attribute("sicCode1") -> "10390",
                DataRetrieve.Attribute("sicCode2") -> "01220",
                DataRetrieve.Attribute("sicCode3") -> "20110",
                DataRetrieve.Attribute("sicCode4") -> "46450"
              )
          )
        ),
        lookupRegistry = lookupRegistry
      ),
      AnswerMap(
        "getSicDescriptionSecret" -> Hidden,
        "getCountrySecret"        -> Empty,
        "nationality"             -> StringResult("Czech")
      ),
      List(
        """|SicCode 1 - 10390 - Other processing and preserving of fruit and vegetables
           |
           |SicCode 2 - 01220 - Growing of tropical and subtropical fruits
           |
           |SicCode 3 - 20110 - Manufacture of industrial gases
           |
           |SicCode 4 - 46450 - Wholesale of perfume and cosmetics
           |
           |Nationality ID: N0530
           |
           |CountryCode: CZ
           |
           |getCountry: Czech Republic""".stripMargin,
        """<p>SicCode 1 - 10390 - Other processing and preserving of fruit and vegetables</p>
          |<p>SicCode 2 - 01220 - Growing of tropical and subtropical fruits</p>
          |<p>SicCode 3 - 20110 - Manufacture of industrial gases</p>
          |<p>SicCode 4 - 46450 - Wholesale of perfume and cosmetics</p>
          |<p>Nationality ID: N0530</p>
          |<p>CountryCode: CZ</p>
          |<p>getCountry: Czech Republic</p>""".stripMargin
      ),
      "lookup-ops.json hide sicDescription secret page"
    ),
    (
      MongoUserData(
        "crn"                     -> One("11111111"),
        "dummy"                   -> One("dummy"),
        "getSicDescriptionSecret" -> Many(List("reveal")),
        "nationality"             -> One("French")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "companyRepresentative" ->
              Map(
                DataRetrieve.Attribute("sicCode1") -> "01220",
                DataRetrieve.Attribute("sicCode2") -> "10390",
                DataRetrieve.Attribute("sicCode3") -> "20110",
                DataRetrieve.Attribute("sicCode4") -> "46450"
              )
          )
        ),
        lookupRegistry = lookupRegistry
      ),
      AnswerMap(
        "getSicDescriptionSecret" -> OptionResult(List("reveal")),
        "getCountrySecret"        -> Hidden,
        "nationality"             -> StringResult("French")
      ),
      List(
        """|SicCode 1 - 01220 - Growing of tropical and subtropical fruits
           |
           |SicCode 2 - 10390 - Other processing and preserving of fruit and vegetables
           |
           |SicCode 3 - 20110 - Manufacture of industrial gases
           |
           |SicCode 4 - 46450 - Wholesale of perfume and cosmetics
           |
           |Nationality ID: N0720
           |
           |CountryCode: FR
           |
           |getCountry: France""".stripMargin,
        """<p>SicCode 1 - 01220 - Growing of tropical and subtropical fruits</p>
          |<p>SicCode 2 - 10390 - Other processing and preserving of fruit and vegetables</p>
          |<p>SicCode 3 - 20110 - Manufacture of industrial gases</p>
          |<p>SicCode 4 - 46450 - Wholesale of perfume and cosmetics</p>
          |<p>Nationality ID: N0720</p>
          |<p>CountryCode: FR</p>
          |<p>getCountry: France</p>""".stripMargin
      ),
      "lookup-ops.json hide country secret page"
    ),
    (
      MongoUserData(
        "controlAddToList"          -> Many(List("show")),
        "1_addAnother"              -> Many(List("0")),
        "1_crn"                     -> One("11111111"),
        "1_dummy"                   -> One("AAA"),
        "1_getCountrySecret"        -> Many(List("reveal")),
        "1_getSicDescriptionSecret" -> Many(List("keepHidden")),
        "1_nationality"             -> One("Czech"),
        "2_addAnother"              -> Many(List("1")),
        "2_crn"                     -> One("33333333"),
        "2_dummy"                   -> One("BBB"),
        "2_nationality"             -> One("French")
      ),
      List(
        "n0",
        "ap1.1.0",
        "ap1.1.1",
        "ap1.1.2",
        "ap1.1.3",
        "ar1.1",
        "ap1.2.0",
        "ap1.2.1",
        "ar1.2",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "1_companyRepresentative" ->
              Map(
                DataRetrieve.Attribute("sicCode1") -> "01220",
                DataRetrieve.Attribute("sicCode2") -> "10390",
                DataRetrieve.Attribute("sicCode3") -> "20110",
                DataRetrieve.Attribute("sicCode4") -> "46450"
              ),
            "2_companyRepresentative" ->
              Map(
                DataRetrieve.Attribute("sicCode1") -> "20110",
                DataRetrieve.Attribute("sicCode2") -> "46450"
              )
          )
        ),
        lookupRegistry = lookupRegistry
      ),
      AnswerMap(
        "1_addAnother"              -> OptionResult(List("0")),
        "1_crn"                     -> StringResult("11111111"),
        "1_dummy"                   -> StringResult("AAA"),
        "1_getCountrySecret"        -> OptionResult(List("reveal")),
        "1_getSicDescriptionSecret" -> OptionResult(List("keepHidden")),
        "1_nationality"             -> StringResult("Czech"),
        "2_addAnother"              -> OptionResult(List("1")),
        "2_crn"                     -> StringResult("33333333"),
        "2_dummy"                   -> StringResult("BBB"),
        "2_getCountrySecret"        -> Hidden,
        "2_getSicDescriptionSecret" -> Hidden,
        "2_nationality"             -> StringResult("French"),
        "controlAddToList"          -> OptionResult(List("show")),
        "secretOutsideAtl"          -> Hidden
      ),
      List(
        """|SicCode 1 - 01220 - Growing of tropical and subtropical fruits
           |
           |SicCode 2 - 10390 - Other processing and preserving of fruit and vegetables
           |
           |SicCode 3 - 20110 - Manufacture of industrial gases
           |
           |SicCode D - 46450 - Wholesale of perfume and cosmetics""".stripMargin,
        """|<p>SicCode 1 - 01220 - Growing of tropical and subtropical fruits</p>
           |<p>SicCode 2 - 10390 - Other processing and preserving of fruit and vegetables</p>
           |<p>SicCode 3 - 20110 - Manufacture of industrial gases</p>
           |<p>SicCode D - 46450 - Wholesale of perfume and cosmetics</p>""".stripMargin,
        "Country code: CZ, Country: Czech Republic",
        "<p>Country code: CZ, Country: Czech Republic</p>",
        "1. Czech",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Czech</li>
           |</ol>""".stripMargin,
        """|SicCode 1 - 20110 - Manufacture of industrial gases
           |
           |SicCode 2 - 46450 - Wholesale of perfume and cosmetics""".stripMargin,
        """|<p>SicCode 1 - 20110 - Manufacture of industrial gases</p>
           |<p>SicCode 2 - 46450 - Wholesale of perfume and cosmetics</p>""".stripMargin,
        "Country code: FR, Country: France",
        "<p>Country code: FR, Country: France</p>",
        "2. French",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>French</li>
           |</ol>""".stripMargin,
        "Sic codes A: 01220, 20110, Sic descriptions: 01220 - Growing of tropical and subtropical fruits, 20110 - Manufacture of industrial gases",
        "<p>Sic codes A: 01220, 20110, Sic descriptions: 01220 - Growing of tropical and subtropical fruits, 20110 - Manufacture of industrial gases</p>",
        "Country code: CZ, FR, Country: Czech Republic, France",
        "<p>Country code: CZ, FR, Country: Czech Republic, France</p>"
      ),
      "lookup-ops-atl.json add to list is visible"
    ),
    (
      MongoUserData(
        "controlAddToList" -> Many(List("skip")),
        "outsideDummy"     -> One("Outside"),
        "secretOutsideAtl" -> One("Secret found")
      ),
      List(
        "n0",
        "n2",
        "n3"
      ),
      EvaluationContext.empty.copy(
        lookupRegistry = lookupRegistry
      ),
      AnswerMap(
        "1_addAnother"              -> Hidden,
        "1_crn"                     -> Hidden,
        "1_dummy"                   -> Hidden,
        "1_getCountrySecret"        -> Hidden,
        "1_getSicDescriptionSecret" -> Hidden,
        "1_nationality"             -> Hidden,
        "controlAddToList"          -> OptionResult(List("skip")),
        "secretOutsideAtl"          -> StringResult("Secret found")
      ),
      List(
        "Sic codes A: , Sic descriptions: ",
        "<p>Sic codes A: , Sic descriptions:</p>",
        "Country code: , Country: ",
        "<p>Country code: , Country:</p>"
      ),
      "lookup-ops-atl.json add to list is hidden"
    ),
    (
      MongoUserData(
        "date-day"   -> One("1"),
        "date-month" -> One("2"),
        "date-year"  -> One("2000"),
        "nino"       -> One("AA333333A")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employments" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Smith Holdings"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Foldright"
                )
              )
          )
        )
      ),
      AnswerMap(),
      List(
        "Which employer from year 2000 are you claiming pension tax relief for?",
        "Acme",
        "Smith Holdings",
        "Foldright"
      ),
      "choice-dynamic-data-retrieve-based.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"          -> Many(List("0")),
        "1_director"            -> One("John"),
        "1_employerName"        -> One("Acme"),
        "2_addAnother"          -> Many(List("0")),
        "2_director"            -> One("Jack"),
        "2_employerName"        -> One("Smith Holdings"),
        "3_director"            -> One("Isabella"),
        "3_addAnother"          -> Many(List("1")),
        "3_employerName"        -> One("Foldright"),
        "employerChoiceFromAtl" -> Many(List("employerName_3"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_director"            -> StringResult("John"),
        "1_employerName"        -> StringResult("Acme"),
        "2_director"            -> StringResult("Jack"),
        "2_employerName"        -> StringResult("Smith Holdings"),
        "3_director"            -> StringResult("Isabella"),
        "3_employerName"        -> StringResult("Foldright"),
        "employerChoiceFromAtl" -> OptionResult(List("employerName_3"))
      ),
      List(
        "1. Acme - John",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Acme - John</li>
           |</ol>""".stripMargin,
        "2. Smith Holdings - Jack",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>Smith Holdings - Jack</li>
           |</ol>""".stripMargin,
        "3. Foldright - Isabella",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>Foldright - Isabella</li>
           |</ol>""".stripMargin,
        "Acme is John",
        "Smith Holdings is Jack",
        "Foldright is Isabella",
        "John of Acme",
        "Jack of Smith Holdings",
        "Isabella of Foldright"
      ),
      "choice-dynamic-atl-based.json Generated"
    ),
    (
      MongoUserData(
        "controlAddToList"      -> Many(List("skip")),
        "employerChoiceFromAtl" -> Many(List("NONE"))
      ),
      List(
        "n0",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnother"          -> Hidden,
        "1_director"            -> Hidden,
        "1_employerName"        -> Hidden,
        "controlAddToList"      -> OptionResult(List("skip")),
        "employerChoiceFromAtl" -> OptionResult(List("NONE"))
      ),
      List(
        "Different employer (Add to list has been skipped)"
      ),
      "choice-dynamic-atl-based-conditional.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"   -> Many(List("0")),
        "1_employerName" -> One("AAA"),
        "1_tax"          -> One("100"),
        "2_addAnother"   -> Many(List("0")),
        "2_employerName" -> One("BBB"),
        "2_tax"          -> One("200"),
        "3_addAnother"   -> Many(List("1")),
        "3_employerName" -> One("CCC"),
        "3_tax"          -> One("300")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1. AAA - £100.00",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA - £100.00</li>
           |</ol>""".stripMargin,
        "2. BBB - £200.00",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB - £200.00</li>
           |</ol>""".stripMargin,
        "3. CCC - £300.00",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>CCC - £300.00</li>
           |</ol>""".stripMargin,
        "AAA",
        "£100.00",
        "BBB",
        "£200.00",
        "CCC",
        "£300.00",
        "£600.00"
      ),
      "table-dynamic-atl-based.json Generated"
    ),
    (
      MongoUserData(
        "activeDays" -> One("5"),
        "dummy"      -> One("dummy")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "Expenditure total: £25.00",
        "<p>Expenditure total: £25.00</p>"
      ),
      "typed-expr.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"                 -> Many(List("0")),
        "1_lossSurrenderedWholePeriod" -> One("100"),
        "1_productionName"             -> One("AAA"),
        "2_addAnother"                 -> Many(List("1")),
        "2_lossSurrenderedWholePeriod" -> One("200"),
        "2_productionName"             -> One("BBB"),
        "accountingEnd-day"            -> One("1"),
        "accountingEnd-month"          -> One("1"),
        "accountingEnd-year"           -> One("2001")
      ),
      List(
        "n0",
        "ap1.1.0",
        "ap1.1.1",
        "ar1.1",
        "ap1.2.0",
        "ap1.2.1",
        "ar1.2",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_lossSurrenderedSecondSplitPeriod" -> Hidden,
        "1_lossSurrenderedWholePeriod"       -> NumberResult(100),
        "2_lossSurrenderedSecondSplitPeriod" -> Hidden,
        "2_lossSurrenderedWholePeriod"       -> NumberResult(200),
        "accountingEnd-day"                  -> NumberResult(1),
        "accountingEnd-month"                -> NumberResult(1),
        "accountingEnd-year"                 -> NumberResult(2001),
        "secret"                             -> Empty
      ),
      List(
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin,
        "Tax Relief production totals for the accounting period ending 1 January 2001",
        "AAA",
        "£100.00",
        "£20.00",
        "BBB",
        "£200.00",
        "£40.00",
        "£300.00",
        "£60.00"
      ),
      "table-dynamic-atl-based-typed-exprs.json Generated"
    ),
    (
      MongoUserData(
        "dummy"    -> One("dummy"),
        "employer" -> Many(List("EMP_1")),
        "info"     -> One(""),
        "nino"     -> One("AA333333A"),
        "taxYear"  -> One("2000")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employments" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName")      -> "Acme",
                  DataRetrieve.Attribute("worksNumber")       -> "ACME01",
                  DataRetrieve.Attribute("taxDistrictNumber") -> "123"
                ),
                Map(
                  DataRetrieve.Attribute("employerName")      -> "Smith Holdings",
                  DataRetrieve.Attribute("worksNumber")       -> "SMITH01",
                  DataRetrieve.Attribute("taxDistrictNumber") -> "456"
                ),
                Map(
                  DataRetrieve.Attribute("employerName")      -> "Foldright",
                  DataRetrieve.Attribute("worksNumber")       -> "FOLD01",
                  DataRetrieve.Attribute("taxDistrictNumber") -> "789"
                )
              )
          )
        )
      ),
      AnswerMap(
        "employer" -> OptionResult(List("EMP_1")),
        "secret"   -> Empty
      ),
      List(
        "Acme",
        "Smith Holdings",
        "Foldright",
        """|dataRetrieve.employments.worksNumber: ACME01, SMITH01, FOLD01
           |
           |employer: Smith Holdings
           |
           |indexOf(EMP in employer): 1
           |
           |employerName / taxDistrictNumber / worksNumber: Smith Holdings / 456 / SMITH01""".stripMargin,
        """<p>dataRetrieve.employments.worksNumber: ACME01, SMITH01, FOLD01</p>
          |<p>employer: Smith Holdings</p>
          |<p>indexOf(EMP in employer): 1</p>
          |<p>employerName / taxDistrictNumber / worksNumber: Smith Holdings / 456 / SMITH01</p>""".stripMargin
      ),
      "index-of-and-employments.json show secret page"
    ),
    (
      MongoUserData(
        "dummy"    -> One("dummy"),
        "employer" -> Many(List("EMP_0")),
        "info"     -> One(""),
        "nino"     -> One("AA333333A"),
        "taxYear"  -> One("2000")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employments" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName")      -> "Acme",
                  DataRetrieve.Attribute("worksNumber")       -> "ACME01",
                  DataRetrieve.Attribute("taxDistrictNumber") -> "123"
                ),
                Map(
                  DataRetrieve.Attribute("employerName")      -> "Smith Holdings",
                  DataRetrieve.Attribute("worksNumber")       -> "SMITH01",
                  DataRetrieve.Attribute("taxDistrictNumber") -> "456"
                ),
                Map(
                  DataRetrieve.Attribute("employerName")      -> "Foldright",
                  DataRetrieve.Attribute("worksNumber")       -> "FOLD01",
                  DataRetrieve.Attribute("taxDistrictNumber") -> "789"
                )
              )
          )
        )
      ),
      AnswerMap(
        "employer" -> OptionResult(List("EMP_0")),
        "secret"   -> Hidden
      ),
      List(
        "Acme",
        "Smith Holdings",
        "Foldright",
        """|dataRetrieve.employments.worksNumber: ACME01, SMITH01, FOLD01
           |
           |employer: Acme
           |
           |indexOf(EMP in employer): 0
           |
           |employerName / taxDistrictNumber / worksNumber: Acme / 123 / ACME01""".stripMargin,
        """<p>dataRetrieve.employments.worksNumber: ACME01, SMITH01, FOLD01</p>
          |<p>employer: Acme</p>
          |<p>indexOf(EMP in employer): 0</p>
          |<p>employerName / taxDistrictNumber / worksNumber: Acme / 123 / ACME01</p>""".stripMargin
      ),
      "index-of-and-employments.json hide secret page"
    ),
    (
      MongoUserData(
        "dummy"           -> One("dummy"),
        "endDate-day"     -> One("2"),
        "endDate-month"   -> One("2"),
        "endDate-year"    -> One("2002"),
        "startDate-day"   -> One("1"),
        "startDate-month" -> One("1"),
        "startDate-year"  -> One("2001")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-day"   -> NumberResult(1),
        "startDate-month" -> NumberResult(1),
        "startDate-year"  -> NumberResult(2001),
        "endDate-day"     -> NumberResult(2),
        "endDate-month"   -> NumberResult(2),
        "endDate-year"    -> NumberResult(2002),
        "secretDays"      -> Empty,
        "secretWeeks"     -> Hidden
      ),
      List(
        """|startDate: 1 January 2001
           |
           |endDate: 2 February 2002
           |
           |daysBetween: 398
           |
           |weeksBetween: 56""".stripMargin,
        """<p>startDate: 1 January 2001</p>
          |<p>endDate: 2 February 2002</p>
          |<p>daysBetween: 398</p>
          |<p>weeksBetween: 56</p>""".stripMargin
      ),
      "between-expr.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_dummy"           -> One("AAA"),
        "1_endDate-day"     -> One("4"),
        "1_endDate-month"   -> One("5"),
        "1_endDate-year"    -> One("2006"),
        "1_secretDays"      -> Many(List("reveal")),
        "1_secretWeeks"     -> Many(List("reveal")),
        "1_startDate-day"   -> One("1"),
        "1_startDate-month" -> One("2"),
        "1_startDate-year"  -> One("2003"),
        "2_addAnotherField" -> Many(List("0")),
        "2_dummy"           -> One("BBB"),
        "2_endDate-day"     -> One("10"),
        "2_endDate-month"   -> One("11"),
        "2_endDate-year"    -> One("2012"),
        "2_secretDays"      -> Many(List("reveal")),
        "2_startDate-day"   -> One("7"),
        "2_startDate-month" -> One("8"),
        "2_startDate-year"  -> One("2009"),
        "3_addAnotherField" -> Many(List("1")),
        "3_dummy"           -> One("CCC"),
        "3_endDate-day"     -> One("1"),
        "3_endDate-month"   -> One("1"),
        "3_endDate-year"    -> One("2000"),
        "3_startDate-day"   -> One("1"),
        "3_startDate-month" -> One("2"),
        "3_startDate-year"  -> One("2000")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ap0.1.2",
        "ap0.1.3",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ap0.2.2",
        "ap0.2.3",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1",
        "ar0.3",
        "n1",
        "n2",
        "n3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_endDate-day"     -> NumberResult(4),
        "1_endDate-month"   -> NumberResult(5),
        "1_endDate-year"    -> NumberResult(2006),
        "1_secretDays"      -> OptionResult(List("reveal")),
        "1_secretWeeks"     -> OptionResult(List("reveal")),
        "1_startDate-day"   -> NumberResult(1),
        "1_startDate-month" -> NumberResult(2),
        "1_startDate-year"  -> NumberResult(2003),
        "2_endDate-day"     -> NumberResult(10),
        "2_endDate-month"   -> NumberResult(11),
        "2_endDate-year"    -> NumberResult(2012),
        "2_secretDays"      -> OptionResult(List("reveal")),
        "2_secretWeeks"     -> Empty,
        "2_startDate-day"   -> NumberResult(7),
        "2_startDate-month" -> NumberResult(8),
        "2_startDate-year"  -> NumberResult(2009),
        "3_endDate-day"     -> NumberResult(1),
        "3_endDate-month"   -> NumberResult(1),
        "3_endDate-year"    -> NumberResult(2000),
        "3_secretDays"      -> Hidden,
        "3_secretWeeks"     -> Hidden,
        "3_startDate-day"   -> NumberResult(1),
        "3_startDate-month" -> NumberResult(2),
        "3_startDate-year"  -> NumberResult(2000),
        "secretDaysSum"     -> Empty,
        "secretWeeksSum"    -> Empty
      ),
      List(
        """|startDate: 1 February 2003
           |
           |endDate: 4 May 2006
           |
           |daysBetween: 1189
           |
           |weeksBetween: 169""".stripMargin,
        """|<p>startDate: 1 February 2003</p>
           |<p>endDate: 4 May 2006</p>
           |<p>daysBetween: 1189</p>
           |<p>weeksBetween: 169</p>""".stripMargin,
        "1. 1 February 2003 - 4 May 2006",
        """|<ol class="govuk-list govuk-list--number">
           | <li>1 February 2003 - 4 May 2006</li>
           |</ol>""".stripMargin,
        """|startDate: 7 August 2009
           |
           |endDate: 10 November 2012
           |
           |daysBetween: 1192
           |
           |weeksBetween: 170""".stripMargin,
        """|<p>startDate: 7 August 2009</p>
           |<p>endDate: 10 November 2012</p>
           |<p>daysBetween: 1192</p>
           |<p>weeksBetween: 170</p>""".stripMargin,
        "2. 7 August 2009 - 10 November 2012",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>7 August 2009 - 10 November 2012</li>
           |</ol>""".stripMargin,
        """|startDate: 1 February 2000
           |
           |endDate: 1 January 2000
           |
           |daysBetween: -30
           |
           |weeksBetween: -4""".stripMargin,
        """|<p>startDate: 1 February 2000</p>
           |<p>endDate: 1 January 2000</p>
           |<p>daysBetween: -30</p>
           |<p>weeksBetween: -4</p>""".stripMargin,
        "3. 1 February 2000 - 1 January 2000",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>1 February 2000 - 1 January 2000</li>
           |</ol>""".stripMargin,
        """|startDate: 1 February 2003, 7 August 2009, 1 February 2000
           |
           |endDate: 4 May 2006, 10 November 2012, 1 January 2000
           |
           |daysBetween: 1189, 1192, -30
           |
           |daysBetween.sum: 2351
           |
           |weeksBetween: 169, 170, -4
           |
           |weeksBetween.sum: 335""".stripMargin,
        """|<p>startDate: 1 February 2003, 7 August 2009, 1 February 2000</p>
           |<p>endDate: 4 May 2006, 10 November 2012, 1 January 2000</p>
           |<p>daysBetween: 1189, 1192, -30</p>
           |<p>daysBetween.sum: 2351</p>
           |<p>weeksBetween: 169, 170, -4</p>
           |<p>weeksBetween.sum: 335</p>""".stripMargin
      ),
      "between-expr-atl.json Generated"
    ),
    (
      MongoUserData(
        "address-country"               -> One(""),
        "address-postcode"              -> One("AA11 1AA"),
        "address-street1"               -> One("Line 1"),
        "address-street2"               -> One("Line 2"),
        "address-street3"               -> One("Town"),
        "address-street4"               -> One(""),
        "address-uk"                    -> One("true"),
        "addressCounty-country"         -> One(""),
        "addressCounty-postcode"        -> One("AA11 1AA"),
        "addressCounty-street1"         -> One("Line 1"),
        "addressCounty-street2"         -> One("Line 2"),
        "addressCounty-street3"         -> One("Town"),
        "addressCounty-street4"         -> One("County"),
        "addressCounty-uk"              -> One("true"),
        "dummy"                         -> One("dummy"),
        "internationalAddress-country"  -> One("France"),
        "internationalAddress-postcode" -> One(""),
        "internationalAddress-street1"  -> One("Street 1"),
        "internationalAddress-street2"  -> One("Street 2"),
        "internationalAddress-street3"  -> One("Street 3"),
        "internationalAddress-street4"  -> One(""),
        "internationalAddress-uk"       -> One("false")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "Test Line 1, Line 2, Town, AA11 1AA - Line 1, Line 2, Town, County, AA11 1AA - Street 1, Street 2, Street 3, France",
        """|address: Line 1, Line 2, Town, AA11 1AA
           |
           |address with county: Line 1, Line 2, Town, County, AA11 1AA
           |
           |international address: Street 1, Street 2, Street 3, France""".stripMargin,
        """|<p>address: Line 1<br>
           | Line 2<br>
           | Town<br>
           | AA11 1AA</p>
           |<p>address with county: Line 1<br>
           | Line 2<br>
           | Town<br>
           | County<br>
           | AA11 1AA</p>
           |<p>international address: Street 1<br>
           | Street 2<br>
           | Street 3<br>
           | France</p>""".stripMargin
      ),
      "address.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnotherProcessingAddress" -> Many(List("0")),
        "1_addressControl"              -> Many(List("showAddress")),
        "1_processingAddress-country"   -> One(""),
        "1_processingAddress-postcode"  -> One("DD11 1DD"),
        "1_processingAddress-street1"   -> One("AAA"),
        "1_processingAddress-street2"   -> One("BBB"),
        "1_processingAddress-street3"   -> One("CCC"),
        "1_processingAddress-street4"   -> One(""),
        "1_processingAddress-uk"        -> One("true"),
        "2_addAnotherProcessingAddress" -> Many(List("0")),
        "2_addressControl"              -> Many(List("hideAddress")),
        "3_addAnotherProcessingAddress" -> Many(List("1")),
        "3_addressControl"              -> Many(List("showAddress")),
        "3_processingAddress-country"   -> One(""),
        "3_processingAddress-postcode"  -> One("HH11 1HH"),
        "3_processingAddress-street1"   -> One("EEE"),
        "3_processingAddress-street2"   -> One("FFF"),
        "3_processingAddress-street3"   -> One("GGG"),
        "3_processingAddress-street4"   -> One(""),
        "3_processingAddress-uk"        -> One("true"),
        "details"                       -> One("Alternative details"),
        "dummy"                         -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1",
        "ar0.3",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addressControl"             -> OptionResult(List("showAddress")),
        "1_processingAddress-country"  -> StringResult(""),
        "1_processingAddress-postcode" -> StringResult("DD11 1DD"),
        "1_processingAddress-street1"  -> StringResult("AAA"),
        "1_processingAddress-street2"  -> StringResult("BBB"),
        "1_processingAddress-street3"  -> StringResult("CCC"),
        "1_processingAddress-street4"  -> StringResult(""),
        "1_processingAddress-uk"       -> StringResult("true"),
        "2_addressControl"             -> OptionResult(List("hideAddress")),
        "2_processingAddress-country"  -> Hidden,
        "2_processingAddress-postcode" -> Hidden,
        "2_processingAddress-street1"  -> Hidden,
        "2_processingAddress-street2"  -> Hidden,
        "2_processingAddress-street3"  -> Hidden,
        "2_processingAddress-street4"  -> Hidden,
        "2_processingAddress-uk"       -> Hidden,
        "3_addressControl"             -> OptionResult(List("showAddress")),
        "3_processingAddress-country"  -> StringResult(""),
        "3_processingAddress-postcode" -> StringResult("HH11 1HH"),
        "3_processingAddress-street1"  -> StringResult("EEE"),
        "3_processingAddress-street2"  -> StringResult("FFF"),
        "3_processingAddress-street3"  -> StringResult("GGG"),
        "3_processingAddress-street4"  -> StringResult(""),
        "3_processingAddress-uk"       -> StringResult("true"),
        "details"                      -> Hidden
      ),
      List(
        "AAA, BBB, CCC, DD11 1DD",
        "Address 1: AAA, BBB, CCC, DD11 1DD",
        """|<p>Address 1: AAA<br>
           | BBB<br>
           | CCC<br>
           | DD11 1DD</p>""".stripMargin,
        "",
        "Address 2: ",
        "<p>Address 2:</p>",
        "EEE, FFF, GGG, HH11 1HH",
        "Address 3: EEE, FFF, GGG, HH11 1HH",
        """|<p>Address 3: EEE<br>
           | FFF<br>
           | GGG<br>
           | HH11 1HH</p>""".stripMargin,
        "all addresses: AAA, BBB, CCC, DD11 1DD, EEE, FFF, GGG, HH11 1HH",
        """|<p>all addresses: AAA<br>
           | BBB<br>
           | CCC<br>
           | DD11 1DD<br>
           | EEE<br>
           | FFF<br>
           | GGG<br>
           | HH11 1HH</p>""".stripMargin
      ),
      "address-atl.json one address in atl is visible"
    ),
    (
      MongoUserData(
        "1_addAnotherProcessingAddress" -> Many(List("0")),
        "1_addressControl"              -> Many(List("hideAddress")),
        "1_processingAddress-country"   -> One(""),
        "1_processingAddress-postcode"  -> One("AA11 1AA"),
        "1_processingAddress-street1"   -> One("AAA"),
        "1_processingAddress-street2"   -> One(""),
        "1_processingAddress-street3"   -> One(""),
        "1_processingAddress-street4"   -> One(""),
        "1_processingAddress-uk"        -> One("true"),
        "2_addAnotherProcessingAddress" -> Many(List("1")),
        "2_addressControl"              -> Many(List("hideAddress")),
        "details"                       -> One("Alternative details"),
        "dummy"                         -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addressControl"             -> OptionResult(List("hideAddress")),
        "1_processingAddress-country"  -> Hidden,
        "1_processingAddress-postcode" -> Hidden,
        "1_processingAddress-street1"  -> Hidden,
        "1_processingAddress-street2"  -> Hidden,
        "1_processingAddress-street3"  -> Hidden,
        "1_processingAddress-street4"  -> Hidden,
        "1_processingAddress-uk"       -> Hidden,
        "2_addressControl"             -> OptionResult(List("hideAddress")),
        "2_processingAddress-country"  -> Hidden,
        "2_processingAddress-postcode" -> Hidden,
        "2_processingAddress-street1"  -> Hidden,
        "2_processingAddress-street2"  -> Hidden,
        "2_processingAddress-street3"  -> Hidden,
        "2_processingAddress-street4"  -> Hidden,
        "2_processingAddress-uk"       -> Hidden,
        "details"                      -> StringResult("Alternative details")
      ),
      List(
        "",
        "Address 1: ",
        "<p>Address 1:</p>",
        "",
        "Address 2: ",
        "<p>Address 2:</p>",
        "all addresses: ",
        "<p>all addresses:</p>"
      ),
      "address-atl.json no addresses in atl is visible"
    ),
    (
      MongoUserData(
        "address"          -> One(""),
        "address-country"  -> One(""),
        "address-postcode" -> One("DD11 1DD"),
        "address-street1"  -> One("AAA"),
        "address-street2"  -> One("BBB"),
        "address-street3"  -> One("CCC"),
        "address-street4"  -> One(""),
        "address-uk"       -> One("true"),
        "addressControl"   -> Many(List("showAddress")),
        "dummy"            -> One("dummy")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "address-country"  -> StringResult(""),
        "address-postcode" -> StringResult("DD11 1DD"),
        "address-street1"  -> StringResult("AAA"),
        "address-street2"  -> StringResult("BBB"),
        "address-street3"  -> StringResult("CCC"),
        "address-street4"  -> StringResult(""),
        "address-uk"       -> StringResult("true"),
        "addressControl"   -> OptionResult(List("showAddress")),
        "details"          -> Empty
      ),
      List(
        """|Address: 'AAA, BBB, CCC, DD11 1DD'
           |
           |Address is (=) visible
           |
           |Address is (!=) visible""".stripMargin,
        """|<p>Address: 'AAA<br>
           | BBB<br>
           | CCC<br>
           | DD11 1DD'</p>
           |<p>Address is (=) visible</p>
           |<p>Address is (!=) visible</p>""".stripMargin
      ),
      "address-visibility.json show address"
    ),
    (
      MongoUserData(
        "address"          -> One(""),
        "address-country"  -> One(""),
        "address-postcode" -> One("DD11 1DD"),
        "address-street1"  -> One("AAA"),
        "address-street2"  -> One("BBB"),
        "address-street3"  -> One("CCC"),
        "address-street4"  -> One(""),
        "address-uk"       -> One("true"),
        "addressControl"   -> Many(List("hideAddress")),
        "dummy"            -> One("dummy")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "address-country"  -> Hidden,
        "address-postcode" -> Hidden,
        "address-street1"  -> Hidden,
        "address-street2"  -> Hidden,
        "address-street3"  -> Hidden,
        "address-street4"  -> Hidden,
        "address-uk"       -> Hidden,
        "addressControl"   -> OptionResult(List("hideAddress")),
        "details"          -> Hidden
      ),
      List(
        """Address: ''
          |
          |Address is (=) hidden
          |
          |Address is (!=) hidden""".stripMargin,
        """|<p>Address: ''</p>
           |<p>Address is (=) hidden</p>
           |<p>Address is (!=) hidden</p>""".stripMargin
      ),
      "address-visibility.json hide address"
    ),
    (
      MongoUserData(
        "dummy"                    -> One("dummy"),
        "overseasAddress-city"     -> One("DDD"),
        "overseasAddress-country"  -> One("Czech Republic"),
        "overseasAddress-line1"    -> One("AAA"),
        "overseasAddress-line2"    -> One("BBB"),
        "overseasAddress-line3"    -> One("CCC"),
        "overseasAddress-postcode" -> One("EEE")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "Test AAA, BBB, CCC, DDD, EEE, Czech Republic",
        "overseas address: AAA, BBB, CCC, DDD, EEE, Czech Republic",
        """|<p>overseas address: AAA<br>
           | BBB<br>
           | CCC<br>
           | DDD<br>
           | EEE<br>
           | Czech Republic</p>""".stripMargin
      ),
      "overseas-address.json Generated"
    ),
    (
      MongoUserData(
        "addressControl"           -> Many(List("showAddress")),
        "details"                  -> One("More details"),
        "dummy"                    -> One("dummy"),
        "overseasAddress-city"     -> One("DDD"),
        "overseasAddress-country"  -> One("France"),
        "overseasAddress-line1"    -> One("AAA"),
        "overseasAddress-line2"    -> One("BBB"),
        "overseasAddress-line3"    -> One("CCC"),
        "overseasAddress-postcode" -> One("EEE")
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "addressControl"           -> OptionResult(List("showAddress")),
        "details"                  -> StringResult("More details"),
        "overseasAddress-city"     -> StringResult("DDD"),
        "overseasAddress-country"  -> StringResult("France"),
        "overseasAddress-line1"    -> StringResult("AAA"),
        "overseasAddress-line2"    -> StringResult("BBB"),
        "overseasAddress-line3"    -> StringResult("CCC"),
        "overseasAddress-postcode" -> StringResult("EEE")
      ),
      List(
        """|Overseas address: 'AAA, BBB, CCC, DDD, EEE, France'
           |
           |Overseas address is (=) visible
           |
           |Overseas address is (!=) visible""".stripMargin,
        """|<p>Overseas address: 'AAA<br>
           | BBB<br>
           | CCC<br>
           | DDD<br>
           | EEE<br>
           | France'</p>
           |<p>Overseas address is (=) visible</p>
           |<p>Overseas address is (!=) visible</p>""".stripMargin
      ),
      "overseas-address-visibility.json show address"
    ),
    (
      MongoUserData(
        "addressControl"           -> Many(List("hideAddress")),
        "details"                  -> One("More details"),
        "dummy"                    -> One("dummy"),
        "overseasAddress-city"     -> One("DDD"),
        "overseasAddress-country"  -> One("France"),
        "overseasAddress-line1"    -> One("AAA"),
        "overseasAddress-line2"    -> One("BBB"),
        "overseasAddress-line3"    -> One("CCC"),
        "overseasAddress-postcode" -> One("EEE")
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "addressControl"           -> OptionResult(List("hideAddress")),
        "details"                  -> Hidden,
        "overseasAddress-city"     -> Hidden,
        "overseasAddress-country"  -> Hidden,
        "overseasAddress-line1"    -> Hidden,
        "overseasAddress-line2"    -> Hidden,
        "overseasAddress-line3"    -> Hidden,
        "overseasAddress-postcode" -> Hidden
      ),
      List(
        """|Overseas address: ''
           |
           |Overseas address is (=) hidden
           |
           |Overseas address is (!=) hidden""".stripMargin,
        """|<p>Overseas address: ''</p>
           |<p>Overseas address is (=) hidden</p>
           |<p>Overseas address is (!=) hidden</p>""".stripMargin
      ),
      "overseas-address-visibility.json hide address"
    ),
    (
      MongoUserData(
        "1_addAnotherOverseasAddress"          -> Many(List("0")),
        "1_addressControl"                     -> Many(List("showAddress")),
        "1_processingOverseasAddress-city"     -> One("DDD"),
        "1_processingOverseasAddress-country"  -> One("France"),
        "1_processingOverseasAddress-line1"    -> One("AAA"),
        "1_processingOverseasAddress-line2"    -> One("BBB"),
        "1_processingOverseasAddress-line3"    -> One("CCC"),
        "1_processingOverseasAddress-postcode" -> One("EEE"),
        "2_addAnotherOverseasAddress"          -> Many(List("0")),
        "2_addressControl"                     -> Many(List("showAddress")),
        "2_processingOverseasAddress-city"     -> One("JJJ"),
        "2_processingOverseasAddress-country"  -> One("Luxembourg"),
        "2_processingOverseasAddress-line1"    -> One("GGG"),
        "2_processingOverseasAddress-line2"    -> One("HHH"),
        "2_processingOverseasAddress-line3"    -> One("III"),
        "2_processingOverseasAddress-postcode" -> One("KKK"),
        "3_addAnotherOverseasAddress"          -> Many(List("1")),
        "3_addressControl"                     -> Many(List("hideAddress")),
        "dummy"                                -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addressControl"                     -> OptionResult(List("showAddress")),
        "1_processingOverseasAddress-city"     -> StringResult("DDD"),
        "1_processingOverseasAddress-country"  -> StringResult("France"),
        "1_processingOverseasAddress-line1"    -> StringResult("AAA"),
        "1_processingOverseasAddress-line2"    -> StringResult("BBB"),
        "1_processingOverseasAddress-line3"    -> StringResult("CCC"),
        "1_processingOverseasAddress-postcode" -> StringResult("EEE"),
        "2_addressControl"                     -> OptionResult(List("showAddress")),
        "2_processingOverseasAddress-city"     -> StringResult("JJJ"),
        "2_processingOverseasAddress-country"  -> StringResult("Luxembourg"),
        "2_processingOverseasAddress-line1"    -> StringResult("GGG"),
        "2_processingOverseasAddress-line2"    -> StringResult("HHH"),
        "2_processingOverseasAddress-line3"    -> StringResult("III"),
        "2_processingOverseasAddress-postcode" -> StringResult("KKK"),
        "3_addressControl"                     -> OptionResult(List("hideAddress")),
        "3_processingOverseasAddress-city"     -> Hidden,
        "3_processingOverseasAddress-country"  -> Hidden,
        "3_processingOverseasAddress-line1"    -> Hidden,
        "3_processingOverseasAddress-line2"    -> Hidden,
        "3_processingOverseasAddress-line3"    -> Hidden,
        "3_processingOverseasAddress-postcode" -> Hidden,
        "details"                              -> Hidden
      ),
      List(
        "AAA, BBB, CCC, DDD, EEE, France",
        "Address 1: AAA, BBB, CCC, DDD, EEE, France",
        """|<p>Address 1: AAA<br>
           | BBB<br>
           | CCC<br>
           | DDD<br>
           | EEE<br>
           | France</p>""".stripMargin,
        "GGG, HHH, III, JJJ, KKK, Luxembourg",
        "Address 2: GGG, HHH, III, JJJ, KKK, Luxembourg",
        """|<p>Address 2: GGG<br>
           | HHH<br>
           | III<br>
           | JJJ<br>
           | KKK<br>
           | Luxembourg</p>""".stripMargin,
        "",
        "Address 3: ",
        "<p>Address 3:</p>",
        "all addresses: AAA, BBB, CCC, DDD, EEE, France, GGG, HHH, III, JJJ, KKK, Luxembourg",
        """|<p>all addresses: AAA<br>
           | BBB<br>
           | CCC<br>
           | DDD<br>
           | EEE<br>
           | France<br>
           | GGG<br>
           | HHH<br>
           | III<br>
           | JJJ<br>
           | KKK<br>
           | Luxembourg</p>""".stripMargin
      ),
      "overseas-address-atl.json two addresses in atl are visible"
    ),
    (
      MongoUserData(
        "1_addAnotherOverseasAddress"          -> Many(List("0")),
        "1_addressControl"                     -> Many(List("hideAddress")),
        "1_processingOverseasAddress-city"     -> One("DDD"),
        "1_processingOverseasAddress-country"  -> One("France"),
        "1_processingOverseasAddress-line1"    -> One("AAA"),
        "1_processingOverseasAddress-line2"    -> One("BBB"),
        "1_processingOverseasAddress-line3"    -> One("CCC"),
        "1_processingOverseasAddress-postcode" -> One("EEE"),
        "2_addAnotherOverseasAddress"          -> Many(List("0")),
        "2_addressControl"                     -> Many(List("hideAddress")),
        "3_addAnotherOverseasAddress"          -> Many(List("1")),
        "3_addressControl"                     -> Many(List("hideAddress")),
        "details"                              -> One("Alternative details"),
        "dummy"                                -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addressControl"                     -> OptionResult(List("hideAddress")),
        "1_processingOverseasAddress-city"     -> Hidden,
        "1_processingOverseasAddress-country"  -> Hidden,
        "1_processingOverseasAddress-line1"    -> Hidden,
        "1_processingOverseasAddress-line2"    -> Hidden,
        "1_processingOverseasAddress-line3"    -> Hidden,
        "1_processingOverseasAddress-postcode" -> Hidden,
        "2_addressControl"                     -> OptionResult(List("hideAddress")),
        "2_processingOverseasAddress-city"     -> Hidden,
        "2_processingOverseasAddress-country"  -> Hidden,
        "2_processingOverseasAddress-line1"    -> Hidden,
        "2_processingOverseasAddress-line2"    -> Hidden,
        "2_processingOverseasAddress-line3"    -> Hidden,
        "2_processingOverseasAddress-postcode" -> Hidden,
        "3_addressControl"                     -> OptionResult(List("hideAddress")),
        "3_processingOverseasAddress-city"     -> Hidden,
        "3_processingOverseasAddress-country"  -> Hidden,
        "3_processingOverseasAddress-line1"    -> Hidden,
        "3_processingOverseasAddress-line2"    -> Hidden,
        "3_processingOverseasAddress-line3"    -> Hidden,
        "3_processingOverseasAddress-postcode" -> Hidden,
        "details"                              -> StringResult("Alternative details")
      ),
      List(
        "",
        "Address 1: ",
        "<p>Address 1:</p>",
        "",
        "Address 2: ",
        "<p>Address 2:</p>",
        "",
        "Address 3: ",
        "<p>Address 3:</p>",
        "all addresses: ",
        "<p>all addresses:</p>"
      ),
      "overseas-address-atl.json no addresses in atl is visible"
    ),
    (
      MongoUserData(
        "dummy"                   -> One("dummy"),
        "postcodeLookup-filter"   -> One(""),
        "postcodeLookup-postcode" -> One("ZZ9Z 9TT")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          selectedAddresses = Some(
            Map(
              FormComponentId("postcodeLookup") -> "GB990091234559"
            )
          ),
          postcodeLookup = Some(
            Map(
              FormComponentId("postcodeLookup") -> mkAddressLookupResult(
                id = "GB990091234559",
                lines = List("2 Testing Lane"),
                town = "Royal Madeuptown",
                postcode = "ZZ9Z 9TT"
              )
            )
          )
        )
      ),
      AnswerMap(),
      List(
        "Test 2 Testing Lane, Royal Madeuptown, ZZ9Z 9TT",
        "postcodeLookup: 2 Testing Lane, Royal Madeuptown, ZZ9Z 9TT",
        """|<p>postcodeLookup: 2 Testing Lane<br>
           | Royal Madeuptown<br>
           | ZZ9Z 9TT</p>""".stripMargin
      ),
      "postcode-lookup.json Generated"
    ),
    (
      MongoUserData(
        "details"                 -> One("Details"),
        "dummy"                   -> One("dummy"),
        "postcodeLookup-filter"   -> One(""),
        "postcodeLookup-postcode" -> One("ZZ9Z 9TT"),
        "postcodeLookupControl"   -> Many(List("showPostcodeLookup"))
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          selectedAddresses = Some(
            Map(
              FormComponentId("postcodeLookup") -> "GB990091234559"
            )
          ),
          postcodeLookup = Some(
            Map(
              FormComponentId("postcodeLookup") -> mkAddressLookupResult(
                id = "GB990091234559",
                lines = List("2 Testing Lane"),
                town = "Royal Madeuptown",
                postcode = "ZZ9Z 9TT"
              )
            )
          )
        )
      ),
      AnswerMap(
        "details"                 -> StringResult("Details"),
        "postcodeLookup-filter"   -> StringResult(""),
        "postcodeLookup-postcode" -> StringResult("ZZ9Z 9TT"),
        "postcodeLookupControl"   -> OptionResult(List("showPostcodeLookup"))
      ),
      List(
        """|Postcode lookup: '2 Testing Lane, Royal Madeuptown, ZZ9Z 9TT'
           |
           |Postcode lookup is (=) visible
           |
           |Postcode lookup is (!=) visible""".stripMargin,
        """|<p>Postcode lookup: '2 Testing Lane<br>
           | Royal Madeuptown<br>
           | ZZ9Z 9TT'</p>
           |<p>Postcode lookup is (=) visible</p>
           |<p>Postcode lookup is (!=) visible</p>""".stripMargin
      ),
      "postcode-lookup-visibility.json show postcodeLookup"
    ),
    (
      MongoUserData(
        "details"                 -> One("Details"),
        "dummy"                   -> One("dummy"),
        "postcodeLookup-filter"   -> One(""),
        "postcodeLookup-postcode" -> One("ZZ9Z 9TT"),
        "postcodeLookupControl"   -> Many(List("hidePostcodeLookup"))
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          selectedAddresses = Some(
            Map(
              FormComponentId("postcodeLookup") -> "GB990091234559"
            )
          ),
          postcodeLookup = Some(
            Map(
              FormComponentId("postcodeLookup") -> mkAddressLookupResult(
                id = "GB990091234559",
                lines = List("2 Testing Lane"),
                town = "Royal Madeuptown",
                postcode = "ZZ9Z 9TT"
              )
            )
          )
        )
      ),
      AnswerMap(
        "details"                 -> Hidden,
        "postcodeLookup-filter"   -> Hidden,
        "postcodeLookup-postcode" -> Hidden,
        "postcodeLookupControl"   -> OptionResult(List("hidePostcodeLookup"))
      ),
      List(
        """|Postcode lookup: ''
           |
           |Postcode lookup is (=) hidden
           |
           |Postcode lookup is (!=) hidden""".stripMargin,
        """|<p>Postcode lookup: ''</p>
           |<p>Postcode lookup is (=) hidden</p>
           |<p>Postcode lookup is (!=) hidden</p>""".stripMargin
      ),
      "postcode-lookup-visibility.json hide postcodeLookup"
    ),
    (
      MongoUserData(
        "1_addAnotherPostcodeLookup" -> Many(List("0")),
        "1_postcodeLookup-filter"    -> One(""),
        "1_postcodeLookup-postcode"  -> One("ZZ9Z 9TT"),
        "1_postcodeLookupControl"    -> Many(List("showPostcodeLookup")),
        "2_addAnotherPostcodeLookup" -> Many(List("0")),
        "2_postcodeLookupControl"    -> Many(List("hidePostcodeLookup")),
        "3_addAnotherPostcodeLookup" -> Many(List("1")),
        "3_postcodeLookup-filter"    -> One(""),
        "3_postcodeLookup-postcode"  -> One("FX1A 7GA"),
        "3_postcodeLookupControl"    -> Many(List("showPostcodeLookup")),
        "dummy"                      -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1",
        "ar0.3",
        "n1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          selectedAddresses = Some(
            Map(
              FormComponentId("1_postcodeLookup") -> "GB990091234559",
              FormComponentId("3_postcodeLookup") -> "GB990091234584"
            )
          ),
          postcodeLookup = Some(
            Map(
              FormComponentId("1_postcodeLookup") -> mkAddressLookupResult(
                id = "GB990091234559",
                lines = List("2 Testing Lane"),
                town = "Royal Madeuptown",
                postcode = "ZZ9Z 9TT"
              ),
              FormComponentId("3_postcodeLookup") -> mkAddressLookupResult(
                id = "GB990091234584",
                lines = List("3 Other Place", "Some District"),
                town = "Anytown",
                postcode = "FX1A 7GA"
              )
            )
          )
        )
      ),
      AnswerMap(
        "1_manualEntry"             -> Hidden,
        "1_postcodeLookup-filter"   -> StringResult(""),
        "1_postcodeLookup-postcode" -> StringResult("ZZ9Z 9TT"),
        "1_postcodeLookupControl"   -> OptionResult(List("showPostcodeLookup")),
        "2_manualEntry"             -> Empty,
        "2_postcodeLookup-filter"   -> Hidden,
        "2_postcodeLookup-postcode" -> Hidden,
        "2_postcodeLookupControl"   -> OptionResult(List("hidePostcodeLookup")),
        "3_manualEntry"             -> Hidden,
        "3_postcodeLookup-filter"   -> StringResult(""),
        "3_postcodeLookup-postcode" -> StringResult("FX1A 7GA"),
        "3_postcodeLookupControl"   -> OptionResult(List("showPostcodeLookup")),
        "details"                   -> Hidden
      ),
      List(
        "2 Testing Lane, Royal Madeuptown, ZZ9Z 9TT",
        "Postcode lookup 1: 2 Testing Lane, Royal Madeuptown, ZZ9Z 9TT",
        """|<p>Postcode lookup 1: 2 Testing Lane<br>
           | Royal Madeuptown<br>
           | ZZ9Z 9TT</p>""".stripMargin,
        "",
        "Postcode lookup 2: ",
        "<p>Postcode lookup 2:</p>",
        "3 Other Place, Some District, Anytown, FX1A 7GA",
        "Postcode lookup 3: 3 Other Place, Some District, Anytown, FX1A 7GA",
        """|<p>Postcode lookup 3: 3 Other Place<br>
           | Some District<br>
           | Anytown<br>
           | FX1A 7GA</p>""".stripMargin,
        "all addresses: 2 Testing Lane, Royal Madeuptown, ZZ9Z 9TT, 3 Other Place, Some District, Anytown, FX1A 7GA",
        """|<p>all addresses: 2 Testing Lane<br>
           | Royal Madeuptown<br>
           | ZZ9Z 9TT<br>
           | 3 Other Place<br>
           | Some District<br>
           | Anytown<br>
           | FX1A 7GA</p>""".stripMargin
      ),
      "postcode-lookup-atl.json two postcode lookups are visible"
    ),
    (
      MongoUserData(
        "1_manualEntry"              -> One("No lane 100"),
        "1_addAnotherPostcodeLookup" -> Many(List("0")),
        "1_postcodeLookup-filter"    -> One(""),
        "1_postcodeLookup-postcode"  -> One("ZZ9Z 9TT"),
        "1_postcodeLookupControl"    -> Many(List("hidePostcodeLookup")),
        "2_addAnotherPostcodeLookup" -> Many(List("0")),
        "2_postcodeLookupControl"    -> Many(List("hidePostcodeLookup")),
        "3_addAnotherPostcodeLookup" -> Many(List("1")),
        "3_postcodeLookup-filter"    -> One(""),
        "3_postcodeLookup-postcode"  -> One("FX1A 7GA"),
        "3_postcodeLookupControl"    -> Many(List("hidePostcodeLookup")),
        "dummy"                      -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1",
        "ar0.3",
        "n1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          selectedAddresses = Some(
            Map(
              FormComponentId("1_postcodeLookup") -> "GB990091234559",
              FormComponentId("3_postcodeLookup") -> "GB990091234584"
            )
          ),
          postcodeLookup = Some(
            Map(
              FormComponentId("1_postcodeLookup") -> mkAddressLookupResult(
                id = "GB990091234559",
                lines = List("2 Testing Lane"),
                town = "Royal Madeuptown",
                postcode = "ZZ9Z 9TT"
              ),
              FormComponentId("3_postcodeLookup") -> mkAddressLookupResult(
                id = "GB990091234584",
                lines = List("3 Other Place", "Some District"),
                town = "Anytown",
                postcode = "FX1A 7GA"
              )
            )
          )
        )
      ),
      AnswerMap(
        "1_manualEntry"             -> StringResult("No lane 100"),
        "1_postcodeLookup-filter"   -> Hidden,
        "1_postcodeLookup-postcode" -> Hidden,
        "1_postcodeLookupControl"   -> OptionResult(List("hidePostcodeLookup")),
        "2_manualEntry"             -> Empty,
        "2_postcodeLookup-filter"   -> Hidden,
        "2_postcodeLookup-postcode" -> Hidden,
        "2_postcodeLookupControl"   -> OptionResult(List("hidePostcodeLookup")),
        "3_manualEntry"             -> Empty,
        "3_postcodeLookup-filter"   -> Hidden,
        "3_postcodeLookup-postcode" -> Hidden,
        "3_postcodeLookupControl"   -> OptionResult(List("hidePostcodeLookup")),
        "details"                   -> Empty
      ),
      List(
        "",
        "Postcode lookup 1: ",
        "<p>Postcode lookup 1:</p>",
        "",
        "Postcode lookup 2: ",
        "<p>Postcode lookup 2:</p>",
        "",
        "Postcode lookup 3: ",
        "<p>Postcode lookup 3:</p>",
        "all addresses: ",
        "<p>all addresses:</p>"
      ),
      "postcode-lookup-atl.json no postcode lookup is visible"
    ),
    (
      MongoUserData(
        "addressInUK" -> Many(List("0"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "addressInUK"                  -> OptionResult(List("0")),
        "dummy"                        -> Hidden,
        "itmpAddressOverseas-city"     -> Hidden,
        "itmpAddressOverseas-country"  -> Hidden,
        "itmpAddressOverseas-line1"    -> Hidden,
        "itmpAddressOverseas-line2"    -> Hidden,
        "itmpAddressOverseas-line3"    -> Hidden,
        "itmpAddressOverseas-postcode" -> Hidden,
        "itmpAddressUk-country"        -> Hidden,
        "itmpAddressUk-postcode"       -> Hidden,
        "itmpAddressUk-street1"        -> Hidden,
        "itmpAddressUk-street2"        -> Hidden,
        "itmpAddressUk-street3"        -> Hidden,
        "itmpAddressUk-street4"        -> Hidden,
        "itmpAddressUk-uk"             -> Hidden
      ),
      List.empty[String],
      "itmp-address.json no itmp address"
    ),
    (
      MongoUserData(
        "itmpAddressOverseas-city"     -> One("Line 5"),
        "itmpAddressOverseas-country"  -> One("France"),
        "itmpAddressOverseas-line1"    -> One("Line 1"),
        "itmpAddressOverseas-line2"    -> One("Line 2"),
        "itmpAddressOverseas-line3"    -> One("Line 3 Line 4"),
        "itmpAddressOverseas-postcode" -> One("AA111AA")
      ),
      List(),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          itmpRetrievals = Some(
            ItmpRetrievals(
              itmpName = None,
              itmpDateOfBirth = None,
              itmpAddress = Some(
                ItmpAddress(
                  line1 = Some("Line 1"),
                  line2 = Some("Line 2"),
                  line3 = Some("Line 3"),
                  line4 = Some("Line 4"),
                  line5 = Some("Line 5"),
                  postCode = Some("AA111AA"),
                  countryCode = Some("FR"),
                  countryName = Some("France")
                )
              )
            )
          )
        )
      ),
      AnswerMap(
        "addressInUK"                  -> Hidden,
        "dummy"                        -> Empty,
        "itmpAddressOverseas-city"     -> StringResult("Line 5"),
        "itmpAddressOverseas-country"  -> StringResult("France"),
        "itmpAddressOverseas-line1"    -> StringResult("Line 1"),
        "itmpAddressOverseas-line2"    -> StringResult("Line 2"),
        "itmpAddressOverseas-line3"    -> StringResult("Line 3 Line 4"),
        "itmpAddressOverseas-postcode" -> StringResult("AA111AA"),
        "itmpAddressUk-country"        -> Hidden,
        "itmpAddressUk-postcode"       -> Hidden,
        "itmpAddressUk-street1"        -> Hidden,
        "itmpAddressUk-street2"        -> Hidden,
        "itmpAddressUk-street3"        -> Hidden,
        "itmpAddressUk-street4"        -> Hidden,
        "itmpAddressUk-uk"             -> Hidden
      ),
      List(
        "itmp address: Line 1, Line 2, Line 3, Line 4 Line 5, AA111AA, France",
        """|<p>itmp address: Line 1<br>
           | Line 2<br>
           | Line 3<br>
           | Line 4 Line 5<br>
           | AA111AA<br>
           | France</p>""".stripMargin
      ),
      "itmp-address.json overseas address prepopulation"
    ),
    (
      MongoUserData(
        "dummy"                  -> One(""),
        "itmpAddressUk-country"  -> One(""),
        "itmpAddressUk-postcode" -> One("AA11 1AA"),
        "itmpAddressUk-street1"  -> One("Line 1"),
        "itmpAddressUk-street2"  -> One("Line 2"),
        "itmpAddressUk-street3"  -> One("Line 3"),
        "itmpAddressUk-uk"       -> One("true")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          itmpRetrievals = Some(
            ItmpRetrievals(
              itmpName = None,
              itmpDateOfBirth = None,
              itmpAddress = Some(
                ItmpAddress(
                  line1 = Some("Line 1"),
                  line2 = Some("Line 2"),
                  line3 = Some("Line 3"),
                  line4 = Some("Line 4"),
                  line5 = Some("Line 5"),
                  postCode = Some("AA111AA"),
                  countryCode = Some("GB"),
                  countryName = Some("")
                )
              )
            )
          )
        )
      ),
      AnswerMap(
        "addressInUK"                  -> Hidden,
        "dummy"                        -> StringResult(""),
        "itmpAddressOverseas-city"     -> Hidden,
        "itmpAddressOverseas-country"  -> Hidden,
        "itmpAddressOverseas-line1"    -> Hidden,
        "itmpAddressOverseas-line2"    -> Hidden,
        "itmpAddressOverseas-line3"    -> Hidden,
        "itmpAddressOverseas-postcode" -> Hidden,
        "itmpAddressUk-country"        -> StringResult(""),
        "itmpAddressUk-postcode"       -> StringResult("AA11 1AA"),
        "itmpAddressUk-street1"        -> StringResult("Line 1"),
        "itmpAddressUk-street2"        -> StringResult("Line 2"),
        "itmpAddressUk-street3"        -> StringResult("Line 3"),
        "itmpAddressUk-street4"        -> Empty,
        "itmpAddressUk-uk"             -> StringResult("true")
      ),
      List(
        "itmp address: Line 1, Line 2, Line 3, Line 4 Line 5, AA111AA",
        """|<p>itmp address: Line 1<br>
           | Line 2<br>
           | Line 3<br>
           | Line 4 Line 5<br>
           | AA111AA</p>""".stripMargin
      ),
      "itmp-address.json uk address prepopulation"
    ),
    (
      MongoUserData(
        "address"          -> One(""),
        "address-country"  -> One(""),
        "address-postcode" -> One("TF3 4NT"),
        "address-street1"  -> One("Plaza 2"),
        "address-street2"  -> One("Ironmasters Way"),
        "address-street3"  -> One("Telford"),
        "address-street4"  -> One("Shropshire"),
        "address-uk"       -> One("true")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "agencyInfo" ->
              Map(
                DataRetrieve.Attribute("agencyAddress")  -> "Plaza 2 Ironmasters Way Telford Shropshire TF3 4NT",
                DataRetrieve.Attribute("agencyEmail")    -> "john@rogers.co.uk",
                DataRetrieve.Attribute("agencyName")     -> "Rogers associates",
                DataRetrieve.Attribute("address_line_1") -> "Plaza 2",
                DataRetrieve.Attribute("address_line_2") -> "Ironmasters Way",
                DataRetrieve.Attribute("postal_code")    -> "TF3 4NT",
                DataRetrieve.Attribute("region")         -> "Shropshire",
                DataRetrieve.Attribute("locality")       -> "Telford",
                DataRetrieve.Attribute("agencyPhone")    -> "01332752856",
                DataRetrieve.Attribute("country")        -> ""
              )
          )
        )
      ),
      AnswerMap(
        "address-country"          -> StringResult(""),
        "address-postcode"         -> StringResult("TF3 4NT"),
        "address-street1"          -> StringResult("Plaza 2"),
        "address-street2"          -> StringResult("Ironmasters Way"),
        "address-street3"          -> StringResult("Telford"),
        "address-street4"          -> StringResult("Shropshire"),
        "address-uk"               -> StringResult("true"),
        "overseasAddress-city"     -> Hidden,
        "overseasAddress-country"  -> Hidden,
        "overseasAddress-line1"    -> Hidden,
        "overseasAddress-line2"    -> Hidden,
        "overseasAddress-line3"    -> Hidden,
        "overseasAddress-postcode" -> Hidden
      ),
      List(
        "Enter date and ninos ''",
        "dataRetrieve.agencyInfo: Plaza 2, Ironmasters Way, Telford, Shropshire, TF3 4NT",
        """|<p>dataRetrieve.agencyInfo: Plaza 2<br>
           | Ironmasters Way<br>
           | Telford<br>
           | Shropshire<br>
           | TF3 4NT</p>""".stripMargin
      ),
      "data-retrieve-agent-details.json uk address"
    ),
    (
      MongoUserData(
        "dummy"                    -> One("dummy"),
        "info"                     -> One(""),
        "overseasAddress"          -> One(""),
        "overseasAddress-city"     -> One("Florida"),
        "overseasAddress-country"  -> One("US"),
        "overseasAddress-line1"    -> One("Plaza 2"),
        "overseasAddress-line2"    -> One("Sunset Boulevard"),
        "overseasAddress-line3"    -> One("Miami"),
        "overseasAddress-postcode" -> One("750075")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "agencyInfo" ->
              Map(
                DataRetrieve.Attribute("agencyAddress")  -> "Plaza 2 Sunset Boulevard Miami Florida 750075 US",
                DataRetrieve.Attribute("agencyEmail")    -> "mike@rogers.co.uk",
                DataRetrieve.Attribute("agencyName")     -> "Rogers USA",
                DataRetrieve.Attribute("address_line_2") -> "Sunset Boulevard",
                DataRetrieve.Attribute("postal_code")    -> "750075",
                DataRetrieve.Attribute("region")         -> "Florida",
                DataRetrieve.Attribute("locality")       -> "Miami",
                DataRetrieve.Attribute("address_line_1") -> "Plaza 2",
                DataRetrieve.Attribute("agencyPhone")    -> "01332752856",
                DataRetrieve.Attribute("country")        -> "US"
              )
          )
        )
      ),
      AnswerMap(
        "address-country"          -> Hidden,
        "address-postcode"         -> Hidden,
        "address-street1"          -> Hidden,
        "address-street2"          -> Hidden,
        "address-street3"          -> Hidden,
        "address-street4"          -> Hidden,
        "address-uk"               -> Hidden,
        "overseasAddress-city"     -> StringResult("Florida"),
        "overseasAddress-country"  -> StringResult("US"),
        "overseasAddress-line1"    -> StringResult("Plaza 2"),
        "overseasAddress-line2"    -> StringResult("Sunset Boulevard"),
        "overseasAddress-line3"    -> StringResult("Miami"),
        "overseasAddress-postcode" -> StringResult("750075")
      ),
      List(
        "Enter date and ninos 'US'",
        "dataRetrieve.agencyInfo: Plaza 2, Sunset Boulevard, Miami, Florida, 750075, US",
        """|<p>dataRetrieve.agencyInfo: Plaza 2<br>
           | Sunset Boulevard<br>
           | Miami<br>
           | Florida<br>
           | 750075<br>
           | US</p>""".stripMargin
      ),
      "data-retrieve-agent-details.json non uk address"
    ),
    (
      MongoUserData(
        "CRNrepresentativeMiniSummary" -> One(""),
        "crnRepresentative"            -> One("11111111"),
        "dummy"                        -> One("dummy")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "companyRepresentative" ->
              Map(
                DataRetrieve.Attribute("address_line_1") -> "1 Main Street",
                DataRetrieve.Attribute("address_line_2") -> "Office 234",
                DataRetrieve.Attribute("companyName")    -> "Acme Limited",
                DataRetrieve.Attribute("companyStatus")  -> "active",
                DataRetrieve.Attribute("country")        -> "UK",
                DataRetrieve.Attribute("dateOfCreation") -> "2020-02-01",
                DataRetrieve.Attribute("locality")       -> "London",
                DataRetrieve.Attribute("po_box")         -> "",
                DataRetrieve.Attribute("postal_code")    -> "E14 5AB",
                DataRetrieve.Attribute("region")         -> "London",
                DataRetrieve.Attribute("sicCode1")       -> "01220",
                DataRetrieve.Attribute("sicCode2")       -> "10390",
                DataRetrieve.Attribute("sicCode3")       -> "20110",
                DataRetrieve.Attribute("sicCode4")       -> "46450"
              )
          )
        ),
        lookupRegistry = lookupRegistry
      ),
      AnswerMap(),
      List(
        "11111111",
        "Acme Limited",
        "1 Main Street, Office 234, London, London, E14 5AB, UK",
        "1 February 2020",
        "01220 - Growing of tropical and subtropical fruits"
      ),
      "data-retrieve-company-representative.json Generated"
    ),
    (
      MongoUserData(
        "crn"         -> One("66666666"),
        "dummy"       -> One("dummy"),
        "miniSummary" -> One("")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "companyHouseInsolvency" ->
              List(
                Map(
                  DataRetrieve.Attribute("name")           -> "John Smith",
                  DataRetrieve.Attribute("role")           -> "receiver-manager",
                  DataRetrieve.Attribute("caseNumber")     -> "1",
                  DataRetrieve.Attribute("country")        -> "",
                  DataRetrieve.Attribute("appointedOn")    -> "2022-08-05",
                  DataRetrieve.Attribute("address_line_2") -> "Someburb",
                  DataRetrieve.Attribute("postal_code")    -> "SM1 2WH",
                  DataRetrieve.Attribute("region")         -> "",
                  DataRetrieve.Attribute("caseType")       -> "receiver-manager",
                  DataRetrieve.Attribute("locality")       -> "Somewheresville",
                  DataRetrieve.Attribute("address_line_1") -> "1 Somewhere St"
                ),
                Map(
                  DataRetrieve.Attribute("name")           -> "Jane Doe",
                  DataRetrieve.Attribute("role")           -> "receiver-manager",
                  DataRetrieve.Attribute("caseNumber")     -> "1",
                  DataRetrieve.Attribute("country")        -> "",
                  DataRetrieve.Attribute("appointedOn")    -> "2022-08-05",
                  DataRetrieve.Attribute("address_line_2") -> "Differentburb",
                  DataRetrieve.Attribute("postal_code")    -> "NW8 6WK",
                  DataRetrieve.Attribute("region")         -> "",
                  DataRetrieve.Attribute("caseType")       -> "receiver-manager",
                  DataRetrieve.Attribute("locality")       -> "Nowhere",
                  DataRetrieve.Attribute("address_line_1") -> "14 Some Place"
                ),
                Map(
                  DataRetrieve.Attribute("name")           -> "John Smith",
                  DataRetrieve.Attribute("role")           -> "receiver-manager",
                  DataRetrieve.Attribute("caseNumber")     -> "2",
                  DataRetrieve.Attribute("country")        -> "",
                  DataRetrieve.Attribute("appointedOn")    -> "2022-08-05",
                  DataRetrieve.Attribute("address_line_2") -> "Someburb",
                  DataRetrieve.Attribute("postal_code")    -> "SM1 2WH",
                  DataRetrieve.Attribute("region")         -> "",
                  DataRetrieve.Attribute("caseType")       -> "receiver-manager",
                  DataRetrieve.Attribute("locality")       -> "Somewheresville",
                  DataRetrieve.Attribute("address_line_1") -> "1 Somewhere St"
                ),
                Map(
                  DataRetrieve.Attribute("name")           -> "John McSmith",
                  DataRetrieve.Attribute("role")           -> "practitioner",
                  DataRetrieve.Attribute("caseNumber")     -> "3",
                  DataRetrieve.Attribute("country")        -> "",
                  DataRetrieve.Attribute("appointedOn")    -> "",
                  DataRetrieve.Attribute("address_line_2") -> "",
                  DataRetrieve.Attribute("postal_code")    -> "AN3 1WE",
                  DataRetrieve.Attribute("region")         -> "Anothershire",
                  DataRetrieve.Attribute("caseType")       -> "in-administration",
                  DataRetrieve.Attribute("locality")       -> "Anothertown",
                  DataRetrieve.Attribute("address_line_1") -> "58 Another Rd"
                )
              )
          )
        )
      ),
      AnswerMap(),
      List(
        "receiver-manager, receiver-manager, receiver-manager, in-administration",
        "1, 1, 2, 3",
        "John Smith, Jane Doe, John Smith, John McSmith",
        "receiver-manager, receiver-manager, receiver-manager, practitioner",
        "5 August 2022, 5 August 2022, 5 August 2022",
        "1 Somewhere St, Someburb, Somewheresville, SM1 2WH, 14 Some Place, Differentburb, Nowhere, NW8 6WK, 1 Somewhere St, Someburb, Somewheresville, SM1 2WH, 58 Another Rd, Anothertown, Anothershire, AN3 1WE",
        "1 Somewhere St, 14 Some Place, 1 Somewhere St, 58 Another Rd",
        "Someburb, Differentburb, Someburb"
      ),
      "data-retrieve-company-house-profile.json Generated"
    ),
    (
      MongoUserData(
        "accountNumber" -> One("86563611"),
        "companyName"   -> One("Lambent Illuminati"),
        "sortCode"      -> One("207102")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "bankDetails" ->
              Map(
                DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs") -> "no",
                DataRetrieve.Attribute("sortCodeSupportsDirectDebit")              -> "yes",
                DataRetrieve.Attribute("accountName")                              -> "Lambent Illumination",
                DataRetrieve.Attribute("sortCodeSupportsDirectCredit")             -> "no",
                DataRetrieve.Attribute("accountNumberIsWellFormatted")             -> "yes",
                DataRetrieve.Attribute("nameMatches")                              -> "partial",
                DataRetrieve.Attribute("sortCodeIsPresentOnEISCD")                 -> "yes",
                DataRetrieve.Attribute("sortCodeBankName")                         -> "BARCLAYS BANK UK PLC",
                DataRetrieve.Attribute("accountExists")                            -> "yes",
                DataRetrieve.Attribute("failureMaxAttempts")                       -> "7",
                DataRetrieve.Attribute("failureCount")                             -> "7",
                DataRetrieve.Attribute("failureCountResetTime")                    -> "2059-12-17T15:28:00"
              )
          )
        )
      ),
      AnswerMap("secret" -> Empty),
      List(
        "accountNumberIsWellFormatted: yes",
        "sortCodeIsPresentOnEISCD: yes",
        "sortCodeBankName: BARCLAYS BANK UK PLC",
        "nonStandardAccountDetailsRequiredForBacs: no",
        "accountExists: yes",
        "nameMatches: partial",
        "sortCodeSupportsDirectDebit: yes",
        "sortCodeSupportsDirectCredit: no",
        "accountName: Lambent Illumination",
        "failedCount: 7",
        "isBlocked: true",
        "unblockTime: 3:28pm",
        "unblockDate: 17 December 2059"
      ),
      "data-retrieve-business-bank-account-existence.json Generated"
    ),
    (
      MongoUserData(
        "accountNumber" -> One("56523611"),
        "sortCode"      -> One("207106")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "bankDetails" ->
              Map(
                DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs") -> "no",
                DataRetrieve.Attribute("sortCodeSupportsDirectDebit")              -> "yes",
                DataRetrieve.Attribute("sortCodeSupportsDirectCredit")             -> "yes",
                DataRetrieve.Attribute("sortCodeIsPresentOnEISCD")                 -> "yes",
                DataRetrieve.Attribute("iban")                                     -> "GB21BARC20710656523611",
                DataRetrieve.Attribute("isValid")                                  -> "yes",
                DataRetrieve.Attribute("sortCodeBankName")                         -> "BARCLAYS BANK UK PLC"
              )
          )
        )
      ),
      AnswerMap(),
      List(
        "isValid: yes",
        "sortCodeIsPresentOnEISCD: yes",
        "sortCodeBankName: BARCLAYS BANK UK PLC",
        "nonStandardAccountDetailsRequiredForBacs: no",
        "sortCodeSupportsDirectDebit: yes",
        "sortCodeSupportsDirectCredit: yes",
        "iban: GB21BARC20710656523611"
      ),
      "data-retrieve-validate-bank-details.json Generated"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty,
      AnswerMap(
        "overLimit"  -> Empty,
        "underLimit" -> Hidden
      ),
      List(
        "submissionReference: BPLX-FGU6-E7Q2",
        "id: form-template-ctx",
        "fileSizeLimit: 123",
        "lang: en"
      ),
      "form-template-ctx.json Generated"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "printAcknowledgementPdf: /acknowledgement/pdf/link-ctx",
        "printSummaryPdf: /summary/pdf/link-ctx",
        "newForm.other-template-id: /new-form/othertemplateid/clean",
        "newForm: /new-form/link-ctx",
        "newSession: /new-form/link-ctx/session",
        "newSession: /new-form/link-ctx/session",
        "download: /download?filename=attachement.pdf",
        "image: /image-file/link-ctx/-?fileName=nature.jpg",
        "pageId: /form/section/link-ctx/-/n0",
        "link: /redirect?redirectUrl=https%3A%2F%2Fwww.hmrc.gov.uk%2Fsubmissions"
      ),
      "link-ctx.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_atlDummy"        -> One("Dummy 1"),
        "1_name"            -> One("AAA"),
        "2_addAnotherField" -> Many(List("0")),
        "2_atlDummy"        -> One("Dummy 2"),
        "2_name"            -> One("BBB"),
        "3_addAnotherField" -> Many(List("1")),
        "3_atlDummy"        -> One("Dummy 3"),
        "3_name"            -> One("CCC"),
        "dummy"             -> One("AAA"),
        "finalDummy"        -> One("DDD")
      ),
      List(
        "n0",
        "ap1.1.0",
        "ap1.1.1",
        "ar1.1",
        "ap1.2.0",
        "ap1.2.1",
        "ar1.2",
        "ap1.3.0",
        "ap1.3.1",
        "ar1.3",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "[Refresh this page](/form/section/link-ctx-page-id-link/-/n0)",
        "<p><a href=\"/form/section/link-ctx-page-id-link/-/n0\" target=\"_blank\" class=\"govuk-link\">Refresh this page</a></p>",
        "[Go to previous page](/form/section/link-ctx-page-id-link/-/ap1.1.0)",
        "<p><a href=\"/form/section/link-ctx-page-id-link/-/ap1.1.0\" target=\"_blank\" class=\"govuk-link\">Go to previous page</a></p>",
        "AAA 1",
        "<p>AAA 1</p>",
        "[Go to previous page](/form/section/link-ctx-page-id-link/-/ap1.2.0)",
        "<p><a href=\"/form/section/link-ctx-page-id-link/-/ap1.2.0\" target=\"_blank\" class=\"govuk-link\">Go to previous page</a></p>",
        "BBB 2",
        "<p>BBB 2</p>",
        "[Go to previous page](/form/section/link-ctx-page-id-link/-/ap1.3.0)",
        "<p><a href=\"/form/section/link-ctx-page-id-link/-/ap1.3.0\" target=\"_blank\" class=\"govuk-link\">Go to previous page</a></p>",
        "CCC 3",
        "<p>CCC 3</p>",
        "[Go back and add or remove goods items](/form/section/link-ctx-page-id-link/-/ar1.3)",
        "<p><a href=\"/form/section/link-ctx-page-id-link/-/ar1.3\" target=\"_blank\" class=\"govuk-link\">Go back and add or remove goods items</a></p>"
      ),
      "link-ctx-page-id-link.json Generated"
    )
  )
}
