/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.foldable._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }
import ExpandUtils._
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.lookup.LookupExtractors
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.{ Many, One }
import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

/* class ExpandUtilsSpec
 *     extends FlatSpec with Matchers with PropertyChecks with RecalculatedFormDataSupport with FormModelSupport {
 *
 *   private val lookupExtractors = new LookupExtractors(Map.empty)
 *
 *   "submittedFCs" should "FormComponents reconstructed from data and ignore unrelated FormComponents" in {
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue(
 *       "repeatingSectionDriver",
 *       "repeatingSecondField",
 *       "repeatingThirdField-day",
 *       "repeatingThirdField-month",
 *       "repeatingThirdField-year",
 *       "repeatingFourthField-street1",
 *       "repeatingFourthField-street2",
 *       "repeatingFourthField-street3",
 *       "repeatingFourthField-street4",
 *       "repeatingFourthField-uk",
 *       "repeatingFourthField-postcode",
 *       "repeatingFourthField-country",
 *       "repeatingFifthField-1",
 *       "repeatingFifthField-2",
 *       "repeatingFifthField-3"
 *     )
 *
 *     val formComponents =
 *       mkFormComponent(FormComponentId("repeatingSectionDriver"), Text(BasicText, Value)) ::
 *         mkFormComponent(FormComponentId("repeatingSecondField"), Text(BasicText, Value)) ::
 *         mkFormComponent(FormComponentId("repeatingThirdField"), Date(AnyDate, Offset(0), None)) ::
 *         mkFormComponent(FormComponentId("repeatingFourthField"), Address(true)) ::
 *         mkFormComponent(FormComponentId("repeatingFifthField"), UkSortCode(Value)) :: Nil
 *
 *     val unrelated =
 *       mkFormComponent(FormComponentId("not-submitted-text"), Text(BasicText, Value)) ::
 *         mkFormComponent(FormComponentId("not-submitted-address"), Address(true)) ::
 *         mkFormComponent(FormComponentId("not-submitted-ukSortCode"), UkSortCode(Value)) :: Nil
 *
 *     val submittedFormComponents = submittedFCs(data, unrelated ++ formComponents)
 *
 *     submittedFormComponents shouldBe formComponents
 *
 *   }
 *
 *   "appendZeroPrefix" should "append 0_ prefix to FormComponentId if it doesn't have another prefix" in {
 *
 *     val formComponentIds = Table(
 *       // format: off
 *       ("input",     "output"),
 *       ("abd",       "0_abd"),
 *       ("1_abd",     "1_abd"),
 *       ("0_abd",     "0_abd"),
 *       ("1_",        "1_"),
 *       ("123_abd",   "123_abd"),
 *       ("123_",      "123_"),
 *       ("das123_dd", "0_das123_dd")
 *       // format: on
 *     )
 *
 *     forAll(formComponentIds) { (input, expectedOuput) ⇒
 *       appendZeroPrefix(FormComponentId(input)) shouldBe FormComponentId(expectedOuput)
 *     }
 *   }
 *
 *   "stripAnyPrefix" should "strip any prefix" in {
 *
 *     val formComponentIds = Table(
 *       // format: off
 *       ("input",       "output"),
 *       ("abd",         "abd"),
 *       ("1_abd",       "abd"),
 *       ("1_",          ""),
 *       ("1_1_",        "1_"),
 *       ("123_abd",     "abd"),
 *       ("123_",        ""),
 *       ("das123_dd",   "das123_dd"),
 *       ("1_test_1_1_", "test_1_1_")
 *       // format: on
 *     )
 *
 *     forAll(formComponentIds) { (input, expectedOuput) ⇒
 *       stripAnyPrefix(FormComponentId(input)) shouldBe FormComponentId(expectedOuput)
 *     }
 *   }
 *
 *   "stripZeroPrefix" should "strip zero prefix" in {
 *
 *     val formComponentIds = Table(
 *       // format: off
 *       ("input",       "output"),
 *       ("abd",         "abd"),
 *       ("0_abd",       "abd"),
 *       ("0_0_abd",     "0_abd"),
 *       ("1_abd",       "1_abd"),
 *       ("0_",          ""),
 *       ("1_",          "1_"),
 *       ("123_abd",     "123_abd"),
 *       ("123_",        "123_"),
 *       ("0123_",       "0123_"),
 *       ("das0_dd",     "das0_dd"),
 *       ("das123_dd",   "das123_dd"),
 *       ("0_test_1_1_", "test_1_1_"),
 *       ("1_test_1_1_", "1_test_1_1_")
 *       // format: on
 *     )
 *
 *     forAll(formComponentIds) { (input, expectedOuput) ⇒
 *       stripZeroPrefix(FormComponentId(input)) shouldBe FormComponentId(expectedOuput)
 *     }
 *   }
 *
 *   "groupIndex" should "return group fields ids with required index" in {
 *     val groupIds = "a" :: "b" :: "c" :: "d" :: Nil map FormComponentId.apply
 *     val group = mkGroup(groupIds)
 *
 *     val expectedForIndex2 = "1_a" :: "1_b" :: "1_c" :: "1_d" :: Nil map FormComponentId.apply
 *     val expectedForIndex5 = "4_a" :: "4_b" :: "4_c" :: "4_d" :: Nil map FormComponentId.apply
 *
 *     groupIndex(1, group) shouldBe groupIds
 *     groupIndex(2, group) shouldBe expectedForIndex2
 *     groupIndex(5, group) shouldBe expectedForIndex5
 *   }
 *
 *   "addNextGroup" should "add group containing simple components" in {
 *
 *     val groupIds = "a" :: "b" :: "c" :: "d" :: Nil map FormComponentId.apply
 *     val group = mkFormComponent(FormComponentId("HA"), mkGroup(groupIds))
 *
 *     val formData1 = variadicFormDataWithSingleValue("dummy", "a", "b", "c", "d")
 *     val formData2 = variadicFormDataWithSingleValue("dummy", "a", "b", "c", "d", "1_a", "1_b", "1_c", "1_d")
 *
 *     val expected1 = variadicFormDataWithSingleValue("", "1_a", "1_b", "1_c", "1_d")
 *     val expected2 = variadicFormDataWithSingleValue("", "2_a", "2_b", "2_c", "2_d")
 *
 *     val data1 = FormDataRecalculated.empty.copy(recData = RecData.fromData(formData1))
 *     val data2 = FormDataRecalculated.empty.copy(recData = RecData.fromData(formData2))
 *
 *     val data1Expected = FormDataRecalculated.empty.copy(recData = RecData.fromData(formData1 ++ expected1))
 *     val data2Expected = FormDataRecalculated.empty.copy(recData = RecData.fromData(formData2 ++ expected2))
 *
 *     addNextGroup(Some(group), data1, lookupExtractors) shouldBe ((data1Expected, Some("1_a")))
 *     addNextGroup(Some(group), data2, lookupExtractors) shouldBe ((data2Expected, Some("2_a")))
 *   }
 *
 *   it should "add group containing Date component" in {
 *     val groupIds = "a" :: Nil map FormComponentId.apply
 *     val group = mkFormComponent(FormComponentId("HA"), mkGroupWithDate(groupIds))
 *
 *     val formData1 = variadicFormDataWithSingleValue("dummy", "a-day", "a-month", "a-year")
 *     val formData2 =
 *       variadicFormDataWithSingleValue("dummy", "a-day", "a-month", "a-year", "1_a-day", "1_a-month", "1_a-year")
 *
 *     val expected1 = variadicFormDataWithSingleValue("", "1_a-day", "1_a-month", "1_a-year")
 *     val expected2 = variadicFormDataWithSingleValue("", "2_a-day", "2_a-month", "2_a-year")
 *
 *     val data1 = FormDataRecalculated.empty.copy(recData = RecData.fromData(formData1))
 *     val data2 = FormDataRecalculated.empty.copy(recData = RecData.fromData(formData2))
 *
 *     val data1Expected = FormDataRecalculated.empty.copy(recData = RecData.fromData(formData1 ++ expected1))
 *     val data2Expected = FormDataRecalculated.empty.copy(recData = RecData.fromData(formData2 ++ expected2))
 *
 *     addNextGroup(Some(group), data1, lookupExtractors) shouldBe ((data1Expected, Some("1_a-day")))
 *     addNextGroup(Some(group), data2, lookupExtractors) shouldBe ((data2Expected, Some("2_a-day")))
 *   }
 *
 *   "removeGroupFromData" should "remove group fields at particular index from data, and keep unrelated fields untouched" in {
 *
 *     val groupIds = "a" :: "b" :: "c" :: "d" :: Nil map FormComponentId.apply
 *     val group = mkFormComponent(FormComponentId("dummy"), mkGroup(groupIds))
 *
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue(
 *       "unrelated",
 *       "a",
 *       "b",
 *       "c",
 *       "d",
 *       "1_a",
 *       "1_b",
 *       "1_c",
 *       "1_d",
 *       "2_a",
 *       "2_b",
 *       "2_c",
 *       "2_d",
 *       "3_a",
 *       "3_b",
 *       "3_c",
 *       "3_d")
 *
 *     val res1 = removeGroupFromData(1, Some(group), data)
 *     val res2 = removeGroupFromData(2, Some(group), data)
 *     val res3 = removeGroupFromData(3, Some(group), data)
 *     val res4 = removeGroupFromData(4, Some(group), data)
 *
 *     val expected1 = mkFormDataRecalculated(
 *       "a"         -> "1_A",
 *       "b"         -> "1_B",
 *       "c"         -> "1_C",
 *       "d"         -> "1_D",
 *       "1_a"       -> "2_A",
 *       "1_b"       -> "2_B",
 *       "1_c"       -> "2_C",
 *       "1_d"       -> "2_D",
 *       "2_a"       -> "3_A",
 *       "2_b"       -> "3_B",
 *       "2_c"       -> "3_C",
 *       "2_d"       -> "3_D",
 *       "unrelated" -> "UNRELATED"
 *     )
 *     val expected2 = mkFormDataRecalculated(
 *       "a"         -> "A",
 *       "b"         -> "B",
 *       "c"         -> "C",
 *       "d"         -> "D",
 *       "1_a"       -> "2_A",
 *       "1_b"       -> "2_B",
 *       "1_c"       -> "2_C",
 *       "1_d"       -> "2_D",
 *       "2_a"       -> "3_A",
 *       "2_b"       -> "3_B",
 *       "2_c"       -> "3_C",
 *       "2_d"       -> "3_D",
 *       "unrelated" -> "UNRELATED"
 *     )
 *     val expected3 = mkFormDataRecalculated(
 *       "a"         -> "A",
 *       "b"         -> "B",
 *       "c"         -> "C",
 *       "d"         -> "D",
 *       "1_a"       -> "1_A",
 *       "1_b"       -> "1_B",
 *       "1_c"       -> "1_C",
 *       "1_d"       -> "1_D",
 *       "2_a"       -> "3_A",
 *       "2_b"       -> "3_B",
 *       "2_c"       -> "3_C",
 *       "2_d"       -> "3_D",
 *       "unrelated" -> "UNRELATED"
 *     )
 *     val expected4 = mkFormDataRecalculated(
 *       "a"         -> "A",
 *       "b"         -> "B",
 *       "c"         -> "C",
 *       "d"         -> "D",
 *       "1_a"       -> "1_A",
 *       "1_b"       -> "1_B",
 *       "1_c"       -> "1_C",
 *       "1_d"       -> "1_D",
 *       "2_a"       -> "2_A",
 *       "2_b"       -> "2_B",
 *       "2_c"       -> "2_C",
 *       "2_d"       -> "2_D",
 *       "unrelated" -> "UNRELATED"
 *     )
 *
 *     res1 shouldBe expected1
 *     res2 shouldBe expected2
 *     res3 shouldBe expected3
 *     res4 shouldBe expected4
 *   }
 *
 *   it should "remove group containing Date component" in {
 *     val groupIds = "a" :: Nil map FormComponentId.apply
 *     val group = mkFormComponent(FormComponentId("dummy"), mkGroupWithDate(groupIds))
 *
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue(
 *       "unrelated",
 *       "a-day",
 *       "a-month",
 *       "a-year",
 *       "1_a-day",
 *       "1_a-month",
 *       "1_a-year",
 *       "2_a-day",
 *       "2_a-month",
 *       "2_a-year")
 *
 *     val res1 = removeGroupFromData(1, Some(group), data)
 *     val res2 = removeGroupFromData(2, Some(group), data)
 *     val res3 = removeGroupFromData(3, Some(group), data)
 *
 *     val expected1 = mkFormDataRecalculated(
 *       "a-day"     -> "1_A-DAY",
 *       "a-month"   -> "1_A-MONTH",
 *       "a-year"    -> "1_A-YEAR",
 *       "1_a-day"   -> "2_A-DAY",
 *       "1_a-month" -> "2_A-MONTH",
 *       "1_a-year"  -> "2_A-YEAR",
 *       "unrelated" -> "UNRELATED"
 *     )
 *     val expected2 = mkFormDataRecalculated(
 *       "a-day"     -> "A-DAY",
 *       "a-month"   -> "A-MONTH",
 *       "a-year"    -> "A-YEAR",
 *       "1_a-day"   -> "2_A-DAY",
 *       "1_a-month" -> "2_A-MONTH",
 *       "1_a-year"  -> "2_A-YEAR",
 *       "unrelated" -> "UNRELATED"
 *     )
 *     val expected3 = mkFormDataRecalculated(
 *       "a-day"     -> "A-DAY",
 *       "a-month"   -> "A-MONTH",
 *       "a-year"    -> "A-YEAR",
 *       "1_a-day"   -> "1_A-DAY",
 *       "1_a-month" -> "1_A-MONTH",
 *       "1_a-year"  -> "1_A-YEAR",
 *       "unrelated" -> "UNRELATED"
 *     )
 *
 *     res1 shouldBe expected1
 *     res2 shouldBe expected2
 *     res3 shouldBe expected3
 *   }
 *
 *   "getAllFieldsInGroup" should "return List of GroupList constructed from data" in {
 *
 *     val groupIds = "a" :: "b" :: "c" :: "d" :: Nil map FormComponentId.apply
 *
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue(
 *       "unrelated",
 *       "a",
 *       "b",
 *       "c",
 *       "d",
 *       "1_a",
 *       "1_b",
 *       "1_c",
 *       "1_d",
 *       "2_a",
 *       "2_b",
 *       "2_c",
 *       "2_d",
 *       "3_a",
 *       "3_b",
 *       "3_c",
 *       "3_d")
 *
 *     val group = mkGroup(groupIds)
 *     val formComponent = mkFormComponent(FormComponentId("dummy"), group)
 *     val res = getAllFieldsInGroup(formComponent, group, data)
 *
 *     val expectedChoice =
 *       List(
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("d"), Text(BasicText, Value))
 *           )),
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("1_a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("1_b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("1_c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("1_d"), Text(BasicText, Value))
 *           )),
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("2_a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("2_b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("2_c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("2_d"), Text(BasicText, Value))
 *           )),
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("3_a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("3_b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("3_c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("3_d"), Text(BasicText, Value))
 *           ))
 *       )
 *
 *     res shouldBe expectedChoice
 *
 *   }
 *
 *   "fillToMin" should "fill group to required min size" in {
 *
 *     def toComponent(fcId: String): FormComponent = mkFormComponent(FormComponentId(fcId), Text(BasicText, Value))
 *
 *     val groupIds = "a" :: "b" :: "c" :: "d" :: Nil map FormComponentId.apply
 *     val group = mkGroup(groupIds).copy(repeatsMin = Some(3))
 *
 *     val first = "a" :: "b" :: "c" :: "d" :: Nil map toComponent
 *     val second = "1_a" :: "1_b" :: "1_c" :: "1_d" :: Nil map toComponent
 *     val third = "2_a" :: "2_b" :: "2_c" :: "2_d" :: Nil map toComponent
 *     val fourth = "3_a" :: "3_b" :: "3_c" :: "3_d" :: Nil map toComponent
 *
 *     val oneGroup = GroupList(first) :: Nil
 *     val twoGroups = GroupList(first) :: GroupList(second) :: Nil
 *     val threeGroups = GroupList(first) :: GroupList(second) :: GroupList(third) :: Nil
 *     val fourGroups = GroupList(first) :: GroupList(second) :: GroupList(third) :: GroupList(fourth) :: Nil
 *
 *     val res1 = fillToMin(oneGroup, group)
 *     val res2 = fillToMin(twoGroups, group)
 *     val res3 = fillToMin(threeGroups, group)
 *     val res4 = fillToMin(fourGroups, group)
 *
 *     val expectedChoice =
 *       List(
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("d"), Text(BasicText, Value))
 *           )),
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("1_a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("1_b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("1_c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("1_d"), Text(BasicText, Value))
 *           )),
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("2_a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("2_b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("2_c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("2_d"), Text(BasicText, Value))
 *           ))
 *       )
 *
 *     res1 shouldBe expectedChoice
 *     res2 shouldBe expectedChoice
 *     res3 shouldBe expectedChoice
 *
 *     val expected4 =
 *       List(
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("d"), Text(BasicText, Value))
 *           )),
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("1_a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("1_b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("1_c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("1_d"), Text(BasicText, Value))
 *           )),
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("2_a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("2_b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("2_c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("2_d"), Text(BasicText, Value))
 *           )),
 *         GroupList(
 *           List(
 *             mkFormComponent(FormComponentId("3_a"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("3_b"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("3_c"), Text(BasicText, Value)),
 *             mkFormComponent(FormComponentId("3_d"), Text(BasicText, Value))
 *           ))
 *       )
 *
 *     res4 shouldBe expected4
 *
 *   }
 *
 *   private def mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue(fcIds: String*): FormDataRecalculated =
 *     mkFormDataRecalculated(fcIds.map(id => (id, id.toUpperCase)): _*)
 *
 *   "getAlwaysEmptyHiddenGroup" should "should ignore Choice which is not part of a Group" in {
 *
 *     val section = mkSection(mkFormComponent(FormComponentId("a"), choice) :: Nil)
 *
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue("a")
 *
 *     val singleton = getSingleton(section, data)
 *
 *     val res = getAlwaysEmptyHiddenGroup(data, singleton, lookupExtractors)
 *
 *     res shouldBe empty
 *   }
 *
 *   it should "return base FormComponent of group if submitted data are empty" in {
 *
 *     val section = mkSectionWithGroupHaving(
 *       List(
 *         "a" -> choice
 *       )
 *     )
 *
 *     val data = FormDataRecalculated.empty
 *
 *     val singleton = getSingleton(section, data)
 *
 *     val res = getAlwaysEmptyHiddenGroup(data, singleton, lookupExtractors)
 *
 *     val expectedChoice = "a" :: Nil map (id => mkFormComponent(FormComponentId(id), choice))
 *
 *     res shouldBe expectedChoice
 *
 *   }
 *
 *   it should "return base FormComponent of a group if submitted data contains only base formComponent id" in {
 *
 *     val section = mkSectionWithGroupHaving(
 *       List(
 *         "a" -> choice
 *       )
 *     )
 *
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue("a")
 *
 *     val singleton = getSingleton(section, data)
 *
 *     val res = getAlwaysEmptyHiddenGroup(data, singleton, lookupExtractors)
 *
 *     val expectedChoice = "a" :: Nil map (id => mkFormComponent(FormComponentId(id), choice))
 *
 *     res shouldBe expectedChoice
 *
 *   }
 *
 *   it should "return base FormComponent, first and second index of a group" in {
 *
 *     val section = mkSectionWithGroupHaving(
 *       List(
 *         "a" -> choice
 *       )
 *     )
 *
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue("a", "1_a", "2_a")
 *
 *     val singleton = getSingleton(section, data)
 *
 *     val res = getAlwaysEmptyHiddenGroup(data, singleton, lookupExtractors)
 *
 *     val expectedChoice = "a" :: "1_a" :: "2_a" :: Nil map (id => mkFormComponent(FormComponentId(id), choice))
 *
 *     res shouldBe expectedChoice
 *
 *   }
 *
 *   it should "handle group with multiple fields" in {
 *
 *     val section = mkSectionWithGroupHaving(
 *       List(
 *         "a" -> choice,
 *         "b" -> choice
 *       )
 *     )
 *
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue("a", "b")
 *
 *     val singleton = getSingleton(section, data)
 *
 *     val res = getAlwaysEmptyHiddenGroup(data, singleton, lookupExtractors)
 *
 *     val expectedChoice = "a" :: "b" :: Nil map (id => mkFormComponent(FormComponentId(id), choice))
 *
 *     res shouldBe expectedChoice
 *
 *   }
 *
 *   it should "disregard non choice FormComponents" in {
 *
 *     val section = mkSectionWithGroupHaving(
 *       List(
 *         "a" -> choice,
 *         "b" -> choice,
 *         "c" -> Text(BasicText, Value)
 *       )
 *     )
 *
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue("a", "b", "c", "1_a", "1_b", "1_c")
 *
 *     val singleton = getSingleton(section, data)
 *
 *     val res = getAlwaysEmptyHiddenGroup(data, singleton, lookupExtractors)
 *
 *     val expectedChoice = "a" :: "b" :: "1_a" :: "1_b" :: Nil map (id => mkFormComponent(FormComponentId(id), choice))
 *
 *     res shouldBe expectedChoice
 *
 *   }
 *
 *   it should "handle FormComponent of type InformationMessage and FileUpload" in {
 *
 *     val section = mkSectionWithGroupHaving(
 *       List(
 *         "a" -> choice,
 *         "b" -> informationMessage,
 *         "c" -> FileUpload(),
 *         "d" -> Text(BasicText, Value)
 *       )
 *     )
 *
 *     val data =
 *       mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue("a", "b", "c", "d", "1_a", "1_b", "1_c", "1_d")
 *
 *     val singleton = getSingleton(section, data)
 *
 *     val emptyHidden = getAlwaysEmptyHiddenGroup(data, singleton, lookupExtractors)
 *
 *     val expectedChoice = "a" :: "1_a" :: Nil map (id => mkFormComponent(FormComponentId(id), choice))
 *     val expectedInfo = "b" :: "1_b" :: Nil map (id => mkFormComponent(FormComponentId(id), informationMessage))
 *     val expectedFileUpload = "c" :: "1_c" :: Nil map (id => mkFormComponent(FormComponentId(id), FileUpload()))
 *
 *     emptyHidden should contain theSameElementsAs (expectedChoice ++ expectedInfo ++ expectedFileUpload)
 *
 *   }
 *
 *   "getHiddenTemplateFields" should "produce list of hidden components and data with empty values for all choice components present on the page " in {
 *     val section = mkSectionWithGroupHaving(
 *       List(
 *         "a" -> choice,
 *         "b" -> choice,
 *         "c" -> Text(BasicText, Value)
 *       )
 *     )
 *
 *     val sections = section ::
 *       mkSection(mkFormComponent(FormComponentId("e"), Text(BasicText, Value)) :: Nil) ::
 *       mkSection(mkFormComponent(FormComponentId("f"), Text(BasicText, Value)) :: Nil) :: Nil
 *
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue("a", "b", "c", "1_a", "1_b", "1_c", "e", "f")
 *
 *     val singleton = getSingleton(section, data)
 *     val formModel = mkFormModel(sections, data)
 *
 *     val (hiddenFormComponent, dataUpd) = Fields.getHiddenTemplateFields(singleton, formModel, data, lookupExtractors)
 *
 *     val expectedFC = List(
 *       "e"   -> Text(BasicText, Value),
 *       "f"   -> Text(BasicText, Value),
 *       "a"   -> choice,
 *       "b"   -> choice,
 *       "1_a" -> choice,
 *       "1_b" -> choice
 *     ) map { case (id, comp) => mkFormComponent(FormComponentId(id), comp) }
 *
 *     val expectedData = mkVariadicFormDataRecalculated(
 *       "a"   -> Many(Seq.empty),
 *       "1_a" -> Many(Seq.empty),
 *       "b"   -> Many(Seq.empty),
 *       "1_b" -> Many(Seq.empty),
 *       "c"   -> One("C"),
 *       "1_c" -> One("1_C"),
 *       "e"   -> One("E"),
 *       "f"   -> One("F")
 *     )
 *
 *     dataUpd shouldBe expectedData
 *     hiddenFormComponent shouldBe expectedFC
 *   }
 *
 *   it should "render choice (outside of group) always as empty on current page" in {
 *     val section = mkSection(mkFormComponent(FormComponentId("a"), choice) :: Nil)
 *
 *     val sections = section ::
 *       mkSection(mkFormComponent(FormComponentId("b"), choice) :: Nil) ::
 *       mkSection(mkFormComponent(FormComponentId("c"), Text(BasicText, Value)) :: Nil) :: Nil
 *
 *     val data = mkFormDataRecalculatedUsingUpperCaseFormComponentIdForValue("a", "b", "c")
 *
 *     val singleton = getSingleton(section, data)
 *     val formModel = mkFormModel(sections, data)
 *
 *     val (hiddenFormComponent, dataUpd) = Fields.getHiddenTemplateFields(singleton, formModel, data, lookupExtractors)
 *
 *     val expectedData = mkVariadicFormDataRecalculated(
 *       "a" -> Many(Seq()),
 *       "b" -> One("B"),
 *       "c" -> One("C")
 *     )
 *
 *     val expectedFC = List(
 *       "a" -> choice,
 *       "b" -> choice,
 *       "c" -> Text(BasicText, Value)
 *     ) map { case (id, comp) => mkFormComponent(FormComponentId(id), comp) }
 *
 *     dataUpd shouldBe expectedData
 *     hiddenFormComponent should contain theSameElementsAs expectedFC
 *
 *   }
 *
 *   "hiddenFileUploads" should "return FileUpload components which are not part of a group" in {
 *     val fileUploadOutsideOfGroup = mkFormComponent(FormComponentId("a"), FileUpload())
 *     val fileUpload = mkFormComponent(FormComponentId("b"), FileUpload())
 *     val text = mkFormComponent(FormComponentId("c"), Text(BasicText, Value))
 *
 *     val group = mkGroupWith(fileUpload :: Nil)
 *     val fileUploadInAGroup = mkFormComponent(FormComponentId("dummy"), group)
 *
 *     val section = mkSection(text :: fileUploadOutsideOfGroup :: fileUploadInAGroup :: Nil)
 *     val singleton = getSingleton(section)
 *     val res = hiddenFileUploads(singleton)
 *
 *     res shouldBe List(fileUploadOutsideOfGroup)
 *   }
 *
 *   private def mkSectionWithGroupHaving(xs: List[(String, ComponentType)]): Section.NonRepeatingPage = {
 *     val fields = xs.map { case (id, comp) => mkFormComponent(FormComponentId(id), comp) }
 *
 *     val group = mkGroupWith(fields)
 *
 *     mkSection(mkFormComponent(FormComponentId("dummy"), group) :: Nil)
 *   }
 *
 *   val choice =
 *     Choice(YesNo, NonEmptyList.of(toSmartString("yes"), toSmartString("no")), Vertical, List.empty, None)
 *   val informationMessage = InformationMessage(StandardInfo, toSmartString("info-text"))
 *
 *   def mkGroup(groupIds: List[FormComponentId]) =
 *     Group(
 *       groupIds.map(fcId => mkFormComponent(fcId, Text(BasicText, Value))),
 *       Vertical,
 *       repeatsMax = Some(5),
 *       repeatsMin = None,
 *       None,
 *       None
 *     )
 *
 *   def mkGroupWithDate(groupIds: List[FormComponentId]) =
 *     Group(
 *       groupIds.map(fcId => mkFormComponent(fcId, Date(AnyDate, Offset(0), None))),
 *       Vertical,
 *       repeatsMax = Some(5),
 *       repeatsMin = None,
 *       None,
 *       None
 *     )
 *
 *   def mkGroupWith(fcIds: List[FormComponent]) =
 *     Group(
 *       fcIds,
 *       Vertical,
 *       repeatsMax = Some(5),
 *       repeatsMin = None,
 *       None,
 *       None
 *     )
 *
 *   private def mkFormComponent(fcId: FormComponentId, ct: ComponentType) =
 *     FormComponent(
 *       fcId,
 *       ct,
 *       toSmartString("some-component"),
 *       None,
 *       None,
 *       None,
 *       true,
 *       false,
 *       true,
 *       false,
 *       false,
 *       None,
 *       None
 *     )
 * } */
