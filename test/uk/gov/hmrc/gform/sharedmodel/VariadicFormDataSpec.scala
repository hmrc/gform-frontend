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

package uk.gov.hmrc.gform.sharedmodel

import cats.data.NonEmptyList
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.{ DependencyGraphVerification, FormModel, FormModelSupport, SectionSelectorType }
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import VariadicValue.{ Many, One }
import VariadicFormData.{ manys, ones }

import scala.util.{ Failure, Success, Try }

class VariadicFormDataSpec extends FlatSpec with Matchers with FormModelSupport {

  private def mkModelComponentId(value: String) =
    ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId(value)))

  private val aFormComponentId = mkModelComponentId("a")
  private val bFormComponentId = mkModelComponentId("b")
  private val cFormComponentId = mkModelComponentId("c")

  "get" should "return None if no value can be found" in {
    VariadicFormData.empty.get(aFormComponentId) shouldBe None
  }

  it should "return Some(value) if a value can be found" in {
    val data = VariadicFormData(Map(aFormComponentId -> One("x"), bFormComponentId -> Many(Seq("y"))))

    data.get(aFormComponentId) shouldBe Some(One("x"))
    data.get(bFormComponentId) shouldBe Some(Many(Seq("y")))
  }

  "one" should "return None if no value can be found" in {
    VariadicFormData.empty.one(aFormComponentId) shouldBe None
  }

  it should "return Some(value) if the value can be found and is a One" in {
    VariadicFormData(Map(aFormComponentId -> One("x"))).one(aFormComponentId) shouldBe Some("x")
  }

  it should "throw an exception if the value can be found but is a Many" in {
    Try(VariadicFormData(Map(aFormComponentId -> Many(Seq("x")))).one(aFormComponentId)) match {
      case Failure(_) =>
      case Success(_) => fail
    }
  }

  "oneOrElse" should "return the default value if not value can be found" in {
    VariadicFormData.empty.oneOrElse(aFormComponentId, "x") shouldBe "x"
  }

  it should "return the bound value if the value can be found and is a One" in {
    VariadicFormData(Map(aFormComponentId -> One("x"))).oneOrElse(aFormComponentId, "y") shouldBe "x"
  }

  it should "throw an exception if the value can be found but is a Many" in {
    Try(VariadicFormData(Map(aFormComponentId -> Many(Seq("x")))).oneOrElse(aFormComponentId, "y")) match {
      case Failure(_) =>
      case Success(_) => fail
    }
  }

  "many" should "return None if no value can be found" in {
    VariadicFormData.empty.many(aFormComponentId) shouldBe None
  }

  it should "return Some(value) if the value can be found and is a Many" in {
    VariadicFormData(Map(aFormComponentId -> Many(Seq("x")))).many(aFormComponentId) shouldBe Some(Seq("x"))
  }

  it should "throw an exception if the value can be found but is a One" in {
    Try(VariadicFormData(Map(aFormComponentId -> One("x"))).many(aFormComponentId)) match {
      case Failure(_) =>
      case Success(_) => fail
    }
  }

  "++" should "combine all bindings in the two collections" in {
    val data = ones(aFormComponentId -> "x") ++
      manys(bFormComponentId         -> Seq("y"))

    data.get(aFormComponentId) shouldBe Some(One("x"))
    data.get(bFormComponentId) shouldBe Some(Many(Seq("y")))
  }

  it should "overwrite bindings in the first collection with those in the second" in {
    val first: VariadicFormData[SourceOrigin.Current] =
      ones(aFormComponentId    -> "x") ++
        manys(bFormComponentId -> Seq("y"))

    val second: VariadicFormData[SourceOrigin.Current] = manys(bFormComponentId -> Seq("z"))

    val data: VariadicFormData[SourceOrigin.Current] = first ++ second

    data.get(aFormComponentId) shouldBe Some(One("x"))
    data.get(bFormComponentId) shouldBe Some(Many(Seq("z")))
  }

  "keySet" should "return all the keys" in {
    (ones(aFormComponentId   -> "x") ++
      manys(bFormComponentId -> Seq("y"))).keySet shouldBe Set(aFormComponentId, bFormComponentId)
  }

  "addValue" should "add the value" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addValue (bFormComponentId -> Many(Seq("y")))

    data.one(aFormComponentId) shouldBe Some("x")
    data.many(bFormComponentId) shouldBe Some(Seq("y"))
  }

  it should "overwrite any existing binding with the same key" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addValue (aFormComponentId -> Many(Seq("y")))

    data.many(aFormComponentId) shouldBe Some(List("y"))
  }

  "addOne" should "add the value" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addOne (bFormComponentId -> "y")

    data.one(aFormComponentId) shouldBe Some("x")
    data.one(bFormComponentId) shouldBe Some("y")
  }

  it should "overwrite any existing binding with the same key" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addOne (aFormComponentId -> "y")

    data.one(aFormComponentId) shouldBe Some("y")
  }

  "addMany" should "add the values" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addMany (bFormComponentId -> Seq("y"))

    data.one(aFormComponentId) shouldBe Some("x")
    data.many(bFormComponentId) shouldBe Some(Seq("y"))
  }

  it should "overwrite any existing binding with the same key" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addMany (aFormComponentId -> Seq("y"))

    data.many(aFormComponentId) shouldBe Some(Seq("y"))
  }

  "--" should "remove all bindings with the given keys" in {
    val data = ones(aFormComponentId -> "x") ++
      manys(bFormComponentId         -> Seq("y"))

    data -- Set(aFormComponentId) shouldBe manys(bFormComponentId -> Seq("y"))
    data -- Set(bFormComponentId) shouldBe ones(aFormComponentId  -> "x")
  }

  it should "do nothing if the key doesn't have a binding" in {
    ones(aFormComponentId -> "x") -- Seq(bFormComponentId) shouldBe ones(aFormComponentId -> "x")
  }

  "collect" should "return the matching mapped values" in {
    val data =
      ones(aFormComponentId -> "One thing", bFormComponentId -> "Another One thing", cFormComponentId -> "Whatever")

    data.collect { case (k, One(v)) if v.contains("One") => k }.toSet shouldBe Set(aFormComponentId, bFormComponentId)
  }

  "contains" should "indicate if the FormComponentId has a binding" in {
    val data = ones(aFormComponentId -> "One thing", bFormComponentId -> "Another One thing")

    data.contains(aFormComponentId) shouldBe true
    data.contains(bFormComponentId) shouldBe true
    data.contains(cFormComponentId) shouldBe false
  }

  "mapValues" should "map the values" in {
    val data =
      ones(aFormComponentId    -> "Value") ++
        manys(bFormComponentId -> Seq("First", "Second"))

    data.mapValues {
      case (_, One(v))   => Many(Seq(v))
      case (_, Many(vs)) => One(vs.head)
    } should be
    manys(aFormComponentId  -> Seq("Value")) ++
      ones(bFormComponentId -> "First")
  }

  "buildFromMongoData" should "create values of the right VariadicValue type" in {
    val formTemplate = mkFormTemplate(
      mkSection(
        mkFormComponent(
          "a",
          Choice(Radio, NonEmptyList.one(toSmartString("Option A")), Vertical, List.empty[Int], None))))
    val fmb = mkFormModelBuilder(formTemplate)

    val formModel: FormModel[DependencyGraphVerification] = fmb.dependencyGraphValidation[SectionSelectorType.Normal]
    VariadicFormData.buildFromMongoData(
      formModel,
      Map(aFormComponentId -> "1, 2, ", bFormComponentId -> "3, 4, 5, ")
    ) shouldBe (VariadicFormData.manys(aFormComponentId -> Seq("1", "2")) ++ VariadicFormData.ones(
      bFormComponentId                                  -> "3, 4, 5, "))
  }

  it should "re-index group fields when field in group is un-indexed" in {
    val groupModelComponentId = ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId("group")))
    val text1ModelComponentId = ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId("text1")))
    val text2ModelComponentId = ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId("text2")))

    val groupModelComponentIdAtIndex1 = ModelComponentId.pure(IndexedComponentId.indexed(BaseComponentId("group"), 1))
    val text1ModelComponentIdAtIndex1 = ModelComponentId.pure(IndexedComponentId.indexed(BaseComponentId("text1"), 1))
    val text2ModelComponentIdAtIndex1 = ModelComponentId.pure(IndexedComponentId.indexed(BaseComponentId("text2"), 1))

    val formTemplate = mkFormTemplate(
      mkSection(
        mkFormComponent(
          "group",
          mkGroup(1, List(mkFormComponent("text1", Constant("")), mkFormComponent("text2", Constant("")))))))
    val formModel: FormModel[DependencyGraphVerification] =
      mkFormModelBuilder(formTemplate).dependencyGraphValidation[SectionSelectorType.Normal]

    val result = VariadicFormData.buildFromMongoData(
      formModel,
      Map(
        groupModelComponentId         -> "",
        text1ModelComponentId         -> "text1Value",
        text2ModelComponentId         -> "text2Value",
        groupModelComponentIdAtIndex1 -> "",
        text1ModelComponentIdAtIndex1 -> "text1Value1",
        text2ModelComponentIdAtIndex1 -> "text2Value1"
      )
    )

    result.data shouldBe Map(
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("group"), 1)) -> One(""),
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("text1"), 1)) -> One("text1Value"),
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("text2"), 1)) -> One("text2Value"),
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("group"), 2)) -> One(""),
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("text1"), 2)) -> One("text1Value1"),
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("text2"), 2)) -> One("text2Value1")
    )
  }

  it should "not re-index group fields when all fields in the group are indexed" in {
    val groupModelComponentIdAtIndex1 = ModelComponentId.pure(IndexedComponentId.indexed(BaseComponentId("group"), 1))
    val text1ModelComponentIdAtIndex1 = ModelComponentId.pure(IndexedComponentId.indexed(BaseComponentId("text1"), 1))
    val text2ModelComponentIdAtIndex1 = ModelComponentId.pure(IndexedComponentId.indexed(BaseComponentId("text2"), 1))

    val groupModelComponentIdAtIndex2 = ModelComponentId.pure(IndexedComponentId.indexed(BaseComponentId("group"), 2))
    val text1ModelComponentIdAtIndex2 = ModelComponentId.pure(IndexedComponentId.indexed(BaseComponentId("text1"), 2))
    val text2ModelComponentIdAtIndex2 = ModelComponentId.pure(IndexedComponentId.indexed(BaseComponentId("text2"), 2))

    val formTemplate = mkFormTemplate(
      mkSection(
        mkFormComponent(
          "group",
          mkGroup(1, List(mkFormComponent("text1", Constant("")), mkFormComponent("text2", Constant("")))))))
    val formModel: FormModel[DependencyGraphVerification] =
      mkFormModelBuilder(formTemplate).dependencyGraphValidation[SectionSelectorType.Normal]

    val result = VariadicFormData.buildFromMongoData(
      formModel,
      Map(
        groupModelComponentIdAtIndex1 -> "",
        text1ModelComponentIdAtIndex1 -> "text1Value1",
        text2ModelComponentIdAtIndex1 -> "text2Value1",
        groupModelComponentIdAtIndex2 -> "",
        text1ModelComponentIdAtIndex2 -> "text1Value2",
        text2ModelComponentIdAtIndex2 -> "text2Value2"
      )
    )

    result.data shouldBe Map(
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("group"), 1)) -> One(""),
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("text1"), 1)) -> One("text1Value1"),
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("text2"), 1)) -> One("text2Value1"),
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("group"), 2)) -> One(""),
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("text1"), 2)) -> One("text1Value2"),
      ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId("text2"), 2)) -> One("text2Value2")
    )
  }
}
