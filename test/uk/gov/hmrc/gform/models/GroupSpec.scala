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

package uk.gov.hmrc.gform.models

import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class GroupSpec extends Spec {

  "dataEntered" should "return true if data in the group's fields exist" in {

    val formData1: Map[FormComponentId, Seq[String]] = Map(FormComponentId("tid")  -> Seq("nonempty"))
    val formData2: Map[FormComponentId, Seq[String]] = Map(FormComponentId("tid")  -> Seq(""))
    val formData3: Map[FormComponentId, Seq[String]] = Map(FormComponentId("tid")  -> Seq("", ""))
    val formData4: Map[FormComponentId, Seq[String]] = Map(FormComponentId("tid")  -> Seq("", "", "nonempty"))
    val formData5: Map[FormComponentId, Seq[String]] = Map(FormComponentId("tid2") -> Seq("", "", "nonempty"))

    val grp: Group = Group(
      List(
        FormComponent(
          FormComponentId("tid"),
          Text(AnyText, Value),
          toLocalisedString("tlabel"),
          None,
          None,
          None,
          false,
          true,
          true,
          true,
          false,
          None)),
      Horizontal,
      Some(5),
      Some(1),
      Some(toLocalisedString("repeatLabel")),
      Some(toLocalisedString("repeatAddAnotherText"))
    )

    FormDataHelpers.dataEnteredInGroup(grp, Map.empty[FormComponentId, Seq[String]]) should be(false)
    FormDataHelpers.dataEnteredInGroup(grp, formData2) should be(false)
    FormDataHelpers.dataEnteredInGroup(grp, formData3) should be(false)
    FormDataHelpers.dataEnteredInGroup(grp, formData1) should be(true)
    FormDataHelpers.dataEnteredInGroup(grp, formData4) should be(true)
    FormDataHelpers.dataEnteredInGroup(grp, formData5) should be(false)
  }
}
