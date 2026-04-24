/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.controllers.helpers

import java.time.Instant
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.BooleanExprCache
import uk.gov.hmrc.gform.sharedmodel.form.{ FormComponentIdToFileIdMapping, QueryParams, TaskIdTaskStatusMapping }
import uk.gov.hmrc.gform.sharedmodel.{ NotChecked, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ Accepted, EnvelopeId, Form, FormData, FormField, FormId, ThirdPartyData, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }

class FormDataHelpersSpec extends Spec {

  private def toFormFields(xs: List[(String, String)]): List[FormField] = xs.map { case (fcId, value) =>
    FormField(FormComponentId(fcId).modelComponentId, value)
  }

  "updateFormField" should "update FormField in form data" in {
    val formFields = toFormFields(
      List(
        "1" -> "one",
        "2" -> "two",
        "3" -> "three"
      )
    )

    val existingForm = Form(
      FormId("666"),
      EnvelopeId("id1"),
      UserId("usr"),
      FormTemplateId("temp"),
      None,
      FormData(formFields),
      Accepted,
      VisitIndex.Classic(Set.empty),
      ThirdPartyData(
        NotChecked,
        Map.empty,
        QueryParams.empty,
        None,
        BooleanExprCache.empty,
        None,
        None,
        None,
        None,
        None,
        None,
        None
      ),
      None,
      FormComponentIdToFileIdMapping.empty,
      TaskIdTaskStatusMapping.empty,
      Instant.now
    )

    val updatedForm =
      FormDataHelpers.updateFormField(existingForm, FormField(FormComponentId("2").modelComponentId, "xxx"))

    updatedForm.formData.fields contains toFormFields(List(("1", "one"), ("2", "xxx"), ("3", "three")))
  }
}
