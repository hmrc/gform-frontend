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

package uk.gov.hmrc.gform.controllers.helpers

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.{ NotChecked, UserId }

class FormDataHelpersSpec extends Spec {

  "updateFormField" should "update FormField in form data" in {
    val formFields = Seq(
      FormField(FormComponentId("1"), "one"),
      FormField(FormComponentId("2"), "two"),
      FormField(FormComponentId("3"), "three"))

    val existingForm = Form(
      FormId("666"),
      EnvelopeId("id1"),
      UserId("usr"),
      FormTemplateId("temp"),
      FormData(formFields),
      Accepted,
      VisitIndex(Set.empty),
      ThirdPartyData(None, NotChecked, Map.empty, QueryParams.empty),
      None
    )

    val updatedForm = FormDataHelpers.updateFormField(existingForm, FormField(FormComponentId("2"), "xxx"))

    updatedForm.formData.fields contains Seq(
      FormField(FormComponentId("1"), "one"),
      FormField(FormComponentId("2"), "xxx"),
      FormField(FormComponentId("3"), "three"))
  }
}
