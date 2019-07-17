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

package uk.gov.hmrc.gform.gform.handlers

import play.api.libs.json.Json
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

class DeclarationControllerRequestHandlerSpec extends Spec {

  it should "handle UpdatedFormRequest" in new DeclarationControllerRequestHandler {

    val actual: Option[Form] =
      handleUpdatedFormRequest(Option(requestBody), ExampleData.form)

    actual.value.formData.fields should contain(newFormField)
  }

  private val newFormField = FormField(FormComponentId("surname"), "elchapo")
  private val requestBody = Json.toJson(newFormField)
}
