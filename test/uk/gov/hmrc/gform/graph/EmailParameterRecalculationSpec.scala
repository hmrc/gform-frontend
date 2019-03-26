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

package uk.gov.hmrc.gform.graph
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthCacheWithoutForm }
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.ExampleData

import scala.concurrent.ExecutionContext

class EmailParameterRecalculationSpec(implicit ec: ExecutionContext) extends FlatSpec with Matchers {

  val cache: AuthCacheWithForm = AuthCacheWithoutForm(ExampleData.materialisedRetrievals, ExampleData.formTemplate)
    .toAuthCacheWithForm(ExampleData.form)

  val emailParameterRecalculation: EmailParameterRecalculation = EmailParameterRecalculation(cache)

  "parameter format with list of email parameters" should "convert it to a Map[EmailTemplateVariable, EmailParameterValue]" in {

    val emailParameters = List(EmailParameter("templateVarId", FormCtx("${fieldId}")))

    ExampleData.formDataRecalculated

    val data: Data = Map(FormComponentId("templateVarIdUniqueEmailParameter") -> Seq("value"))

    emailParameterRecalculation
      .parameterFormat(emailParameters, FormDataRecalculated.empty.copy(recData = RecData.fromData(data))) shouldBe Map(
      EmailTemplateVariable("templateVarId") -> EmailParameterValue("value"))
  }

}
