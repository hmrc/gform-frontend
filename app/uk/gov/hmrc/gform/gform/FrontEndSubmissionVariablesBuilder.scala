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

package uk.gov.hmrc.gform.gform
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.graph.processor.IdentifierExtractor
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.FrontEndSubmissionVariables
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object FrontEndSubmissionVariablesBuilder extends IdentifierExtractor {

  def apply[D <: DataOrigin](
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    customerId: CustomerId
  ): FrontEndSubmissionVariables = {

    val identifierValue = formModelVisibilityOptics.formModel.allFormComponents
      .collectFirst { case HasExpr(UserCtx(UserField.EnrolledIdentifier)) =>
        processContext(retrievals, formTemplate.authConfig)
      }
      .getOrElse("")

    FrontEndSubmissionVariables(jsonBuilder(identifierValue, customerId))
  }

  def processContext(retrievals: MaterialisedRetrievals, authConfig: AuthConfig) =
    authConfig match {
      case HmrcEnrolmentModule(auth)             => enrolmentIdentifierValue(retrievals, auth)
      case HmrcAgentWithEnrolmentModule(_, auth) => enrolmentIdentifierValue(retrievals, auth)
      case _                                     => ""
    }

  private val jsonBuilder: (String, CustomerId) => JsValue = (identifier, customerId) =>
    Json.parse(s"""{ "user" :{ "enrolledIdentifier": "$identifier", "customerId": "${customerId.id}" } }""")
}
