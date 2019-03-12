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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.graph.processor.IdentifierExtractor
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object VariablesBuilder extends IdentifierExtractor {

  def apply(
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    customerId: CustomerId,
    emailParameters: EmailParametersRecalculated
  ): Variables = {

    val identifierValue = formTemplate.sections
      .flatMap(_.fields)
      .collectFirst {
        case HasExpr(SingleExpr(UserCtx(EnrolledIdentifier))) =>
          processContext(retrievals, formTemplate.authConfig)
      }
      .getOrElse("")

    val emailParameterJsonArray = "{" + emailParameters.emailParametersMap
      .map(parameter => s""" "${parameter._1.emailTemplateVariableId}": "${parameter._2.value}" """)
      .mkString(", ") + "}"

    Variables(jsonBuilder(identifierValue, customerId, emailParameterJsonArray))
  }

  def processContext(retrievals: MaterialisedRetrievals, authConfig: AuthConfig) =
    authConfig match {
      case HmrcEnrolmentModule(auth)             => enrolmentIdentifierValue(retrievals, auth)
      case HmrcAgentWithEnrolmentModule(_, auth) => enrolmentIdentifierValue(retrievals, auth)
      case _                                     => ""
    }

//  private val jsonBuilder: (String, CustomerId) => JsValue = (identifier, customerId) =>
//    Json.parse(s"""{ "user" :{ "enrolledIdentifier": "$identifier", "customerId": "${customerId.id}" } }""")

  private val jsonBuilder: (String, CustomerId, String) => JsValue = (value, customerId, emailParameters) =>
    Json.parse(s"""{ "user" :{ "enrolledIdentifier": "$value",
               "customerId": "${customerId.id}"},
                  | "emailParameters": $emailParameters  }""".stripMargin)
}
