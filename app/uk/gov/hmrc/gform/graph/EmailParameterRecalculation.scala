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
import cats.MonadError
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

case class EmailParameterRecalculation(cache: AuthCacheWithForm)(implicit ex: ExecutionContext) {

  def recalculateEmailParameters(
    recalculation: Recalculation[Future, Throwable]
  )(implicit hc: HeaderCarrier, me: MonadError[Future, Throwable]): Future[EmailParametersRecalculated] =
    recalculation
      .recalculateFormData(
        cache.variadicFormData,
        formTemplateWithParametersAsComponents,
        cache.retrievals,
        cache.form.thirdPartyData,
        cache.form.envelopeId)
      .map(mapToParameterTemplateVariables)

  private def toLocalisedString(string: String) = LocalisedString(Map(LangADT.En -> string))

  private def mkFormComponent(fcId: String, ct: ComponentType) =
    FormComponent(
      FormComponentId(fcId + "UniqueEmailParameter"),
      ct,
      toLocalisedString("UniqueEmailParameter"),
      None,
      None,
      None,
      mandatory = true,
      editable = false,
      submissible = true,
      derived = false,
      onlyShowOnSummary = false,
      None,
      None
    )

  private def mkSection(formComponents: List[FormComponent]): Section =
    Section(
      toLocalisedString("Section Name"),
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      formComponents,
      None,
      None
    )

  private def formTemplateWithParametersAsComponents: FormTemplate = {

    val newFormComponents = cache.formTemplate.emailParameters.fold(List.empty[FormComponent])(_.toList.map(parameter =>
      mkFormComponent(parameter.emailTemplateVariable, Text(AnyText, parameter.value))))

    val newSections = cache.formTemplate.sections ::: List(mkSection(newFormComponents))

    val newFormTemplate = cache.formTemplate.copy(sections = newSections)

    newFormTemplate
  }

  private def mapToParameterTemplateVariables(formDataRecalculated: FormDataRecalculated): EmailParametersRecalculated =
    EmailParametersRecalculated(
      cache.formTemplate.emailParameters.fold(Map.empty[EmailTemplateVariable, EmailParameterValue])(parameters =>
        parameterFormat(parameters.toList, formDataRecalculated)))

  private def parameterToTuple(
    parameter: EmailParameter,
    formDataRecalculated: FormDataRecalculated): (String, Option[String]) =
    (
      parameter.emailTemplateVariable,
      formDataRecalculated.data
        .one(FormComponentId(parameter.emailTemplateVariable + "UniqueEmailParameter"))) //stops issues when template variable id is same as field id.

  def parameterFormat(
    parameters: List[EmailParameter],
    formDataRecalculated: FormDataRecalculated): Map[EmailTemplateVariable, EmailParameterValue] =
    parameters
      .map(parameter => parameterToTuple(parameter, formDataRecalculated))
      .map {
        case (variableId, parameterValue) =>
          (EmailTemplateVariable(variableId), EmailParameterValue(parameterValue.getOrElse("")))
      }
      .toMap

}
