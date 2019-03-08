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

package uk.gov.hmrc.gform.controllers.helpers
import cats.MonadError
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.{ Data, Recalculation }
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.frontend.controller.FrontendController
import scala.concurrent.Future

case class EmailParameterRecalculationHelper(cache: AuthCacheWithForm) extends FrontendController {

  def recalculateEmailParameters(
    data: Data,
    recalculation: Recalculation[Future, Throwable],
    gformConnector: GformConnector,
    maybeAccessCode: Option[AccessCode]
  )(implicit hc: HeaderCarrier, me: MonadError[Future, Throwable]): Future[EmailParameters] =
    recalculation
      .recalculateFormData(
        cache.form.formData.toData,
        formTemplateWithParametersAsComponents,
        cache.retrievals,
        cache.form.thirdPartyData,
        cache.form.envelopeId)
      .map(mapToParameterTemplateVariables)

  private def mkFormComponent(fcId: String, ct: ComponentType) =
    FormComponent(
      FormComponentId(fcId + "UniqueEmailParameter"),
      ct,
      "UniqueEmailParameter",
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
      "Section Name",
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
      mkFormComponent(parameter.emailTemplateVariable, Text(AnyText, parameter.value.expr))))

    val newSections = cache.formTemplate.sections ::: List(mkSection(newFormComponents))

    val newFormTemplate = cache.formTemplate.copy(sections = newSections)

    newFormTemplate
  }

  private def mapToParameterTemplateVariables(formDataRecalculated: FormDataRecalculated): EmailParameters =
    EmailParameters(cache.formTemplate.emailParameters.fold(Map.empty[String, String])(parameters =>
      parameterFormat(parameters.toList, formDataRecalculated)))

  private def parameterToTuple(
    parameter: EmailParameter,
    formDataRecalculated: FormDataRecalculated): (String, Option[Seq[String]]) =
    (
      parameter.emailTemplateVariable,
      formDataRecalculated.data.get(FormComponentId(parameter.emailTemplateVariable + "UniqueEmailParameter")))

  private def parameterFormat(
    parameters: List[EmailParameter],
    formDataRecalculated: FormDataRecalculated): Map[String, String] =
    parameters
      .map(parameter => parameterToTuple(parameter, formDataRecalculated))
      .map(parameter => (parameter._1, parameter._2.getOrElse(Seq(""))))
      .map(parameter => (parameter._1, parameter._2.head))
      .toMap

}
