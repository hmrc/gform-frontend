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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.Helpers.{ toLocalisedString, toSmartString }

object FormTemplateBuilder {

  def mkGroup(max: Int, formComponents: List[FormComponent]): Group =
    Group(
      formComponents,
      Vertical,
      Some(max),
      None,
      None,
      None
    )

  def mkSection(formComponents: FormComponent*): Section = mkSection(formComponents.toList)
  def mkSection(formComponents: List[FormComponent]) =
    Section(
      toSmartString("Section Name"),
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

  def mkSectionIncludeIf(formComponents: List[FormComponent], includeIf: IncludeIf) =
    Section(
      toSmartString("Section Name"),
      None,
      None,
      None,
      Some(includeIf),
      None,
      None,
      None,
      formComponents,
      None,
      None
    )

  val ls = toSmartString("Label")

  def mkFormComponent(fcId: String, ct: ComponentType) =
    FormComponent(
      FormComponentId(fcId),
      ct,
      ls,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None
    )

  def mkFormComponentEditable(fcId: String, ct: ComponentType) =
    FormComponent(
      FormComponentId(fcId),
      ct,
      ls,
      None,
      None,
      None,
      true,
      true,
      true,
      false,
      false,
      None,
      None
    )

  def mkFormComponent(fcId: String, expr: Expr): FormComponent =
    mkFormComponent(fcId, Text(BasicText, expr))

  def mkFormComponentEditable(fcId: String, expr: Expr): FormComponent =
    mkFormComponentEditable(fcId, Text(BasicText, expr))

  def mkFormTemplate(sections: Section*): FormTemplate = mkFormTemplate(sections.toList)
  def mkFormTemplate(sections: List[Section]) = FormTemplate.withDeprecatedDmsSubmission(
    FormTemplateId("tst1"),
    toLocalisedString("Dependecy heavy experiment"),
    Some(BetaBanner),
    Default,
    OnePerUser(ContinueOrDeletePage.Show),
    DmsSubmission("R&D", TextExpression(FormCtx("utrRepComp")), "CCG-CT-RandDreports", "CCG", None),
    HmrcAgentModule(AllowAnyAgentAffinityUser),
    "randd_confirmation_submission",
    Some(NonEmptyList
      .of(EmailParameter("fullNameVariable", FormCtx("fullName")), EmailParameter("emailVariable", FormCtx("email")))),
    None,
    sections,
    AcknowledgementSection(
      toSmartString("Acknowledgement Page"),
      Some(toSmartString("this page is to acknowledge submission")),
      Some(toSmartString("shortName for acknowledgement")),
      List.empty[FormComponent]
    ),
    DeclarationSection(toSmartString("Declaration"), None, None, Nil),
    None
  )

}
