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
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.Helpers.toLocalisedString

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

  def mkSection(formComponents: List[FormComponent]) =
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

  def mkSectionIncludeIf(formComponents: List[FormComponent], includeIf: IncludeIf) =
    Section(
      toLocalisedString("Section Name"),
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

  val ls = toLocalisedString("Label")

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
    mkFormComponent(fcId, Text(AnyText, expr))

  def mkFormComponentEditable(fcId: String, expr: Expr): FormComponent =
    mkFormComponentEditable(fcId, Text(AnyText, expr))

  def mkFormTemplate(sections: List[Section]) = FormTemplate.withDeprecatedDmsSubmission(
    FormTemplateId("tst1"),
    toLocalisedString("Dependecy heavy experiment"),
    toLocalisedString(""),
    Some(BetaBanner),
    Default,
    None,
    None,
    DmsSubmission("R&D", TextExpression(FormCtx("utrRepComp")), "CCG-CT-RandDreports", "CCG", None),
    HmrcAgentModule(AllowAnyAgentAffinityUser),
    "randd_confirmation_submission",
    Some(NonEmptyList
      .of(EmailParameter("fullNameVariable", FormCtx("fullName")), EmailParameter("emailVariable", FormCtx("email")))),
    "http://www.google.co.uk",
    "http://www.yahoo.co.uk",
    None,
    sections,
    AcknowledgementSection(
      toLocalisedString("Acknowledgement Page"),
      Some(toLocalisedString("this page is to acknowledge submission")),
      Some(toLocalisedString("shortName for acknowledgement")),
      List.empty[FormComponent]
    ),
    DeclarationSection(toLocalisedString("Declaration"), None, None, Nil)
  )

}
