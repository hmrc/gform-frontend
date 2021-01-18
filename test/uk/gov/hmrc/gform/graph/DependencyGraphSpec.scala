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

package uk.gov.hmrc.gform.graph

import cats.data.NonEmptyList
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import org.scalatest.prop.TableFor2
import org.scalatest.{ FlatSpec, Matchers }
import scala.language.implicitConversions
import uk.gov.hmrc.gform.Helpers.{ toSmartString, toSmartStringExpression }
import uk.gov.hmrc.gform.eval.{ AllFormTemplateExpressions, ExprMetadata }
import uk.gov.hmrc.gform.models.{ Basic, DependencyGraphVerification, FormModel, FormModelSupport, Interim, SectionSelectorType, VariadicFormDataSupport }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.{ DestinationList, DestinationPrint }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, PrintSection }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr => _, _ }
import uk.gov.hmrc.gform.sharedmodel.graph.{ DependencyGraph, GraphNode }
import uk.gov.hmrc.gform.sharedmodel.graph.GraphNode._
import FormTemplateBuilder._

class DependencyGraphSpec extends FlatSpec with Matchers with FormModelSupport with VariadicFormDataSupport {

  implicit def simpleSyntax(s: String): FormComponentId = FormComponentId(s)

  private val emptyPage: Page[Basic] = Page(
    toSmartString(""),
    None,
    None,
    None,
    None,
    None,
    List.empty[FormComponent],
    None,
    None,
    None,
    None
  )
  private val emptyAddToList: Section.AddToList = Section.AddToList(
    toSmartString(""),
    toSmartString(""),
    toSmartString(""),
    toSmartString(""),
    None,
    None,
    NonEmptyList.one(emptyPage),
    mkFormComponent("choice", Value),
    None,
    None
  )

  private val emptyChoice = Choice(Checkbox, NonEmptyList.one(toSmartString("")), Vertical, List.empty, None)

  private val emptyInformationMessage = InformationMessage(StandardInfo, toSmartString(""))

  private val dummyFormTemplate = mkFormTemplate(mkSection(mkFormComponent("dummy", Value)))

  "Dependency Graph" should "handle component's value expression" in {
    val sections = List(
      mkSection(mkFormComponent("a", Value)),
      mkSection(mkFormComponent("b", FormCtx("a")))
    )

    val res = layers(sections)

    res shouldBe List(
      (0, Set(Simple("b"))),
      (1, Set(Expr(FormCtx("a")))),
      (2, Set(Simple("a")))
    )
  }

  it should "handle component's label containing expression" in {
    val sections = List(
      mkSection(mkFormComponent("a", Value)),
      mkSection(mkFormComponent("b", Value).copy(errorMessage = Some(toSmartStringExpression("", FormCtx("a")))))
    )

    val res = layers(sections)

    res shouldBe List(
      (0, Set(Simple("b"))),
      (1, Set(Expr(FormCtx("a")))),
      (2, Set(Simple("a")))
    )
  }

  it should "handle simple case of self reference" in {
    val sections = List(
      mkSection(mkFormComponent("a", Value)),
      mkSection(
        mkFormComponent("b", Value).copy(
          errorMessage = Some(toSmartStringExpression("", Else(FormCtx("a"), FormCtx("b"))))))
    )

    val res = layers(sections)

    res shouldBe List(
      (0, Set(Expr(FormCtx("b")))),
      (1, Set(Simple("b"))),
      (2, Set(Expr(FormCtx("a")))),
      (3, Set(Simple("a")))
    )
  }

  it should "support validIf" in {
    val sections = List(
      mkSection(mkFormComponent("a", Value)),
      mkSection(mkFormComponentValidIf("b", Value, ValidIf(Equals(FormCtx("a"), FormCtx("b")))))
    )

    val res = layers(sections)

    res shouldBe List(
      (0, Set(Expr(FormCtx("b")))),
      (1, Set(Simple("b"))),
      (2, Set(Expr(FormCtx("a")))),
      (3, Set(Simple("a")))
    )
  }

  it should "detect cyclic cross reference in section" in {

    val fcA = mkFormComponent("a", FormCtx("b"))
    val fcB = mkFormComponent("b", FormCtx("a"))

    val sections = List(
      mkSection(fcA :: fcB :: Nil)
    )

    val thrown = the[CycleDetectedException] thrownBy layers(sections)

    thrown.graphNode shouldBe GraphNode.Simple(FormComponentId("b"))

  }

  it should "detect cyclic cross reference in revealingChoice" in {

    val fcA = mkFormComponent("a", FormCtx("b"))
    val fcB = mkFormComponent("b", FormCtx("a"))

    val revealingChoice =
      RevealingChoice(
        List(
          RevealingChoiceElement(toSmartString("Yes"), fcA :: Nil, false),
          RevealingChoiceElement(toSmartString("No"), fcB :: Nil, false)
        ),
        true
      )

    val sections = List(
      mkSection(mkFormComponent("rc", revealingChoice))
    )

    val thrown = the[CycleDetectedException] thrownBy layers(sections)

    thrown.graphNode shouldBe GraphNode.Simple(FormComponentId("b"))

  }

  it should "detect cyclic cross reference in group" in {

    val fcA = mkFormComponent("a", FormCtx("b"))
    val fcB = mkFormComponent("b", FormCtx("a"))

    val group =
      Group(fcA :: fcB :: Nil, None, None, None, None)

    val sections = List(
      mkSection(mkFormComponent("group", group))
    )

    val thrown = the[CycleDetectedException] thrownBy layers(sections)

    thrown.graphNode shouldBe GraphNode.Simple(FormComponentId("b"))

  }

  val groupPropTable = {
    val stringExpr = toSmartStringExpression("", FormCtx("a"))
    Table(
      ("prop", "propSetter"),
      ("repeatLabel", (group: Group) => group.copy(repeatLabel = Some(stringExpr))),
      ("repeatAddAnotherText", (group: Group) => group.copy(repeatAddAnotherText = Some(stringExpr)))
    )
  }

  forAll(groupPropTable) {
    case (prop, propSetter) =>
      it should s"support group's $prop" in {

        val fcA = mkFormComponent("a", Value)

        val group = propSetter(Group(fcA :: Nil, None, None, None, None))

        val sections = List(
          mkSection(mkFormComponent("group", group))
        )

        val res = layers(sections)

        res shouldBe List(
          (0, Set(Simple("group"))),
          (1, Set(Expr(FormCtx("a")))),
          (2, Set(Simple("a")))
        )
      }
  }

  val choicePropTable = {
    val stringExpr = toSmartStringExpression("", FormCtx("a"))
    Table(
      ("prop", "propSetter"),
      ("options", (choice: Choice) => choice.copy(options = NonEmptyList.one(stringExpr))),
      ("optionHelpText", (choice: Choice) => choice.copy(optionHelpText = Some(NonEmptyList.one(stringExpr))))
    )
  }

  forAll(choicePropTable) {
    case (prop, propSetter) =>
      it should s"support choice's $prop" in {

        val choice = propSetter(emptyChoice)

        val sections = List(
          mkSection(mkFormComponent("choice", choice))
        )

        val res = layers(sections)

        res shouldBe List(
          (0, Set(Simple("choice"))),
          (1, Set(Expr(FormCtx("a")))),
          (2, Set(Simple("a")))
        )
      }
  }

  val infomationPropTable = {
    val stringExpr = toSmartStringExpression("", FormCtx("a"))
    Table(
      ("prop", "propSetter"),
      ("infoText", (information: InformationMessage) => information.copy(infoText = stringExpr))
    )
  }

  forAll(infomationPropTable) {
    case (prop, propSetter) =>
      it should s"support infomationMessage's $prop" in {

        val information = propSetter(emptyInformationMessage)

        val sections = List(
          mkSection(mkFormComponent("info", information))
        )

        val res = layers(sections)

        res shouldBe List(
          (0, Set(Simple("info"))),
          (1, Set(Expr(FormCtx("a")))),
          (2, Set(Simple("a")))
        )
      }
  }

  val propTable = {
    val stringWithElseExpr = toSmartStringExpression("", Else(FormCtx("a"), FormCtx("a")))
    Table(
      ("prop", "propSetter"),
      ("label", (fc: FormComponent) => fc.copy(label = stringWithElseExpr)),
      ("helpText", (fc: FormComponent) => fc.copy(helpText = Some(stringWithElseExpr))),
      ("shortName", (fc: FormComponent) => fc.copy(shortName = Some(stringWithElseExpr))),
      ("errorMessage", (fc: FormComponent) => fc.copy(errorMessage = Some(stringWithElseExpr))),
      (
        "validators.errorMessage",
        (fc: FormComponent) => fc.copy(validators = FormComponentValidator(ValidIf(IsTrue), stringWithElseExpr) :: Nil))
    )
  }

  forAll(propTable) {
    case (prop, propSetter) =>
      it should s"support expression in $prop" in {
        val sections = List(
          mkSection(mkFormComponent("a", Value)),
          mkSection(propSetter(mkFormComponent("b", Value)))
        )

        val res = layers(sections)

        res shouldBe List(
          (0, Set(Simple("b"))),
          (1, Set(Expr(FormCtx("a")))),
          (2, Set(Simple("a")))
        )
      }
  }

  val selfReferenceDissalowedPropTable = {
    val stringWithCycleElseExpr = toSmartStringExpression("", FormCtx("a"))
    Table(
      ("prop", "propSetter"),
      ("label", (fc: FormComponent) => fc.copy(label = stringWithCycleElseExpr)),
      ("helpText", (fc: FormComponent) => fc.copy(helpText = Some(stringWithCycleElseExpr))),
      ("shortName", (fc: FormComponent) => fc.copy(shortName = Some(stringWithCycleElseExpr)))
    )
  }

  val selfReferenceAllowedPropTable = {
    val selfReferenceBooleanExpr = Equals(FormCtx("a"), FormCtx("b"))
    val selfReferenceExpr = Else(FormCtx("a"), FormCtx("b"))
    val stringWithCycleElseExpr = toSmartStringExpression("", selfReferenceExpr)
    Table(
      ("prop", "propSetter"),
      ("errorMessage", (fc: FormComponent) => fc.copy(errorMessage = Some(stringWithCycleElseExpr))),
      (
        "validators.errorMessage",
        (fc: FormComponent) =>
          fc.copy(validators = FormComponentValidator(ValidIf(IsTrue), stringWithCycleElseExpr) :: Nil)),
      (
        "validators.validIf",
        (fc: FormComponent) =>
          fc.copy(validators = FormComponentValidator(ValidIf(selfReferenceBooleanExpr), toSmartString("")) :: Nil))
    )
  }

  forAll(selfReferenceDissalowedPropTable) {
    case (prop, propSetter) =>
      it should s"reject expression with self reference in $prop" in {
        val sections = List(
          mkSection(propSetter(mkFormComponent("a", Value)))
        )

        val thrown = the[CycleDetectedException] thrownBy layers(sections)

        thrown.graphNode shouldBe GraphNode.Expr(FormCtx("a"))
      }
  }

  forAll(selfReferenceAllowedPropTable) {
    case (prop, propSetter) =>
      it should s"support expression with self reference in $prop" in {
        val sections = List(
          mkSection(mkFormComponent("a", Value)),
          mkSection(propSetter(mkFormComponent("b", Value)))
        )

        val res = layers(sections)

        res shouldBe List(
          (0, Set(Expr(FormCtx("b")))),
          (1, Set(Simple("b"))),
          (2, Set(Expr(FormCtx("a")))),
          (3, Set(Simple("a")))
        )
      }
  }

  it should "support RevealingChoiceElement choice" in {

    val fcA = mkFormComponent("a", Value)
    val fcB = mkFormComponent("b", Value)

    val choice = toSmartStringExpression("", Else(FormCtx("a"), FormCtx("b")))

    val revealingChoice =
      RevealingChoice(
        List(
          RevealingChoiceElement(choice, fcA :: fcB :: Nil, false)
        ),
        true
      )

    val sections = List(
      mkSection(mkFormComponent("rc", revealingChoice))
    )

    val res = layers(sections)

    res shouldBe List(
      (0, Set(Simple("rc"))),
      (1, Set(Expr(FormCtx("a")), Expr(FormCtx("b")))),
      (2, Set(Simple("a"), Simple("b")))
    )

  }

  forAll(selfReferenceDissalowedPropTable) {
    case (prop, propSetter) =>
      it should s"reject expression with self reference in $prop of revealingChoice component" in {

        val fcA = propSetter(mkFormComponent("a", Value))

        val revealingChoice =
          RevealingChoice(
            List(
              RevealingChoiceElement(toSmartString(""), fcA :: Nil, false)
            ),
            true
          )

        val sections = List(
          mkSection(mkFormComponent("rc", revealingChoice))
        )

        val thrown = the[CycleDetectedException] thrownBy layers(sections)

        thrown.graphNode shouldBe GraphNode.Expr(FormCtx("a"))
      }
  }

  forAll(selfReferenceAllowedPropTable) {
    case (prop, propSetter) =>
      it should s"support expression with self reference in $prop of revealingChoice component" in {

        val fcA = mkFormComponent("a", Value)
        val fcB = propSetter(mkFormComponent("b", Value))

        val revealingChoice =
          RevealingChoice(
            List(
              RevealingChoiceElement(toSmartString(""), fcA :: fcB :: Nil, false)
            ),
            true
          )

        val sections = List(
          mkSection(mkFormComponent("rc", revealingChoice))
        )

        val res = layers(sections)

        res shouldBe List(
          (0, Set(Expr(FormCtx("b")))),
          (1, Set(Simple("b"))),
          (2, Set(Expr(FormCtx("a")))),
          (3, Set(Simple("a")))
        )
      }
  }

  val nonRepeatedSectionValidators: TableFor2[String, Page[Basic]] = {
    val ctx = FormCtx("a")
    val stringExpr = toSmartStringExpression("", ctx)
    val hmrcRosmRegistrationCheckValidator = HmrcRosmRegistrationCheckValidator(
      toSmartString(""),
      "",
      FormCtx(""),
      FormCtx("")
    )
    val bankAccountModulusCheck = BankAccountModulusCheck(
      toSmartString(""),
      FormCtx(""),
      FormCtx("")
    )

    def hmrcRosm(rosm: HmrcRosmRegistrationCheckValidator): Page[Basic] =
      emptyPage.copy(validators = Some(rosm))

    def bankAccount(bank: BankAccountModulusCheck): Page[Basic] =
      emptyPage.copy(validators = Some(bank))

    Table(
      // format: off
      ("prop", "propSetter"),
      ("validators.HmrcRosmRegistrationCheckValidator.errorMessage", hmrcRosm(hmrcRosmRegistrationCheckValidator.copy(errorMessage = stringExpr))),
      ("validators.HmrcRosmRegistrationCheckValidator.utr",          hmrcRosm(hmrcRosmRegistrationCheckValidator.copy(utr = ctx))),
      ("validators.HmrcRosmRegistrationCheckValidator.postcode",     hmrcRosm(hmrcRosmRegistrationCheckValidator.copy(postcode = ctx))),
      ("validators.BankAccountModulusCheck.errorMessage",  bankAccount(bankAccountModulusCheck.copy(errorMessage = stringExpr))),
      ("validators.BankAccountModulusCheck.accountNumber", bankAccount(bankAccountModulusCheck.copy(accountNumber = ctx))),
      ("validators.BankAccountModulusCheck.sortCode",      bankAccount(bankAccountModulusCheck.copy(accountNumber = ctx)))
      // format: on
    )
  }

  val nonRepeatedSectionExpressionTable: TableFor2[String, Page[Basic]] = {
    val stringExpr = toSmartStringExpression("", FormCtx("a"))
    Table(
      // format: off
      ("prop", "page"),
      ("title",             emptyPage.copy(title = stringExpr)),
      ("description",       emptyPage.copy(description = Some(stringExpr))),
      ("shortName",         emptyPage.copy(shortName = Some(stringExpr))),
      ("progressIndicator", emptyPage.copy(progressIndicator = Some(stringExpr))),
      ("continueLabel",     emptyPage.copy(continueLabel = Some(stringExpr)))
      // format: on
    )
  }

  forAll(nonRepeatedSectionExpressionTable) {
    case (prop, page) =>
      it should s"support NonRepeated section with expression in $prop" in {

        val section: Section.NonRepeatingPage = Section.NonRepeatingPage(page)

        val sections = List(
          section
        )

        val res = layers(sections)

        res shouldBe List(
          (0, Set(Expr(FormCtx("a")))),
          (1, Set(Simple("a")))
        )
      }
  }

  forAll(nonRepeatedSectionValidators) {
    case (prop, page) =>
      it should s"support NonRepeated section with expression in $prop" in {

        val section: Section.NonRepeatingPage = Section.NonRepeatingPage(page)

        val sections = List(
          section
        )

        val res = layers(sections)

        res shouldBe List(
          (0, Set(Expr(FormCtx("a")), Expr(FormCtx("")))),
          (1, Set(Simple("a"), Simple("")))
        )
      }
  }

  val addToListSectionExpressionTable = {
    val stringExpr = toSmartStringExpression("", FormCtx("a"))
    val expect1 = List(
      (0, Set(Expr(FormCtx("a")))),
      (1, Set(Simple("a")))
    )
    val expect2 = List(
      (0, Set(Simple("1_choice"))),
      (1, Set(Expr(FormCtx("a")))),
      (2, Set(Simple("a")))
    )
    Table(
      // format: off
      ("prop", "expected", "addToList"),
      ("title",                             expect1, emptyAddToList.copy(title = stringExpr)),
      ("description",                       expect1, emptyAddToList.copy(description = stringExpr)),
      ("shortName",                         expect1, emptyAddToList.copy(shortName = stringExpr)),
      ("addAnotherQuestion.options",        expect2, emptyAddToList.copy(addAnotherQuestion = mkFormComponent("choice", emptyChoice.copy(options = NonEmptyList.one(stringExpr))))),
      ("addAnotherQuestion.optionHelpText", expect2, emptyAddToList.copy(addAnotherQuestion = mkFormComponent("choice", emptyChoice.copy(optionHelpText = Some(NonEmptyList.one(stringExpr))))))
      // format: on
    )
  }

  forAll(addToListSectionExpressionTable) {
    case (prop, expected, addToList) =>
      it should s"support AddToList section with expression in $prop" in {

        val sections = List(addToList)

        val res = layers(sections)

        res shouldBe expected
      }
  }

  it should s"support Repeated section with expression in repeats" in {

    val repeating = Section.RepeatingPage(emptyPage, FormCtx("a"))

    val sections = List(
      repeating
    )

    val res = layers(sections)

    res shouldBe List(
      (0, Set(Expr(FormCtx("a")))),
      (1, Set(Simple("a")))
    )
  }

  it should "produce graph" in {

    val fcA = mkFormComponent("a", Value)
    val fcB = mkFormComponent("b", Value)
    val fcC = mkFormComponent("c", Value)
    val fcD = mkFormComponent("d", Value)
    val fcE = mkFormComponent("e", Value)
    val fcF = mkFormComponent("f", Else(FormCtx("c"), FormCtx("d")))
    val fcG =
      mkFormComponent("g", Value).copy(label = toSmartStringExpression("", Else(FormCtx("a"), FormCtx("b"))))

    val includeIf1 =
      IncludeIf(Equals(FormCtx("a"), FormCtx("b")))
    val includeIf2 =
      IncludeIf(Equals(FormCtx("c"), Constant("0")))

    val section1 = mkSection(fcA)
    val section2 = mkSection(fcB)
    val section3 = mkSectionIncludeIf(fcC :: Nil, includeIf1)
    val section4 = mkSectionIncludeIf(fcD :: fcE :: Nil, includeIf2)
    val section5 = mkSection(fcF)
    val section6 = mkSection(fcG)

    val sections = List(
      section1,
      section2,
      section3,
      section4,
      section5,
      section6
    )

    val res = layers(sections)

    res shouldBe List(
      (0, Set(Simple("e"), Simple("f"), Simple("g"))),
      (1, Set(Expr(FormCtx("d")))),
      (2, Set(Simple("d"))),
      (3, Set(Expr(Constant("0")), Expr(FormCtx("c")))),
      (4, Set(Simple("c"))),
      (5, Set(Expr(FormCtx("a")), Expr(FormCtx("b")))),
      (6, Set(Simple("a"), Simple("b")))
    )
  }

  val formTemplateExpressionTable = {
    val emptySS = toSmartString("")
    val ctx = FormCtx("a")
    val stringExpr = toSmartStringExpression("", ctx)
    val emptySummarySection = SummarySection(emptySS, emptySS, emptySS)
    val emptyPrintSectionPage = PrintSection.Page(emptySS, emptySS)
    val emptyPrintSectionPdf = PrintSection.Pdf(emptySS, emptySS)
    val emptyPrintSectionPdfNotification = PrintSection.PdfNotification(emptySS, emptySS, List.empty[FormComponentId])
    val emptyDestinationPrint = DestinationPrint(emptyPrintSectionPage, emptyPrintSectionPdf, None)
    val emptyDeclarationSection = DeclarationSection(emptySS, None, None, List.empty[FormComponent])
    val emptyAcknowledgementSection =
      AcknowledgementSection(emptySS, None, None, List.empty[FormComponent], true, None, None, true)
    val emptyHmrcDms =
      Destination.HmrcDms(DestinationId(""), "", Value, "", "", "", false, false, false, Some(false), false)
    val emptyCompositeDestination = Destination.Composite(DestinationId(""), "", NonEmptyList.one(emptyHmrcDms))
    val emptyDestinationList =
      DestinationList(NonEmptyList.one(emptyHmrcDms), emptyAcknowledgementSection, emptyDeclarationSection)
    val field = mkFormComponent("b", ctx)
    Table(
      // format: off
      ("prop", "page"),
      ("emailParameters",                     dummyFormTemplate.copy(emailParameters = Some(NonEmptyList.of(EmailParameter("", ctx))))),
      ("summarySection.title",                dummyFormTemplate.copy(summarySection = emptySummarySection.copy(title = stringExpr))),
      ("summarySection.header",               dummyFormTemplate.copy(summarySection = emptySummarySection.copy(header = stringExpr))),
      ("summarySection.footer",               dummyFormTemplate.copy(summarySection = emptySummarySection.copy(footer = stringExpr))),
      ("printSection.page.title",             dummyFormTemplate.copy(destinations = emptyDestinationPrint.copy(page = emptyPrintSectionPage.copy(title = stringExpr)))),
      ("printSection.page.instructions",      dummyFormTemplate.copy(destinations = emptyDestinationPrint.copy(page = emptyPrintSectionPage.copy(instructions = stringExpr)))),
      ("printSection.pdf.header",             dummyFormTemplate.copy(destinations = emptyDestinationPrint.copy(pdf = emptyPrintSectionPdf.copy(header = stringExpr)))),
      ("printSection.pdf.footer",             dummyFormTemplate.copy(destinations = emptyDestinationPrint.copy(pdf = emptyPrintSectionPdf.copy(footer = stringExpr)))),
      ("printSection.pdfNotification.header", dummyFormTemplate.copy(destinations = emptyDestinationPrint.copy(pdfNotification = Some(emptyPrintSectionPdfNotification.copy(header = stringExpr))))),
      ("printSection.pdfNotification.footer", dummyFormTemplate.copy(destinations = emptyDestinationPrint.copy(pdfNotification = Some(emptyPrintSectionPdfNotification.copy(footer = stringExpr))))),
      ("acknowledgementSection.title",        dummyFormTemplate.copy(destinations = emptyDestinationList.copy(acknowledgementSection = emptyAcknowledgementSection.copy(title = stringExpr)))),
      ("acknowledgementSection.description",  dummyFormTemplate.copy(destinations = emptyDestinationList.copy(acknowledgementSection = emptyAcknowledgementSection.copy(description = Some(stringExpr))))),
      ("acknowledgementSection.shortName",    dummyFormTemplate.copy(destinations = emptyDestinationList.copy(acknowledgementSection = emptyAcknowledgementSection.copy(shortName = Some(stringExpr))))),
      ("acknowledgementSection.fields",       dummyFormTemplate.copy(destinations = emptyDestinationList.copy(acknowledgementSection = emptyAcknowledgementSection.copy(fields = field :: Nil)))),
      ("declarationSection.title",            dummyFormTemplate.copy(destinations = emptyDestinationList.copy(declarationSection = emptyDeclarationSection.copy(title = stringExpr)))),
      ("declarationSection.description",      dummyFormTemplate.copy(destinations = emptyDestinationList.copy(declarationSection = emptyDeclarationSection.copy(description = Some(stringExpr))))),
      ("declarationSection.shortName",        dummyFormTemplate.copy(destinations = emptyDestinationList.copy(declarationSection = emptyDeclarationSection.copy(shortName = Some(stringExpr))))),
      ("declarationSection.fields",           dummyFormTemplate.copy(destinations = emptyDestinationList.copy(declarationSection = emptyDeclarationSection.copy(fields = field :: Nil)))),
      ("destination.hmrcDms.customerId",      dummyFormTemplate.copy(destinations = emptyDestinationList.copy(destinations = NonEmptyList.one(emptyHmrcDms.copy(customerId = ctx))))),
      ("destination.composite.destinations",  dummyFormTemplate.copy(destinations = emptyDestinationList.copy(destinations = NonEmptyList.one(emptyCompositeDestination.copy(destinations = NonEmptyList.one(emptyHmrcDms.copy(customerId = ctx)))))))
      // format: on
    )
  }

  forAll(formTemplateExpressionTable) {
    case (prop, formTemplate) =>
      it should s"support expression in formTemplate $prop property" in {

        val res = layers(formTemplate)

        res shouldBe List(
          (0, Set(Expr(FormCtx("a")))),
          (1, Set(Simple("a")))
        )
      }
  }

  private def layers(sections: List[Section]): List[(Int, Set[GraphNode])] =
    layers(mkFormTemplate(sections))

  private def layers(formTemplate: FormTemplate): List[(Int, Set[GraphNode])] = {
    val fmb = mkFormModelBuilder(formTemplate)

    val fm: FormModel[DependencyGraphVerification] = fmb.dependencyGraphValidation[SectionSelectorType.Normal]

    val formTemplateExprs: List[ExprMetadata] = AllFormTemplateExpressions(formTemplate)

    DependencyGraph.constructDependencyGraph(
      DependencyGraph.toGraph(fm.asInstanceOf[FormModel[Interim]], formTemplateExprs)) match {

      case Left(node) => throw new CycleDetectedException(node.toOuter)
      case Right(topOrder) =>
        topOrder.toList.map { case (index, items) => (index, items.toSet) }
    }
  }
}

private class CycleDetectedException(val graphNode: GraphNode) extends Exception
