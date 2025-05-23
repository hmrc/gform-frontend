/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.eval

import org.scalatest.prop.{ TableDrivenPropertyChecks, TableFor5 }
import play.api.libs.json.Json
import play.api.test.Helpers
import uk.gov.hmrc.auth.core.{ Assistant, Enrolment, EnrolmentIdentifier, Enrolments, User }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.eval.ExpressionResult.{ AddressResult, DateResult, Empty, ListResult, NumberResult, OptionResult, PeriodResult, StringResult }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.lookup.ShowAll.Enabled
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.{ DataRetrieveAll, FormModel }
import uk.gov.hmrc.gform.models.ExpandUtils.toModelComponentId
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId, ModelPageId }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ QueryParamValue, QueryParams, TaskIdTaskStatusMapping, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.{ NewForm, NewFormForTemplate, NewSession, PageLink }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OffsetUnit.{ Day, Month, Year }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate

class EvaluationResultsSpec extends Spec with TableDrivenPropertyChecks {

  private val booleanExprResolver = BooleanExprResolver(_ => false)
  private val recDataEmpty = RecData[OutOfDate](
    VariadicFormData.empty
  )

  def buildEvaluationContext(
    pageIdSectionNumberMap: Map[ModelPageId, SectionNumber] = Map.empty,
    indexedComponentIds: List[ModelComponentId] = List.empty,
    retrievals: MaterialisedRetrievals = authContext,
    thirdPartyData: ThirdPartyData = ThirdPartyData.empty
  ) =
    EvaluationContext(
      formTemplateId,
      submissionRef,
      None,
      retrievals,
      thirdPartyData.copy(queryParams =
        QueryParams(
          Map(
            QueryParam("availAmt")    -> QueryParamValue("123"),
            QueryParam("availAmtStr") -> QueryParamValue("foo")
          )
        )
      ),
      authConfig,
      HeaderCarrier(),
      Option.empty[FormPhase],
      FileIdsWithMapping.empty,
      Map.empty,
      Map.empty,
      Set.empty,
      Set.empty,
      Set.empty,
      pageIdSectionNumberMap,
      LangADT.En,
      Helpers.stubMessages(
        Helpers.stubMessagesApi(
          Map(
            "en" -> Map(
              "date.January" -> "January"
            )
          )
        )
      ),
      FormModel.modelComponentsToIndexedComponentMap(indexedComponentIds),
      Set.empty,
      FileSizeLimit(1),
      DataRetrieveAll.empty,
      Set.empty[ModelComponentId],
      Map.empty,
      Set.empty,
      new LookupRegistry(Map.empty),
      Map.empty,
      Map.empty,
      TaskIdTaskStatusMapping.empty
    )

  override val evaluationContext: EvaluationContext = buildEvaluationContext()

  "evalExpr - type dateString" should "evaluate expressions" in {

    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        (toModelComponentId("dateFieldId1-year"), VariadicValue.One("1970")),
        (toModelComponentId("dateFieldId1-month"), VariadicValue.One("1")),
        (toModelComponentId("dateFieldId1-day"), VariadicValue.One("11")),
        (toModelComponentId("dateFieldId2-year"), VariadicValue.One("1971")),
        (toModelComponentId("dateFieldId2-month"), VariadicValue.One("1")),
        (toModelComponentId("dateFieldId2-day"), VariadicValue.One("11")),
        (toModelComponentId("textFieldEmpty"), VariadicValue.One("")),
        (toModelComponentId("textField"), VariadicValue.One("textFieldValue"))
      )
    )

    val table = Table(
      ("typeInfo", "recData", "expectedResult"),
      (
        TypeInfo(
          DateCtx(DateFormCtxVar(FormCtx(FormComponentId("dateFieldId1")))),
          StaticTypeData(ExprType.dateString, None)
        ),
        recData,
        DateResult(LocalDate.of(1970, 1, 11))
      ),
      (
        TypeInfo(FormCtx(FormComponentId("dateFieldId1")), StaticTypeData(ExprType.dateString, None)),
        recData,
        DateResult(LocalDate.of(1970, 1, 11))
      ),
      (
        TypeInfo(
          Else(FormCtx(FormComponentId("dateFieldId1")), FormCtx(FormComponentId("dateFieldId2"))),
          StaticTypeData(ExprType.dateString, None)
        ),
        recData,
        DateResult(LocalDate.of(1970, 1, 11))
      ),
      (
        TypeInfo(
          Else(FormCtx(FormComponentId("textFieldEmpty")), FormCtx(FormComponentId("dateFieldId2"))),
          StaticTypeData(ExprType.dateString, None)
        ),
        recData,
        DateResult(LocalDate.of(1971, 1, 11))
      ),
      (
        TypeInfo(
          Else(FormCtx(FormComponentId("textFieldEmpty")), Constant("someConstant")),
          StaticTypeData(ExprType.dateString, None)
        ),
        recData,
        StringResult("someConstant")
      )
    )

    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult) =>
      EvaluationResults.empty
        .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext) shouldBe expectedResult
    }
  }

  "evalExpr - type string" should "evaluate expressions" in {

    val table: TableFor5[TypeInfo, RecData[OutOfDate], EvaluationContext, ExpressionResult, String] = Table(
      ("typeInfo", "recData", "evaluationContext", "expectedResult", "scenario"),
      (
        TypeInfo(
          DateCtx(DateFormCtxVar(FormCtx(FormComponentId("dateFieldId1")))),
          StaticTypeData(ExprType.string, None)
        ),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("dateFieldId1-year"), VariadicValue.One("1970")),
            (toModelComponentId("dateFieldId1-month"), VariadicValue.One("1")),
            (toModelComponentId("dateFieldId1-day"), VariadicValue.One("11")),
            (toModelComponentId("dateFieldId1"), VariadicValue.One("11 January 1970"))
          )
        ),
        evaluationContext,
        DateResult(LocalDate.of(1970, 1, 11)),
        "DateCtx expression converted to type 'string'"
      ),
      (
        TypeInfo(FormCtx(FormComponentId("dateFieldId1")), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("dateFieldId1-year"), VariadicValue.One("1970")),
            (toModelComponentId("dateFieldId1-month"), VariadicValue.One("1")),
            (toModelComponentId("dateFieldId1-day"), VariadicValue.One("11")),
            (toModelComponentId("dateFieldId1"), VariadicValue.One("11 January 1970"))
          )
        ),
        evaluationContext,
        StringResult("11 January 1970"),
        "FormCtx Expression converted to type 'string'"
      ),
      (
        TypeInfo(Count(FormComponentId("addToListQuestion")), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("1_addToListQuestion"), VariadicValue.One("0")),
            (toModelComponentId("2_addToListQuestion"), VariadicValue.One("1")),
            (toModelComponentId("1_addToListField1"), VariadicValue.One("Hello")),
            (toModelComponentId("2_addToListField1"), VariadicValue.One("World"))
          )
        ),
        evaluationContext,
        StringResult("2"),
        "Eval Count(addToListComponent) as string"
      ),
      (
        TypeInfo(LangCtx, StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        evaluationContext,
        StringResult("en"),
        "Eval LangCtx as string"
      ),
      (
        TypeInfo(LinkCtx(PageLink(PageId("unknown"))), StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        evaluationContext,
        Empty,
        "Eval LinkCtx(PageLink(PageId(xxx))) as string (non-existent)"
      ),
      (
        TypeInfo(LinkCtx(PageLink(PageId("page1"))), StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        buildEvaluationContext(pageIdSectionNumberMap =
          Map(ModelPageId.Pure("page1") -> SectionNumber.Classic.NormalPage(TemplateSectionIndex(1)))
        ),
        StringResult("/form/section/aaa999/-/n1"),
        "Eval LinkCtx(PageLink(PageId(xxx))) as string (exact match)"
      ),
      (
        TypeInfo(LinkCtx(PageLink(PageId("1_page1"))), StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        buildEvaluationContext(pageIdSectionNumberMap =
          Map(ModelPageId.Pure("page1") -> SectionNumber.Classic.NormalPage(TemplateSectionIndex(1)))
        ),
        StringResult("/form/section/aaa999/-/n1"),
        "Eval LinkCtx(PageLink(PageId(xxx))) as string (link from repeating/add-to-list page to non-repeating page)"
      ),
      (
        TypeInfo(LinkCtx(PageLink(PageId("page1"))), StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        buildEvaluationContext(pageIdSectionNumberMap =
          Map(ModelPageId.Indexed("page1", 1) -> SectionNumber.Classic.NormalPage(TemplateSectionIndex(1)))
        ),
        StringResult("/form/section/aaa999/-/n1"),
        "Eval LinkCtx(PageLink(PageId(xxx))) as string (link from non-repeating page to repeating page)"
      ),
      (
        TypeInfo(LinkCtx(NewForm), StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        evaluationContext,
        StringResult("/new-form/aaa999/new-form-link"),
        "Eval LinkCtx(NewForm) as string (link to new form)"
      ),
      (
        TypeInfo(LinkCtx(NewFormForTemplate(FormTemplateId("abc"))), StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        evaluationContext,
        StringResult("/new-form/abc/clean"),
        "Eval LinkCtx(NewForm(FormTemplateId(\"abc\"))) as string (link to new form)"
      ),
      (
        TypeInfo(LinkCtx(NewSession), StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        evaluationContext,
        StringResult("/new-form/aaa999/session"),
        "Eval LinkCtx(NewSession) as string (link to logout user and take to signin page)"
      ),
      (
        TypeInfo(
          UserCtx(UserField.Enrolment(ServiceName("a"), IdentifierName("b"), Option(UserFieldFunc.Index(0)))),
          StaticTypeData(ExprType.string, None)
        ),
        recDataEmpty,
        buildEvaluationContext(retrievals =
          authContext.copy(enrolments =
            Enrolments(Set(Enrolment("a", Seq(EnrolmentIdentifier("b", "b1")), "some-state", None)))
          )
        ),
        ExpressionResult.empty,
        "user enrolments at non-existent index"
      ),
      (
        TypeInfo(
          UserCtx(UserField.Enrolment(ServiceName("a"), IdentifierName("c"), Option(UserFieldFunc.Index(1)))),
          StaticTypeData(ExprType.string, None)
        ),
        recDataEmpty,
        buildEvaluationContext(retrievals =
          authContext.copy(enrolments =
            Enrolments(
              Set(
                Enrolment(
                  "a",
                  Seq(EnrolmentIdentifier("b", "b1"), EnrolmentIdentifier("b", "b2"), EnrolmentIdentifier("c", "c1")),
                  "some-state",
                  None
                )
              )
            )
          )
        ),
        StringResult("c1"),
        "user enrolments value at index"
      ),
      (
        TypeInfo(
          UserCtx(UserField.Enrolment(ServiceName("a"), IdentifierName("b"), Option(UserFieldFunc.Count))),
          StaticTypeData(ExprType.string, None)
        ),
        recDataEmpty,
        buildEvaluationContext(retrievals =
          authContext.copy(enrolments =
            Enrolments(
              Set(
                Enrolment(
                  "a",
                  Seq(EnrolmentIdentifier("b", "b1"), EnrolmentIdentifier("b", "b2"), EnrolmentIdentifier("c", "c1")),
                  "some-state",
                  None
                )
              )
            )
          )
        ),
        StringResult("2"),
        "user enrolments count for service a and identifier b"
      ),
      (
        TypeInfo(
          DataRetrieveCtx(
            DataRetrieveId("someDataRetrieveId"),
            DataRetrieve.Attribute("isValid")
          ),
          StaticTypeData(ExprType.string, None)
        ),
        recDataEmpty,
        buildEvaluationContext(thirdPartyData =
          ThirdPartyData.empty.copy(dataRetrieve =
            Some(
              Map(
                DataRetrieveId("someDataRetrieveId") -> DataRetrieveResult(
                  DataRetrieveId("someDataRetrieveId"),
                  RetrieveDataType.ObjectType(Map(DataRetrieve.Attribute("isValid") -> "111")),
                  Json.obj()
                )
              )
            )
          )
        ),
        StringResult("111"),
        "Third party data retrieval for id 'someDataRetrieveId' and attribute 'attribute1'"
      ),
      (
        TypeInfo(
          expr = DataRetrieveCtx(
            id = DataRetrieveId(value = "company"),
            attribute = DataRetrieve.Attribute(name = "registeredOfficeAddress")
          ),
          staticTypeData = StaticTypeData(exprType = ExprType.AddressString, textConstraint = None)
        ),
        recDataEmpty,
        buildEvaluationContext(thirdPartyData =
          ThirdPartyData.empty.copy(dataRetrieve =
            Some(
              Map(
                DataRetrieveId("company") -> DataRetrieveResult(
                  DataRetrieveId("registeredOfficeAddress"),
                  RetrieveDataType.ObjectType(
                    Map(
                      DataRetrieve.Attribute("address_line_1") -> "address_line_1 value",
                      DataRetrieve.Attribute("address_line_2") -> "address_line_2 value",
                      DataRetrieve.Attribute("po_box")         -> "po_box value",
                      DataRetrieve.Attribute("locality")       -> "locality value",
                      DataRetrieve.Attribute("region")         -> "region value",
                      DataRetrieve.Attribute("postal_code")    -> "postal_code value",
                      DataRetrieve.Attribute("country")        -> "country value"
                    )
                  ),
                  Json.obj()
                )
              )
            )
          )
        ),
        AddressResult(
          List(
            "address_line_1 value",
            "address_line_2 value",
            "po_box value",
            "locality value",
            "region value",
            "postal_code value",
            "country value"
          )
        ),
        "Third party data retrieval for id 'company' and attribute 'registeredOfficeAddress'"
      ),
      (
        TypeInfo(LookupColumn(FormComponentId("selectedCountry"), "InEU"), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("selectedCountry"), VariadicValue.One("United Kingdom"))
          )
        ),
        evaluationContext.copy(
          lookupRegistry = new LookupRegistry(
            Map(
              Register.Country -> AjaxLookup(
                LocalisedLookupOptions(
                  Map(
                    LangADT.En -> LookupOptions(
                      Map(
                        LookupLabel("United Kingdom") -> CountryLookupInfo(
                          LookupId("GB"),
                          0,
                          LookupKeywords(Some("England Great Britain")),
                          LookupPriority(1),
                          LookupPriority(1),
                          LookupRegion("1"),
                          LookupInGibraltarEuEeaEfta("1"),
                          Map("InEU" -> "1")
                        )
                      )
                    )
                  )
                ),
                Map.empty,
                Enabled
              )
            )
          ),
          lookupRegister = Map(FormComponentId("selectedCountry").baseComponentId -> Register.Country)
        ),
        StringResult("1"),
        "CsvCountryCheck eval to string with matching LookupInfo"
      ),
      (
        TypeInfo(LookupColumn(FormComponentId("selectedCountry"), "InEU"), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("selectedCountry"), VariadicValue.One("United Kingdom"))
          )
        ),
        evaluationContext,
        Empty,
        "CsvCountryCheck eval to string without matching LookupInfo"
      ),
      (
        TypeInfo(
          CsvCountryCountCheck(FormComponentId("selectedCountry"), "InEU", "1"),
          StaticTypeData(ExprType.number, None)
        ),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("selectedCountry"), VariadicValue.One("United Kingdom"))
          )
        ),
        evaluationContext.copy(
          lookupRegistry = new LookupRegistry(
            Map(
              Register.Country -> AjaxLookup(
                LocalisedLookupOptions(
                  Map(
                    LangADT.En -> LookupOptions(
                      Map(
                        LookupLabel("United Kingdom") -> CountryLookupInfo(
                          LookupId("GB"),
                          0,
                          LookupKeywords(Some("England Great Britain")),
                          LookupPriority(1),
                          LookupPriority(1),
                          LookupRegion("1"),
                          LookupInGibraltarEuEeaEfta("1"),
                          Map("InEU" -> "1")
                        )
                      )
                    )
                  )
                ),
                Map.empty,
                Enabled
              )
            )
          ),
          lookupRegister = Map(FormComponentId("selectedCountry").baseComponentId -> Register.Country)
        ),
        NumberResult(1),
        "CsvCountryCountCheck eval to number with matching LookupInfo"
      ),
      (
        TypeInfo(
          CsvCountryCountCheck(FormComponentId("selectedCountry"), "InEU", "1"),
          StaticTypeData(ExprType.number, None)
        ),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("selectedCountry"), VariadicValue.One("United Kingdom"))
          )
        ),
        evaluationContext,
        NumberResult(0),
        "CsvCountryCountCheck eval to number without matching LookupInfo"
      ),
      (
        TypeInfo(
          LookupColumn(FormComponentId("selectedCountry"), "InEU"),
          StaticTypeData(ExprType.string, None)
        ),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("selectedCountry-country"), VariadicValue.One("United Kingdom"))
          )
        ),
        evaluationContext
          .copy(lookupRegistry =
            new LookupRegistry(
              Map(
                Register.Country -> AjaxLookup(
                  LocalisedLookupOptions(
                    Map(
                      LangADT.En -> LookupOptions(
                        Map(
                          LookupLabel("United Kingdom") -> CountryLookupInfo(
                            LookupId("GB"),
                            0,
                            LookupKeywords(Some("England Great Britain")),
                            LookupPriority(1),
                            LookupPriority(1),
                            LookupRegion("1"),
                            LookupInGibraltarEuEeaEfta("1"),
                            Map("InEU" -> "1")
                          )
                        )
                      )
                    )
                  ),
                  Map.empty,
                  Enabled
                )
              )
            )
          )
          .copy(overseasAddressLookup = Set(BaseComponentId("selectedCountry"))),
        StringResult("1"),
        "CsvCountryCheck eval to string with matching LookupInfo"
      ),
      (
        TypeInfo(
          LookupColumn(FormComponentId("selectedCountry"), "InEU"),
          StaticTypeData(ExprType.string, None)
        ),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("selectedCountry-country"), VariadicValue.One("United Kingdom"))
          )
        ),
        evaluationContext.copy(
          overseasAddressLookup = Set(BaseComponentId("selectedCountry")),
          lookupRegistry = new LookupRegistry(
            Map(
              Register.Country -> AjaxLookup(
                LocalisedLookupOptions(
                  Map(
                    LangADT.En -> LookupOptions(
                      Map(
                        LookupLabel("United Kingdom") -> CountryLookupInfo(
                          LookupId("GB"),
                          0,
                          LookupKeywords(Some("England Great Britain")),
                          LookupPriority(1),
                          LookupPriority(1),
                          LookupRegion("1"),
                          LookupInGibraltarEuEeaEfta("1"),
                          Map("InEU" -> "0")
                        )
                      )
                    )
                  )
                ),
                Map.empty,
                Enabled
              )
            )
          ),
          lookupRegister = Map(FormComponentId("selectedCountry").baseComponentId -> Register.Country)
        ),
        StringResult("0"),
        "CsvOverseasCountryCheck eval to string without matching LookupInfo"
      ),
      (
        TypeInfo(
          StringOps(FormCtx(FormComponentId("textField")), StringFnc.Capitalize),
          StaticTypeData(ExprType.string, None)
        ),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("textField"), VariadicValue.One("the TaX Rate"))
          )
        ),
        evaluationContext,
        StringResult("The TaX Rate"),
        "FormCtx expression converted to Capitalize"
      ),
      (
        TypeInfo(
          StringOps(StringOps(FormCtx(FormComponentId("textField")), StringFnc.LowerCase), StringFnc.CapitalizeAll),
          StaticTypeData(ExprType.string, None)
        ),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("textField"), VariadicValue.One("The tAx raTE"))
          )
        ),
        evaluationContext,
        StringResult("The Tax Rate"),
        "FormCtx expression converted to Lowercase and CapitalizeAll"
      ),
      (
        TypeInfo(
          StringOps(FormCtx(FormComponentId("textField")), StringFnc.LowerCaseFirst),
          StaticTypeData(ExprType.string, None)
        ),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("textField"), VariadicValue.One("The Tax raTE"))
          )
        ),
        evaluationContext,
        StringResult("the Tax raTE"),
        "FormCtx expression converted to LowercaseFirst"
      ),
      (
        TypeInfo(
          Concat(
            List(
              FormCtx(FormComponentId("textField1")),
              Constant(" "),
              FormCtx(FormComponentId("textField2"))
            )
          ),
          StaticTypeData(ExprType.string, None)
        ),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("textField1"), VariadicValue.One("James")),
            (toModelComponentId("textField2"), VariadicValue.One("Smith"))
          )
        ),
        evaluationContext,
        StringResult("James Smith"),
        "Eval Concat(List(Exprs)) as string (concat the values with space)"
      ),
      (
        TypeInfo(
          Concat(
            List(
              Constant("Business owes "),
              Typed(Constant("200"), ExplicitExprType.Sterling(RoundingMode.HalfUp)),
              Constant(" at rate of £"),
              Constant("100"),
              Constant(" in tax year "),
              Add(DateFunction(DateProjection.Year(DateValueExpr(TodayDateExprValue))), Constant("1"))
            )
          ),
          StaticTypeData(ExprType.string, None)
        ),
        recDataEmpty,
        evaluationContext,
        StringResult(s"Business owes £200.00 at rate of £100 in tax year ${LocalDate.now().plusYears(1).getYear}"),
        "Eval Concat(List(Exprs)) as string (concat the values)"
      ),
      (
        TypeInfo(UserCtx(UserField.CredentialRole), StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        buildEvaluationContext(retrievals = authContext.copy(credentialRole = Some(User))),
        StringResult("user"),
        "Fetch and return the 'User' user credential role"
      ),
      (
        TypeInfo(UserCtx(UserField.CredentialRole), StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        buildEvaluationContext(retrievals = authContext.copy(credentialRole = Some(Assistant))),
        StringResult("assistant"),
        "Fetch and return the 'Assistant' user credential role"
      ),
      (
        TypeInfo(UserCtx(UserField.CredentialRole), StaticTypeData(ExprType.string, None)),
        recDataEmpty,
        evaluationContext,
        Empty,
        "Be empty when no credential role is found"
      ),
      (
        TypeInfo(HideZeroDecimals(FormCtx(FormComponentId("sterlingField"))), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("sterlingField"), VariadicValue.One("14500099.00"))
          )
        ),
        evaluationContext.copy(constraints =
          Map(FormComponentId("sterlingField").baseComponentId -> Sterling(RoundingMode.defaultRoundingMode, false))
        ),
        StringResult("£14,500,099"),
        "Should format without trailing zeros when type is sterling"
      ),
      (
        TypeInfo(HideZeroDecimals(FormCtx(FormComponentId("sterlingField"))), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("sterlingField"), VariadicValue.One("14500099.00"))
          )
        ),
        evaluationContext.copy(constraints =
          Map(FormComponentId("sterlingField").baseComponentId -> Sterling(RoundingMode.defaultRoundingMode, false))
        ),
        StringResult("£14,500,099"),
        "Should format without trailing zeros when type is sterling"
      ),
      (
        TypeInfo(HideZeroDecimals(FormCtx(FormComponentId("numberField"))), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("numberField"), VariadicValue.One("14500099.00"))
          )
        ),
        evaluationContext.copy(constraints = Map(FormComponentId("numberField").baseComponentId -> Number())),
        StringResult("14,500,099"),
        "Should format without trailing zeros when type is number"
      ),
      (
        TypeInfo(HideZeroDecimals(FormCtx(FormComponentId("numberField"))), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("numberField"), VariadicValue.One("14500099.01"))
          )
        ),
        evaluationContext.copy(constraints = Map(FormComponentId("numberField").baseComponentId -> Number())),
        StringResult("14,500,099.01"),
        "Should format with trailing zeros when type is number"
      )
    )
    forAll(table) {
      (
        typeInfo: TypeInfo,
        recData: RecData[OutOfDate],
        evaluationContext: EvaluationContext,
        expectedResult: ExpressionResult,
        _
      ) =>
        EvaluationResults(Map.empty, SourceOrigin.changeSource(recData), RepeatedComponentsDetails.empty)
          .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext) shouldBe expectedResult
    }
  }

  it should "evaluate form component reference from outside, when they are in indexed pages (repeating page, ATL)" in {

    val table = Table(
      ("typeInfo", "recData", "evaluationContext", "expectedResult", "exprMap", "repeatedDetails", "scenario"),
      (
        TypeInfo(FormCtx(FormComponentId("addToListNumField")), StaticTypeData(ExprType.number, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("1_addToListQuestion"), VariadicValue.One("0")),
            (toModelComponentId("2_addToListQuestion"), VariadicValue.One("1")),
            (toModelComponentId("1_addToListNumField"), VariadicValue.One("1")),
            (toModelComponentId("2_addToListNumField"), VariadicValue.One("2"))
          )
        ),
        buildEvaluationContext(indexedComponentIds =
          List(
            FormComponentId("1_addToListNumField").modelComponentId,
            FormComponentId("2_addToListNumField").modelComponentId
          )
        ),
        ListResult(List(NumberResult(1), NumberResult(2))),
        Map[Expr, ExpressionResult](
          FormCtx(FormComponentId("1_addToListNumField")) -> NumberResult(1),
          FormCtx(FormComponentId("2_addToListNumField")) -> NumberResult(2)
        ),
        RepeatedComponentsDetails.empty,
        "Ref to AddToList number field from outside ATL"
      ),
      (
        TypeInfo(Sum(FormCtx(FormComponentId("addToListNumField"))), StaticTypeData(ExprType.number, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("1_addToListQuestion"), VariadicValue.One("0")),
            (toModelComponentId("2_addToListQuestion"), VariadicValue.One("1")),
            (toModelComponentId("1_addToListNumField"), VariadicValue.One("1")),
            (toModelComponentId("2_addToListNumField"), VariadicValue.One("2"))
          )
        ),
        buildEvaluationContext(indexedComponentIds =
          List(
            FormComponentId("1_addToListNumField").modelComponentId,
            FormComponentId("2_addToListNumField").modelComponentId
          )
        ),
        NumberResult(3),
        Map[Expr, ExpressionResult](
          FormCtx(FormComponentId("1_addToListNumField")) -> NumberResult(1),
          FormCtx(FormComponentId("2_addToListNumField")) -> NumberResult(2)
        ),
        RepeatedComponentsDetails(
          Map[FormComponentId, FormComponentId](
            FormComponentId("addToListNumField") -> FormComponentId("atlParent")
          )
        ),
        "Ref to Sum of AddToList number field from outside ATL"
      ),
      (
        TypeInfo(FormCtx(FormComponentId("addToListStrField")), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("1_addToListQuestion"), VariadicValue.One("0")),
            (toModelComponentId("2_addToListQuestion"), VariadicValue.One("1")),
            (toModelComponentId("1_addToListStrField"), VariadicValue.One("AAA")),
            (toModelComponentId("2_addToListStrField"), VariadicValue.One("BBB"))
          )
        ),
        buildEvaluationContext(indexedComponentIds =
          List(
            FormComponentId("1_addToListStrField").modelComponentId,
            FormComponentId("2_addToListStrField").modelComponentId
          )
        ),
        ListResult(List(StringResult("AAA"), StringResult("BBB"))),
        Map[Expr, ExpressionResult](
          FormCtx(FormComponentId("1_addToListStrField")) -> StringResult("AAA"),
          FormCtx(FormComponentId("2_addToListStrField")) -> StringResult("BBB")
        ),
        RepeatedComponentsDetails.empty,
        "Ref to AddToList string field from outside ATL"
      ),
      (
        TypeInfo(FormCtx(FormComponentId("addToListChoiceField")), StaticTypeData(ExprType.choiceSelection, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("1_addToListQuestion"), VariadicValue.One("0")),
            (toModelComponentId("2_addToListQuestion"), VariadicValue.One("1")),
            (toModelComponentId("1_addToListChoiceField"), VariadicValue.Many(Seq("1"))),
            (toModelComponentId("2_addToListChoiceField"), VariadicValue.Many(Seq("0")))
          )
        ),
        buildEvaluationContext(indexedComponentIds =
          List(
            FormComponentId("1_addToListChoiceField").modelComponentId,
            FormComponentId("2_addToListChoiceField").modelComponentId
          )
        ),
        ListResult(List(OptionResult(Seq("1")), OptionResult(Seq("0")))),
        Map[Expr, ExpressionResult](
          FormCtx(FormComponentId("1_addToListChoiceField")) -> OptionResult(Seq("1")),
          FormCtx(FormComponentId("2_addToListChoiceField")) -> OptionResult(Seq("0"))
        ),
        RepeatedComponentsDetails.empty,
        "Ref to AddToList string field from outside ATL"
      )
    )
    forAll(table) {
      (
        typeInfo: TypeInfo,
        recData: RecData[OutOfDate],
        evaluationContext: EvaluationContext,
        expectedResult: ExpressionResult,
        exprMap: Map[Expr, ExpressionResult],
        repeatedDetails: RepeatedComponentsDetails,
        _
      ) =>
        EvaluationResults(exprMap, SourceOrigin.changeSource(recData), repeatedDetails)
          .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext) shouldBe expectedResult
    }
  }

  "evalExpr - type number" should "evaluate expressions" in {
    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        (toModelComponentId("1_addToListQuestion"), VariadicValue.One("0")),
        (toModelComponentId("2_addToListQuestion"), VariadicValue.One("1")),
        (toModelComponentId("1_addToListField1"), VariadicValue.One("Hello")),
        (toModelComponentId("2_addToListField1"), VariadicValue.One("World")),
        (toModelComponentId("amount"), VariadicValue.One("14.00")),
        (toModelComponentId("amountString"), VariadicValue.One("Fourteen"))
      )
    )

    val table = Table(
      ("typeInfo", "recData", "evaluationContext", "expectedResult", "exprMap", "repeatedDetails", "scenario"),
      (
        TypeInfo(Count(FormComponentId("addToListQuestion")), StaticTypeData(ExprType.number, None)),
        recData,
        evaluationContext,
        NumberResult(2),
        Map.empty[Expr, ExpressionResult],
        RepeatedComponentsDetails.empty,
        "Ref to AddToList count in number field"
      ),
      (
        TypeInfo(Sum(FormCtx(FormComponentId("addToListQuestion"))), StaticTypeData(ExprType.number, None)),
        recData,
        evaluationContext.copy(indexedComponentIds =
          FormModel.modelComponentsToIndexedComponentMap(List(toModelComponentId("1_addToListQuestion")))
        ),
        NumberResult(1),
        Map[Expr, ExpressionResult](
          FormCtx(FormComponentId("1_addToListQuestion")) -> NumberResult(0),
          FormCtx(FormComponentId("2_addToListQuestion")) -> NumberResult(1)
        ),
        RepeatedComponentsDetails(
          Map[FormComponentId, FormComponentId](
            FormComponentId("addToListQuestion") -> FormComponentId("atlParent")
          )
        ),
        "Ref to AddToList sum in number field"
      ),
      (
        TypeInfo(
          UserCtx(UserField.Enrolment(ServiceName("a"), IdentifierName("b"), Option(UserFieldFunc.Count))),
          StaticTypeData(ExprType.number, None)
        ),
        recData,
        buildEvaluationContext(retrievals =
          authContext.copy(enrolments =
            Enrolments(Set(Enrolment("a", Seq(EnrolmentIdentifier("b", "1")), "some-state", None)))
          )
        ),
        NumberResult(1),
        Map.empty[Expr, ExpressionResult],
        RepeatedComponentsDetails.empty,
        "user enrolments count for service a and identifier b"
      ),
      (
        TypeInfo(
          ParamCtx(QueryParam("availAmt")),
          StaticTypeData(ExprType.number, Some(Sterling(RoundingMode.Down, false)))
        ),
        recData,
        evaluationContext,
        NumberResult(123),
        Map.empty[Expr, ExpressionResult],
        RepeatedComponentsDetails.empty,
        "convert param to Sterling"
      ),
      (
        TypeInfo(
          ParamCtx(QueryParam("availAmtStr")),
          StaticTypeData(ExprType.number, Some(Sterling(RoundingMode.Down, false)))
        ),
        recData,
        evaluationContext,
        ExpressionResult.Invalid("Number - cannot convert 'foo' to number"),
        Map.empty[Expr, ExpressionResult],
        RepeatedComponentsDetails.empty,
        "convert param to Sterling (fail when param is not a convertible to number)"
      ),
      (
        TypeInfo(
          HideZeroDecimals(FormCtx(FormComponentId("amount"))),
          StaticTypeData(ExprType.number, Some(Sterling(RoundingMode.Down, false)))
        ),
        recData,
        evaluationContext,
        NumberResult(14.00),
        Map.empty[Expr, ExpressionResult],
        RepeatedComponentsDetails.empty,
        "use hide zero decimals without fail on number field"
      ),
      (
        TypeInfo(
          HideZeroDecimals(FormCtx(FormComponentId("amountString"))),
          StaticTypeData(ExprType.number, Some(Sterling(RoundingMode.Down, false)))
        ),
        recData,
        evaluationContext,
        ExpressionResult.Invalid("Number - cannot convert 'Fourteen' to number"),
        Map.empty[Expr, ExpressionResult],
        RepeatedComponentsDetails.empty,
        "use hide zero decimals with graceful failure on string field"
      ),
      (
        TypeInfo(UserCtx(UserField.CredentialRole), StaticTypeData(ExprType.number, None)),
        recData,
        evaluationContext,
        ExpressionResult.Invalid("Number - unsupported computation. Cannot combine Number and UserCtx(CredentialRole)"),
        Map.empty[Expr, ExpressionResult],
        RepeatedComponentsDetails.empty,
        "return an unsupported operation"
      )
    )
    forAll(table) {
      (
        typeInfo: TypeInfo,
        recData: RecData[OutOfDate],
        evaluationContext,
        expectedResult: ExpressionResult,
        exprMap: Map[Expr, ExpressionResult],
        repeatedDetails: RepeatedComponentsDetails,
        _
      ) =>
        EvaluationResults(exprMap, SourceOrigin.changeSource(recData), repeatedDetails)
          .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext) shouldBe expectedResult
    }
  }

  "evalExpr - type period" should "evaluate expressions" in {

    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        (toModelComponentId("startDate1-year"), VariadicValue.One("2000")),
        (toModelComponentId("startDate1-month"), VariadicValue.One("1")),
        (toModelComponentId("startDate1-day"), VariadicValue.One("1")),
        (toModelComponentId("endDate1-year"), VariadicValue.One("2001")),
        (toModelComponentId("endDate1-month"), VariadicValue.One("2")),
        (toModelComponentId("endDate1-day"), VariadicValue.One("2")),
        (toModelComponentId("startDate2-year"), VariadicValue.One("2002")),
        (toModelComponentId("startDate2-month"), VariadicValue.One("1")),
        (toModelComponentId("startDate2-day"), VariadicValue.One("1")),
        (toModelComponentId("endDate2-year"), VariadicValue.One("2003")),
        (toModelComponentId("endDate2-month"), VariadicValue.One("10")),
        (toModelComponentId("endDate2-day"), VariadicValue.One("1"))
      )
    )

    val table = Table(
      ("typeInfo", "recData", "expectedResult"),
      (
        TypeInfo(
          Period(
            DateCtx(DateValueExpr(ExactDateExprValue(2001, 1, 1))),
            DateCtx(
              DateExprWithOffset(
                DateValueExpr(ExactDateExprValue(2001, 1, 1)),
                OffsetYMD(List(Year(1), Month(1), Day(1)))
              )
            )
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(java.time.Period.of(1, 1, 1))
      ),
      (
        TypeInfo(
          Period(
            DateCtx(
              DateExprWithOffset(DateFormCtxVar(FormCtx(FormComponentId("startDate1"))), OffsetYMD(List(Year(1))))
            ),
            DateCtx(DateValueExpr(ExactDateExprValue(2003, 2, 2)))
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(java.time.Period.of(2, 1, 1))
      ),
      (
        TypeInfo(
          Period(
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1"))))
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(java.time.Period.of(1, 1, 1))
      ),
      (
        TypeInfo(
          PeriodExt(
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1"))))
            ),
            PeriodFn.Years
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        NumberResult(1)
      ),
      (
        TypeInfo(
          Add(
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1"))))
            ),
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate2")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate2"))))
            )
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(java.time.Period.of(2, 10, 1))
      ),
      (
        TypeInfo(
          Add(
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1"))))
            ),
            PeriodValue("P1Y")
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(java.time.Period.of(2, 1, 1))
      ),
      (
        TypeInfo(
          Else(
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate3")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate3"))))
            ),
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1"))))
            )
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(java.time.Period.of(1, 1, 1))
      )
    )

    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult) =>
      EvaluationResults.empty
        .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext) shouldBe expectedResult
    }
  }

  it should "evaluate group expressions" in {
    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        //group 1
        (toModelComponentId("1_startDate-year"), VariadicValue.One("2000")),
        (toModelComponentId("1_startDate-month"), VariadicValue.One("1")),
        (toModelComponentId("1_startDate-day"), VariadicValue.One("1")),
        (toModelComponentId("1_endDate-year"), VariadicValue.One("2000")),
        (toModelComponentId("1_endDate-month"), VariadicValue.One("10")),
        (toModelComponentId("1_endDate-day"), VariadicValue.One("11")),
        // group 2
        (toModelComponentId("2_startDate-year"), VariadicValue.One("2001")),
        (toModelComponentId("2_startDate-month"), VariadicValue.One("1")),
        (toModelComponentId("2_startDate-day"), VariadicValue.One("1")),
        (toModelComponentId("2_endDate-year"), VariadicValue.One("2001")),
        (toModelComponentId("2_endDate-month"), VariadicValue.One("11")),
        (toModelComponentId("2_endDate-day"), VariadicValue.One("1"))
      )
    )

    val exprMap = Map[Expr, ExpressionResult](
      FormCtx(FormComponentId("1_startDate")) -> DateResult(LocalDate.of(2000, 1, 1)),
      FormCtx(FormComponentId("1_endDate"))   -> DateResult(LocalDate.of(2000, 10, 11)),
      FormCtx(FormComponentId("2_startDate")) -> DateResult(LocalDate.of(2001, 1, 1)),
      FormCtx(FormComponentId("2_endDate"))   -> DateResult(LocalDate.of(2001, 11, 1))
    )

    val repeatedComponentsDetails = RepeatedComponentsDetails(
      Map[FormComponentId, FormComponentId](
        FormComponentId("startDate") -> FormComponentId("atlParent"),
        FormComponentId("endDate")   -> FormComponentId("atlParent")
      )
    )

    val table = Table(
      ("typeInfo", "recData", "expectedResult"),
      (
        TypeInfo(
          PeriodExt(
            Period(
              DateCtx(DateValueExpr(ExactDateExprValue(2000, 1, 1))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            PeriodFn.Sum
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(java.time.Period.of(2, 7, 10))
      ),
      (
        TypeInfo(
          PeriodExt(
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate")))),
              DateCtx(DateValueExpr(ExactDateExprValue(2000, 1, 1)))
            ),
            PeriodFn.Sum
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(java.time.Period.of(-2, -7, -10))
      ),
      (
        TypeInfo(
          PeriodExt(
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            PeriodFn.Sum
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(java.time.Period.of(1, 7, 10))
      ),
      (
        TypeInfo(
          PeriodExt(
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            PeriodFn.TotalMonths
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        NumberResult(19)
      ),
      (
        TypeInfo(
          Sum(
            Between(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate")))),
              MeasurementType.Weeks
            )
          ),
          StaticTypeData(ExprType.number, None)
        ),
        recData,
        NumberResult(83)
      ),
      (
        TypeInfo(
          Sum(
            Between(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate")))),
              MeasurementType.Days
            )
          ),
          StaticTypeData(ExprType.number, None)
        ),
        recData,
        NumberResult(590)
      ),
      (
        TypeInfo(
          PeriodExt(
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            PeriodFn.Years
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        NumberResult(1)
      ),
      (
        TypeInfo(
          PeriodExt(
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            PeriodFn.Months
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        NumberResult(7)
      ),
      (
        TypeInfo(
          PeriodExt(
            Period(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            PeriodFn.Days
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        NumberResult(10)
      )
    )
    forAll(table) {
      (
        typeInfo: TypeInfo,
        recData: RecData[OutOfDate],
        expectedResult: ExpressionResult
      ) =>
        EvaluationResults(exprMap, SourceOrigin.changeSource(recData), repeatedComponentsDetails)
          .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext) shouldBe expectedResult
    }
  }

  "evalExpr - type between" should "evaluate expressions" in {

    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        (toModelComponentId("startDate1-year"), VariadicValue.One("2000")),
        (toModelComponentId("startDate1-month"), VariadicValue.One("1")),
        (toModelComponentId("startDate1-day"), VariadicValue.One("1")),
        (toModelComponentId("endDate1-year"), VariadicValue.One("2001")),
        (toModelComponentId("endDate1-month"), VariadicValue.One("2")),
        (toModelComponentId("endDate1-day"), VariadicValue.One("2")),
        (toModelComponentId("endDate2-year"), VariadicValue.One("2000")),
        (toModelComponentId("endDate2-month"), VariadicValue.One("1")),
        (toModelComponentId("endDate2-day"), VariadicValue.One("7")),
        (toModelComponentId("endDate3-year"), VariadicValue.One("2000")),
        (toModelComponentId("endDate3-month"), VariadicValue.One("1")),
        (toModelComponentId("endDate3-day"), VariadicValue.One("5"))
      )
    )
    val table = Table(
      ("typeInfo", "recData", "expectedResult"),
      (
        TypeInfo(
          Between(
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1")))),
            MeasurementType.Days
          ),
          StaticTypeData(ExprType.number, None)
        ),
        recData,
        NumberResult(399)
      ),
      (
        TypeInfo(
          Between(
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1")))),
            MeasurementType.Weeks
          ),
          StaticTypeData(ExprType.number, None)
        ),
        recData,
        NumberResult(57)
      ),
      (
        TypeInfo(
          Between(
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
            MeasurementType.Days
          ),
          StaticTypeData(ExprType.number, None)
        ),
        recData,
        NumberResult(1)
      ),
      (
        TypeInfo(
          Between(
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate2")))),
            MeasurementType.Weeks
          ),
          StaticTypeData(ExprType.number, None)
        ),
        recData,
        NumberResult(1)
      ),
      (
        TypeInfo(
          Between(
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate3")))),
            MeasurementType.Weeks
          ),
          StaticTypeData(ExprType.number, None)
        ),
        recData,
        NumberResult(0)
      ),
      (
        TypeInfo(
          Between(
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate3")))),
            MeasurementType.Days
          ),
          StaticTypeData(ExprType.number, None)
        ),
        recData,
        NumberResult(5)
      ),
      (
        TypeInfo(
          Divide(
            Between(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1")))),
              MeasurementType.Days
            ),
            Constant("7")
          ),
          StaticTypeData(ExprType.number, None)
        ),
        recData,
        NumberResult(57)
      )
    )
    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult) =>
      EvaluationResults.empty
        .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext) shouldBe expectedResult
    }
  }
}
