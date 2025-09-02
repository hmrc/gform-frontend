/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.graph

import munit.FunSuite
import org.mockito.MockitoSugar.{ times, verify }
import org.mockito.{ ArgumentMatchersSugar, IdiomaticMockito }
import uk.gov.hmrc.gform.auth.UtrEligibilityRequest
import uk.gov.hmrc.gform.auth.models.{ GovernmentGatewayId, IdentifierValue }
import uk.gov.hmrc.gform.eval.{ DbLookupChecker, DelegatedEnrolmentChecker, SeissEligibilityChecker }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ ls, mkFormTemplate, mkSection }
import uk.gov.hmrc.gform.models.ids.IndexedComponentId.Indexed
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.{ FormModelSupport, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DataSource, FormComponent, FormComponentId, FormCtx, In, IncludeIf, ShortText, Text, Value }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, SourceOrigin, VariadicFormData, VariadicValue }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class GraphDataCacheServiceSuite
    extends FunSuite with ArgumentMatchersSugar with IdiomaticMockito with FormModelSupport {

  private val mockSeissEligibilityChecker = mock[SeissEligibilityChecker]
  mockSeissEligibilityChecker(*[UtrEligibilityRequest], *[HeaderCarrier]) shouldReturn Future.successful(true)

  private val mockDelegatedEnrolmentCheckStatus = mock[DelegatedEnrolmentChecker]
  mockDelegatedEnrolmentCheckStatus(
    *[GovernmentGatewayId],
    *[DataSource.DelegatedEnrolment],
    *[IdentifierValue],
    *[HeaderCarrier]
  ) shouldReturn Future.successful(true)

  private val mockDbLookupCheckStatus = mock[DbLookupChecker]

  override def beforeEach(context: BeforeEach): Unit = {
    reset(mockDbLookupCheckStatus)
    mockDbLookupCheckStatus(*[String], *[CollectionName], *[HeaderCarrier]) shouldReturn Future.successful(true)
    super.beforeEach(context)
  }

  private val service =
    new GraphDataCacheService(mockSeissEligibilityChecker, mockDelegatedEnrolmentCheckStatus, mockDbLookupCheckStatus)

  private val baseComponentIdValue = "baseComponentId"
  private val modelComponentId = ModelComponentId.pure(Indexed(BaseComponentId(baseComponentIdValue), 1))
  private val value = "value"
  private val data: VariadicFormData[SourceOrigin.OutOfDate] = VariadicFormData(
    Map.from(
      Seq(
        modelComponentId -> VariadicValue.One(value)
      )
    )
  )

  private val dataSource = DataSource.Mongo(CollectionName("test"))

  private val inExpr = In(FormCtx(FormComponentId("1_" + baseComponentIdValue)), dataSource)

  private val formTemplate = mkFormTemplate(
    mkSection(
      List(
        FormComponent(
          FormComponentId("id"),
          Text(ShortText.default, Value),
          ls,
          false,
          None,
          None,
          Some(
            IncludeIf(
              inExpr
            )
          ),
          None,
          true,
          false,
          true,
          false,
          false,
          None,
          None
        )
      )
    )
  )

  implicit private val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  test("GraphDataCache updates boolean expr cache if new data is available") {
    service
      .get[SectionSelectorType.Normal](
        retrievals,
        data,
        formTemplate,
        mkFormModelBuilder(formTemplate),
        BooleanExprCache(Map(dataSource -> Map("other" -> true)))
      )
      .map { result =>
        verify(mockDbLookupCheckStatus, times(1)).apply(
          any[String],
          any[CollectionName],
          any[HeaderCarrier]
        )
        assertEquals(
          result.booleanExprCache,
          BooleanExprCache(Map(dataSource -> Map("other" -> true, value -> true)))
        )
      }

  }

  test("InExprResolver returns the value of resolved Future call") {
    service
      .get[SectionSelectorType.Normal](
        retrievals,
        data,
        formTemplate,
        mkFormModelBuilder(formTemplate),
        BooleanExprCache.empty
      )
      .map { result =>
        assertEquals(result.inExprResolver(inExpr), true)
      }
  }

  test("InExprResolver returns false if the data is yet to be entered") {
    service
      .get[SectionSelectorType.Normal](
        retrievals,
        data,
        formTemplate,
        mkFormModelBuilder(formTemplate),
        BooleanExprCache.empty
      )
      .map { result =>
        val inExpr = In(FormCtx(FormComponentId("2_" + baseComponentIdValue)), dataSource)
        assertEquals(result.inExprResolver(inExpr), false)
      }
  }

  test("GraphDataCache returns the same boolean expr cache if data was not updated") {
    service
      .get[SectionSelectorType.Normal](
        retrievals,
        data,
        formTemplate,
        mkFormModelBuilder(formTemplate),
        BooleanExprCache(Map(dataSource -> Map(value -> true)))
      )
      .map { result =>
        verify(mockDbLookupCheckStatus, times(0)).apply(
          any[String],
          any[CollectionName],
          any[HeaderCarrier]
        )
        assertEquals(
          result.booleanExprCache,
          BooleanExprCache(Map(dataSource -> Map(value -> true)))
        )
      }
  }

  test("InExprResolver throws a runtime exception if called with In expression that can't exist") {
    service
      .get[SectionSelectorType.Normal](
        retrievals,
        data,
        formTemplate,
        mkFormModelBuilder(formTemplate),
        BooleanExprCache.empty
      )
      .map { result =>
        intercept[RuntimeException](
          result.inExprResolver(In(FormCtx(FormComponentId("nonExistent")), dataSource))
        )
      }
  }
}
