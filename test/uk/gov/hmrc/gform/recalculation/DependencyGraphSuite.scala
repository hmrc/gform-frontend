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

package uk.gov.hmrc.gform.recalculation

import cats.data.NonEmptyList
import java.time.LocalDateTime
import munit.FunSuite
import play.api.i18n.Messages
import play.api.libs.json.Json
import play.api.test.Helpers
import scala.concurrent.Future
import uk.gov.hmrc.auth.core.{ ConfidenceLevel, Enrolments }
import uk.gov.hmrc.gform.RealJsonTemplateSupport
import uk.gov.hmrc.gform.addresslookup.{ AddressLookupResult, PostcodeLookupRetrieve }
import uk.gov.hmrc.gform.commons.MarkDownUtil
import uk.gov.hmrc.gform.lookup.{ AjaxLookup, ShowAll }
import uk.gov.hmrc.gform.SmartStringCollector
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, GovernmentGatewayId, OtherRetrievals }
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluationSyntax }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.lookup.{ CountryLookupInfo, LocalisedLookupOptions, LookupCountryCode, LookupId, LookupInGibraltarEuEeaEfta, LookupLabel, LookupOptions, LookupPriority, LookupRegion, LookupRegistry, LookupSicCodeSection, NationalityLookupInfo, SicCodeLookupInfo }
import uk.gov.hmrc.gform.sharedmodel.form.VisitIndex
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Register
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroup, DataRetrieve, DataRetrieveId, DataRetrieveResult, LangADT, RetrieveDataType, SmartString }

class DependencyGraphSuite extends FunSuite {

  def toVisitIndex(vi: List[String]): VisitIndex = {

    val isTaskList = vi.forall(_.contains(",")) && !vi.isEmpty

    val json = if (isTaskList) {
      val mapping: List[((String, String), String)] = vi.map { visit =>
        visit.split(",").toList match {
          case taskSectionNumber :: taskNumber :: classic :: Nil =>
            (taskSectionNumber, taskNumber) -> classic
          case _ => throw new Exception(s"Unexpected visitIndex: $visit")
        }
      }

      val jsonStr: String = mapping
        .groupBy { case (key, _) => key }
        .map { case (key, values) =>
          key -> values.map { case (_, v) => v }
        }
        .map { case ((tsn, tn), classics) =>
          "\"" + tsn + "," + tn + "\": [" + classics.map(classic => "\"" + classic + "\"").mkString(",") + "]"
        }
        .mkString(",")

      s"""{"visitsIndex": {$jsonStr}}"""

    } else {
      val visitIndex = vi.map(s => "\"" + s + "\"").mkString(",")

      s"""{"visitsIndex": [$visitIndex]}"""
    }

    Json.parse(json).validate[VisitIndex].asOpt.get
  }

  (
    FormModelOpticsTests1.data ++
      FormModelOpticsTests2.data ++
      FormModelOpticsTests3.data ++
      FormModelOpticsTests4.data
  ).zipWithIndex.map {
    case (
          (
            mongoUserData,
            visitIndex,
            evaluationContext,
            answerMapExpected,
            smartStringValuesExpected,
            filenameWithComment
          ),
          i
        ) =>
      val index = i + 1
      val parts = filenameWithComment.split(" ", 2) // limit = 2 means split into at most 2 parts
      val (filename, comment) = if (parts.length == 1) {
        (parts(0), "")
      } else {
        (parts(0), parts(1).trim)
      }

      if (filename.isEmpty) {
        throw new Exception(s"Missing file name in: '$filenameWithComment'")
      }

      if (comment.endsWith("ref")) {
        refreshTemplates()
      }

      test(s"$filename $comment ($index)") {
        runTest(
          mongoUserData,
          toVisitIndex(visitIndex),
          evaluationContext,
          answerMapExpected,
          smartStringValuesExpected,
          filename
        )
      }
  }

  def refreshTemplates() = RealJsonTemplateSupport.refreshTemplates("recalculation")

  implicit val messages: Messages = Helpers.stubMessages(
    Helpers.stubMessagesApi(
      Map(
        "en" -> Map(
          "date.January"   -> "January",
          "date.February"  -> "February",
          "date.March"     -> "March",
          "date.April"     -> "April",
          "date.May"       -> "May",
          "date.June"      -> "June",
          "date.July"      -> "July",
          "date.August"    -> "August",
          "date.September" -> "September",
          "date.October"   -> "October",
          "date.November"  -> "November",
          "date.December"  -> "December"
        )
      )
    )
  )

  implicit val l: LangADT = LangADT.En

  def runTest(
    mongoUserData: MongoUserData,
    visitIndex: VisitIndex,
    evaluationContext0: EvaluationContext,
    answerMapExpected: AnswerMap,
    smartStringValuesExpected: List[String],
    filename: String
  ): Future[Either[UnexpectedState, Unit]] = {
    val filePath = s"test-templates/recalculation/internal/$filename"

    val formModelOptics = RealJsonTemplateSupport.mkFormModelOptics(
      mongoUserData,
      visitIndex,
      evaluationContext0,
      filePath
    )

    val formModelVisibilityOptics = formModelOptics.formModelVisibilityOptics

    val smartStrings: List[(SmartString, Boolean)] =
      SmartStringCollector.collectSmartStrings(formModelVisibilityOptics.formModel).filter { case (smartString, _) =>
        smartString match {
          case SmartString.SmartStringBase(internal)                    => internal.interpolations.nonEmpty
          case SmartString.SmartStringCond(ifConditions, elseCondition) => ifConditions.nonEmpty
        }
      }

    implicit val smartStringEvaluator = new RealSmartStringEvaluatorFactory(messages)(formModelVisibilityOptics)

    val smartStringValues = smartStrings.flatMap { case (smartString, isMarkdown) =>
      if (isMarkdown) {
        List(
          smartString.value(),
          MarkDownUtil.markDownParser(smartString).toString
        )
      } else
        List(smartString.value())
    }
    val answerMap = formModelOptics.formModelVisibilityOptics.freeCalculator.answerMap

    assertEquals(answerMap.pretty(), answerMapExpected.pretty())
    assertEquals(smartStringValues, smartStringValuesExpected)

    Future.successful(Right(()))
  }
}

trait DependencyGraphFixture {

  val authenticatedRetrievals = AuthenticatedRetrievals(
    governmentGatewayId = GovernmentGatewayId("12345"),
    enrolments = Enrolments(Set()),
    affinityGroup = AffinityGroup.Individual,
    groupIdentifier = "aaa",
    maybeNino = None,
    otherRetrievals = OtherRetrievals.empty,
    confidenceLevel = ConfidenceLevel.L50,
    credentialRole = None
  )

  val sicCodeLookupOptions: LocalisedLookupOptions = LocalisedLookupOptions(
    Map(
      LangADT.En -> LookupOptions(
        Map(
          LookupLabel("01220 - Growing of tropical and subtropical fruits") -> SicCodeLookupInfo(
            LookupId("01220"),
            8,
            LookupSicCodeSection("A"),
            Map()
          ),
          LookupLabel("10390 - Other processing and preserving of fruit and vegetables") -> SicCodeLookupInfo(
            LookupId("10390"),
            62,
            LookupSicCodeSection("C"),
            Map()
          ),
          LookupLabel("20110 - Manufacture of industrial gases") -> SicCodeLookupInfo(
            LookupId("20110"),
            145,
            LookupSicCodeSection("C"),
            Map()
          ),
          LookupLabel("46450 - Wholesale of perfume and cosmetics") -> SicCodeLookupInfo(
            LookupId("46450"),
            392,
            LookupSicCodeSection("G"),
            Map()
          )
        )
      )
    )
  )

  val nationalityLookupOptions: LocalisedLookupOptions = LocalisedLookupOptions(
    Map(
      LangADT.En -> LookupOptions(
        Map(
          LookupLabel("Czech") -> NationalityLookupInfo(
            LookupId("N0530"),
            52,
            LookupPriority(1),
            LookupPriority(1),
            LookupRegion("3"),
            LookupCountryCode("CZ"),
            Map("NationalityId" -> "N0530", "CountryCode" -> "CZ")
          ),
          LookupLabel("French") -> NationalityLookupInfo(
            LookupId("N0720"),
            52,
            LookupPriority(1),
            LookupPriority(1),
            LookupRegion("3"),
            LookupCountryCode("FR"),
            Map("NationalityId" -> "N0720", "CountryCode" -> "FR")
          )
        )
      )
    )
  )

  val countryLookupOptions: LocalisedLookupOptions = LocalisedLookupOptions(
    Map(
      LangADT.En -> LookupOptions(
        Map(
          LookupLabel("Czech Republic") -> CountryLookupInfo(
            LookupId("CZ"),
            58,
            LookupPriority(50),
            LookupPriority(50),
            LookupRegion("3"),
            LookupInGibraltarEuEeaEfta("1"),
            Map(
              "InEEA" -> "1"
            )
          ),
          LookupLabel("France") -> CountryLookupInfo(
            LookupId("FR"),
            58,
            LookupPriority(50),
            LookupPriority(50),
            LookupRegion("3"),
            LookupInGibraltarEuEeaEfta("1"),
            Map(
              "InEEA" -> "1"
            )
          ),
          LookupLabel("India") -> CountryLookupInfo(
            LookupId("IN"),
            106,
            LookupPriority(50),
            LookupPriority(50),
            LookupRegion("4"),
            LookupInGibraltarEuEeaEfta("0"),
            Map(
              "InEEA" -> "0"
            )
          )
        )
      )
    )
  )

  val lookupRegistry = new LookupRegistry(
    Map(
      Register.SicCode -> AjaxLookup(
        options = sicCodeLookupOptions,
        searcher = null, // Not used when evaluating expressions
        showAll = ShowAll.Enabled
      ),
      Register.Nationality -> AjaxLookup(
        options = nationalityLookupOptions,
        searcher = null, // Not used when evaluating expressions
        showAll = ShowAll.Enabled
      ),
      Register.Country -> AjaxLookup(
        options = countryLookupOptions,
        searcher = null, // Not used when evaluating expressions
        showAll = ShowAll.Enabled
      )
    )
  )

  def mkDataRetrieves(
    atributes: (String, Map[DataRetrieve.Attribute, String])*
  ): Option[Map[DataRetrieveId, DataRetrieveResult]] =
    Some(atributes.toMap.map { case (dataRetrieveId, atributes) =>
      val failureCount = atributes.get(DataRetrieve.Attribute("failureCount")).map(_.toInt)
      val failureMaxAttempts = atributes.get(DataRetrieve.Attribute("failureMaxAttempts")).map(_.toInt)
      val failureCountResetTime =
        atributes.get(DataRetrieve.Attribute("failureCountResetTime")).map(LocalDateTime.parse)
      DataRetrieveId(dataRetrieveId) ->
        DataRetrieveResult(
          DataRetrieveId(dataRetrieveId),
          RetrieveDataType.ObjectType(atributes),
          Json.obj(),
          failureCount,
          failureMaxAttempts,
          failureCountResetTime
        )
    })

  def mkDataRetrievesList(
    atributes: (String, List[Map[DataRetrieve.Attribute, String]])*
  ): Option[Map[DataRetrieveId, DataRetrieveResult]] =
    Some(atributes.toMap.map { case (dataRetrieveId, atributes) =>
      DataRetrieveId(dataRetrieveId) ->
        DataRetrieveResult(
          DataRetrieveId(dataRetrieveId),
          RetrieveDataType.ListType(atributes),
          Json.obj(),
          None,
          None,
          None
        )
    })

  def mkAddressLookupResult(id: String, lines: List[String], town: String, postcode: String) =
    AddressLookupResult(
      PostcodeLookupRetrieve.Request(postcode, None),
      PostcodeLookupRetrieve.Response(
        filterDisabled = false,
        addresses = Some(
          NonEmptyList.one(
            PostcodeLookupRetrieve.AddressRecord(
              id = id,
              uprn = None,
              address = PostcodeLookupRetrieve.Address(
                lines = lines,
                town = town,
                postcode = postcode,
                subdivision = None,
                country = PostcodeLookupRetrieve.Country(
                  code = "GB",
                  name = "United Kingdom"
                )
              ),
              language = "en",
              localCustodian = None,
              location = None,
              blpuState = None,
              logicalState = None,
              streetClassification = None
            )
          )
        )
      )
    )
}
