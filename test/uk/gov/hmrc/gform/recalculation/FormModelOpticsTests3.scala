/*
 * Copyright 2026 HM Revenue & Customs
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

import org.typelevel.ci.CIString
import uk.gov.hmrc.auth.core.{ ConfidenceLevel, Enrolment, EnrolmentIdentifier, Enrolments, User }
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, EmailRetrievals, GovernmentGatewayId, OtherRetrievals }
import uk.gov.hmrc.gform.models.EmailId
import uk.gov.hmrc.gform.sharedmodel.BooleanExprCache
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormComponentIdToFileIdMapping }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DataSource, FileComponentId, FormComponentId, FormPhase, IdentifierName, InstructionPDF, ServiceName }
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroup, DataRetrieve }
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.{ Many, One }
import uk.gov.hmrc.gform.recalculation.EvaluationStatus._
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData

object FormModelOpticsTests3 extends DependencyGraphFixture {

  val data = List(
    (
      MongoUserData(
        "1_addAnother"         -> Many(List("0")),
        "1_importerDate-day"   -> One("1"),
        "1_importerDate-month" -> One("1"),
        "1_importerDate-year"  -> One("2020"),
        "1_note"               -> One("Note 1"),
        "1_noteSwapped"        -> One("Note Swapped 1"),
        "2_addAnother"         -> Many(List("0")),
        "2_importerDate-day"   -> One("12"),
        "2_importerDate-month" -> One("12"),
        "2_importerDate-year"  -> One("2025"),
        "2_note"               -> One("Note 2"),
        "2_noteSwapped"        -> One("Note Swapped 2"),
        "3_addAnother"         -> Many(List("1")),
        "3_importerDate-day"   -> One("7"),
        "3_importerDate-month" -> One("7"),
        "3_importerDate-year"  -> One("2026"),
        "3_note"               -> One("Note 3"),
        "3_noteSwapped"        -> One("Note Swapped 3")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ap0.1.2",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ap0.2.2",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1",
        "ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_importerDate-day"   -> NumberResult(1),
        "1_importerDate-month" -> NumberResult(1),
        "1_importerDate-year"  -> NumberResult(2020),
        "1_note"               -> StringResult("Note 1"),
        "1_noteSwapped"        -> StringResult("Note Swapped 1"),
        "2_importerDate-day"   -> NumberResult(12),
        "2_importerDate-month" -> NumberResult(12),
        "2_importerDate-year"  -> NumberResult(2025),
        "2_note"               -> StringResult("Note 2"),
        "2_noteSwapped"        -> StringResult("Note Swapped 2"),
        "3_importerDate-day"   -> NumberResult(7),
        "3_importerDate-month" -> NumberResult(7),
        "3_importerDate-year"  -> NumberResult(2026),
        "3_note"               -> Hidden,
        "3_noteSwapped"        -> Hidden
      ),
      List(
        "1 - 1 January 2020 - Note 1 - Note Swapped 1 - Yes - Yes.",
        "<p>1 - 1 January 2020 - Note 1 - Note Swapped 1 - Yes - Yes.</p>",
        "2 - 12 December 2025 - Note 2 - Note Swapped 2 - Yes - Yes.",
        "<p>2 - 12 December 2025 - Note 2 - Note Swapped 2 - Yes - Yes.</p>",
        "3 - 7 July 2026 -  -  - No - No.",
        "<p>3 - 7 July 2026 - - - No - No.</p>",
        "importerDate: 1 January 2020, 12 December 2025, 7 July 2026",
        "effectiveDate: 6 June 2023, 12 December 2025, 7 July 2026",
        "effectiveDateSwapped: 6 June 2023, 12 December 2025, 7 July 2026",
        "note: Note 1, Note 2",
        "note swapped: Note Swapped 1, Note Swapped 2",
        "effectiveDateBefore06062026: Yes, Yes, No",
        "effectiveDateBefore06062026Swapped: Yes, Yes, No"
      ),
      "date-expr-if-else-with-before-atl.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"         -> Many(List("0")),
        "1_importerDate-day"   -> One("7"),
        "1_importerDate-month" -> One("7"),
        "1_importerDate-year"  -> One("2025"),
        "1_note"               -> One("Note 1"),
        "1_noteSwapped"        -> One("Note Swapped 1"),
        "2_addAnother"         -> Many(List("0")),
        "2_importerDate-day"   -> One("1"),
        "2_importerDate-month" -> One("1"),
        "2_importerDate-year"  -> One("2025"),
        "2_note"               -> One("Note 2"),
        "2_noteSwapped"        -> One("Note Swapped 2"),
        "3_addAnother"         -> Many(List("1")),
        "3_importerDate-day"   -> One("1"),
        "3_importerDate-month" -> One("1"),
        "3_importerDate-year"  -> One("2020"),
        "3_note"               -> One("Note 3"),
        "3_noteSwapped"        -> One("Note Swapped 3")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ap0.1.2",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ap0.2.2",
        "ar0.2",
        "ap0.3.0",
        "ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_importerDate-day"   -> NumberResult(7),
        "1_importerDate-month" -> NumberResult(7),
        "1_importerDate-year"  -> NumberResult(2025),
        "1_note"               -> StringResult("Note 1"),
        "1_noteSwapped"        -> StringResult("Note Swapped 1"),
        "2_importerDate-day"   -> NumberResult(1),
        "2_importerDate-month" -> NumberResult(1),
        "2_importerDate-year"  -> NumberResult(2025),
        "2_note"               -> StringResult("Note 2"),
        "2_noteSwapped"        -> StringResult("Note Swapped 2"),
        "3_importerDate-day"   -> NumberResult(1),
        "3_importerDate-month" -> NumberResult(1),
        "3_importerDate-year"  -> NumberResult(2020),
        "3_note"               -> Hidden,
        "3_noteSwapped"        -> Hidden
      ),
      List(
        "1 - 7 July 2025 - Note 1 - Note Swapped 1 - Yes - Yes.",
        "<p>1 - 7 July 2025 - Note 1 - Note Swapped 1 - Yes - Yes.</p>",
        "2 - 1 January 2025 - Note 2 - Note Swapped 2 - Yes - Yes.",
        "<p>2 - 1 January 2025 - Note 2 - Note Swapped 2 - Yes - Yes.</p>",
        "3 - 1 January 2020 -  -  - No - No.",
        "<p>3 - 1 January 2020 - - - No - No.</p>",
        "importerDate: 7 July 2025, 1 January 2025, 1 January 2020",
        "effectiveDate: 6 June 2023, 1 January 2025, 1 January 2020",
        "effectiveDateSwapped: 6 June 2023, 1 January 2025, 1 January 2020",
        "note: Note 1, Note 2",
        "note swapped: Note Swapped 1, Note Swapped 2",
        "effectiveDateAfter06062021: Yes, Yes, No",
        "effectiveDateAfter06062021Swapped: Yes, Yes, No"
      ),
      "date-expr-if-else-with-after-atl.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"                -> Many(List("0")),
        "1_lossSurrendered"           -> One("435546"),
        "1_productionStartDate-day"   -> One("1"),
        "1_productionStartDate-month" -> One("1"),
        "1_productionStartDate-year"  -> One("2011"),
        "2_addAnother"                -> Many(List("0")),
        "2_lossSurrendered"           -> One("1234"),
        "2_productionStartDate-day"   -> One("1"),
        "2_productionStartDate-month" -> One("1"),
        "2_productionStartDate-year"  -> One("2025"),
        "3_addAnother"                -> Many(List("1")),
        "3_lossSurrendered"           -> One("56"),
        "3_productionStartDate-day"   -> One("5"),
        "3_productionStartDate-month" -> One("5"),
        "3_productionStartDate-year"  -> One("2020"),
        "accountingStart-day"         -> One("12"),
        "accountingStart-month"       -> One("12"),
        "accountingStart-year"        -> One("2023")
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_lossSurrendered"           -> NumberResult(435546),
        "1_productionStartDate-day"   -> NumberResult(1),
        "1_productionStartDate-month" -> NumberResult(1),
        "1_productionStartDate-year"  -> NumberResult(2011),
        "2_lossSurrendered"           -> NumberResult(1234),
        "2_productionStartDate-day"   -> NumberResult(1),
        "2_productionStartDate-month" -> NumberResult(1),
        "2_productionStartDate-year"  -> NumberResult(2025),
        "3_lossSurrendered"           -> NumberResult(56),
        "3_productionStartDate-day"   -> NumberResult(5),
        "3_productionStartDate-month" -> NumberResult(5),
        "3_productionStartDate-year"  -> NumberResult(2020),
        "accountingStart-day"         -> NumberResult(12),
        "accountingStart-month"       -> NumberResult(12),
        "accountingStart-year"        -> NumberResult(2023),
        "secretAnd"                   -> Empty,
        "secretAndSwapped"            -> Empty,
        "secretOr"                    -> Hidden,
        "secretOrSwapped"             -> Hidden
      ),
      List(
        "1 - 1 January 2011 - £435,546",
        "<p>1 - 1 January 2011 - £435,546</p>",
        "2 - 1 January 2025 - £1,234",
        "<p>2 - 1 January 2025 - £1,234</p>",
        "3 - 5 May 2020 - £56",
        "<p>3 - 5 May 2020 - £56</p>",
        "creditTotalAnd = £87,737.40",
        "creditTotalAndSwapped = £87,737.40",
        "creditTotalOr = £87,367.20",
        "creditTotalOrSwapped = £87,367.20",
        "creditCalculationAnd = £87,109.20, £617.00, £11.20",
        "creditCalculationAndSwapped = £87,109.20, £617.00, £11.20",
        "creditCalculationOr = £87,109.20, £246.80, £11.20",
        "creditCalculationOrSwapped = £87,109.20, £246.80, £11.20",
        "loss20 = £87,109.20, £246.80, £11.20",
        "loss50 = £217,773.00, £617.00, £28.00",
        "productionStartDate = 1 January 2011, 1 January 2025, 5 May 2020",
        "accountingStartBefore01042025 = Yes",
        "productionStartBefore27102021 = Yes, No, Yes",
        "lossSurrenderedAt20perCentAnd = Yes, No, Yes",
        "lossSurrenderedAt20perCentAndSwapped = Yes, No, Yes",
        "lossSurrenderedAt20perCentOr = Yes, Yes, Yes",
        "lossSurrenderedAt20perCentOrSwapped = Yes, Yes, Yes"
      ),
      "boolean-expr-literal-and-list.json accounting YES"
    ),
    (
      MongoUserData(
        "1_addAnother"                -> Many(List("0")),
        "1_lossSurrendered"           -> One("435546"),
        "1_productionStartDate-day"   -> One("1"),
        "1_productionStartDate-month" -> One("1"),
        "1_productionStartDate-year"  -> One("2011"),
        "2_addAnother"                -> Many(List("0")),
        "2_lossSurrendered"           -> One("1234"),
        "2_productionStartDate-day"   -> One("1"),
        "2_productionStartDate-month" -> One("1"),
        "2_productionStartDate-year"  -> One("2025"),
        "3_addAnother"                -> Many(List("1")),
        "3_lossSurrendered"           -> One("56"),
        "3_productionStartDate-day"   -> One("5"),
        "3_productionStartDate-month" -> One("5"),
        "3_productionStartDate-year"  -> One("2020"),
        "accountingStart-day"         -> One("12"),
        "accountingStart-month"       -> One("12"),
        "accountingStart-year"        -> One("2026")
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_lossSurrendered"           -> NumberResult(435546),
        "1_productionStartDate-day"   -> NumberResult(1),
        "1_productionStartDate-month" -> NumberResult(1),
        "1_productionStartDate-year"  -> NumberResult(2011),
        "2_lossSurrendered"           -> NumberResult(1234),
        "2_productionStartDate-day"   -> NumberResult(1),
        "2_productionStartDate-month" -> NumberResult(1),
        "2_productionStartDate-year"  -> NumberResult(2025),
        "3_lossSurrendered"           -> NumberResult(56),
        "3_productionStartDate-day"   -> NumberResult(5),
        "3_productionStartDate-month" -> NumberResult(5),
        "3_productionStartDate-year"  -> NumberResult(2020),
        "accountingStart-day"         -> NumberResult(12),
        "accountingStart-month"       -> NumberResult(12),
        "accountingStart-year"        -> NumberResult(2026),
        "secretAnd"                   -> Hidden,
        "secretAndSwapped"            -> Hidden,
        "secretOr"                    -> Empty,
        "secretOrSwapped"             -> Empty
      ),
      List(
        "1 - 1 January 2011 - £435,546",
        "<p>1 - 1 January 2011 - £435,546</p>",
        "2 - 1 January 2025 - £1,234",
        "<p>2 - 1 January 2025 - £1,234</p>",
        "3 - 5 May 2020 - £56",
        "<p>3 - 5 May 2020 - £56</p>",
        "creditTotalAnd = £218,418.00",
        "creditTotalAndSwapped = £218,418.00",
        "creditTotalOr = £87,737.40",
        "creditTotalOrSwapped = £87,737.40",
        "creditCalculationAnd = £217,773.00, £617.00, £28.00",
        "creditCalculationAndSwapped = £217,773.00, £617.00, £28.00",
        "creditCalculationOr = £87,109.20, £617.00, £11.20",
        "creditCalculationOrSwapped = £87,109.20, £617.00, £11.20",
        "loss20 = £87,109.20, £246.80, £11.20",
        "loss50 = £217,773.00, £617.00, £28.00",
        "productionStartDate = 1 January 2011, 1 January 2025, 5 May 2020",
        "accountingStartBefore01042025 = No",
        "productionStartBefore27102021 = Yes, No, Yes",
        "lossSurrenderedAt20perCentAnd = No, No, No",
        "lossSurrenderedAt20perCentAndSwapped = No, No, No",
        "lossSurrenderedAt20perCentOr = Yes, No, Yes",
        "lossSurrenderedAt20perCentOrSwapped = Yes, No, Yes"
      ),
      "boolean-expr-literal-and-list.json accounting NO"
    ),
    (
      MongoUserData(
        "1_coreExpenditure" -> One("1,000,000"),
        "1_deductionToDate" -> One("1,000,000")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_deductionToDate" -> NumberResult(1000000)
      ),
      List(
        "FOO",
        "<p>FOO</p>"
      ),
      "sterling-handling.json equals"
    ),
    (
      MongoUserData(
        "1_coreExpenditure" -> One("1,000,000"),
        "1_deductionToDate" -> One("1,000,001")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_deductionToDate" -> NumberResult(1000001)
      ),
      List(
        "BAR",
        "<p>BAR</p>"
      ),
      "sterling-handling.json less than"
    ),
    (
      MongoUserData(
        "1_addAnotherFilm"   -> Many(List("1")),
        "1_coreExpenditure"  -> One("100000"),
        "1_filmName"         -> One("Film A"),
        "1_totalExpenditure" -> One("500000")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1 - Film A :: £500,000 :: £100,000",
        "<p>1 - Film A :: £500,000 :: £100,000</p>",
        "Film A",
        "£100,000",
        "Total <=",
        "£100,000",
        "Total <",
        "£100,000",
        "Total >=",
        "£500,000",
        "Total >",
        "£500,000",
        "deductionLessThanOrEquals: £100,000",
        "deductionLessThan: £100,000",
        "deductionGreaterThanOrEquals: £500,000",
        "deductionGreaterThan: £500,000",
        "Debug: £100,000 <= £500,000 then £100,000 else £500,000"
      ),
      "atl-data-in-table.json one film"
    ),
    (
      MongoUserData(
        "1_addAnotherFilm"   -> Many(List("0")),
        "1_coreExpenditure"  -> One("100000"),
        "1_filmName"         -> One("Film A"),
        "1_totalExpenditure" -> One("500000"),
        "2_addAnotherFilm"   -> Many(List("1")),
        "2_coreExpenditure"  -> One("500000"),
        "2_filmName"         -> One("Film B"),
        "2_totalExpenditure" -> One("100000")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1 - Film A :: £500,000 :: £100,000",
        "<p>1 - Film A :: £500,000 :: £100,000</p>",
        "2 - Film B :: £100,000 :: £500,000",
        "<p>2 - Film B :: £100,000 :: £500,000</p>",
        "Film A",
        "£100,000",
        "Film B",
        "£100,000",
        "Total <=",
        "£200,000",
        "Total <",
        "£200,000",
        "Total >=",
        "£1,000,000",
        "Total >",
        "£1,000,000",
        "deductionLessThanOrEquals: £100,000, £100,000",
        "deductionLessThan: £100,000, £100,000",
        "deductionGreaterThanOrEquals: £500,000, £500,000",
        "deductionGreaterThan: £500,000, £500,000",
        "Debug: £100,000, £500,000 <= £500,000, £100,000 then £100,000, £500,000 else £500,000, £100,000"
      ),
      "atl-data-in-table.json two films"
    ),
    (
      MongoUserData(
        "1_addAnotherFilm"   -> Many(List("0")),
        "1_coreExpenditure"  -> One("500000"),
        "1_filmName"         -> One("Film A"),
        "1_totalExpenditure" -> One("100000"),
        "2_addAnotherFilm"   -> Many(List("1")),
        "2_coreExpenditure"  -> One("100000"),
        "2_filmName"         -> One("Film B"),
        "2_totalExpenditure" -> One("500000")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1 - Film A :: £100,000 :: £500,000",
        "<p>1 - Film A :: £100,000 :: £500,000</p>",
        "2 - Film B :: £500,000 :: £100,000",
        "<p>2 - Film B :: £500,000 :: £100,000</p>",
        "Film A",
        "£100,000",
        "Film B",
        "£100,000",
        "Total <=",
        "£200,000",
        "Total <",
        "£200,000",
        "Total >=",
        "£1,000,000",
        "Total >",
        "£1,000,000",
        "deductionLessThanOrEquals: £100,000, £100,000",
        "deductionLessThan: £100,000, £100,000",
        "deductionGreaterThanOrEquals: £500,000, £500,000",
        "deductionGreaterThan: £500,000, £500,000",
        "Debug: £500,000, £100,000 <= £100,000, £500,000 then £500,000, £100,000 else £100,000, £500,000"
      ),
      "atl-data-in-table.json two films data inverted "
    ),
    (
      MongoUserData(
        "overpayment" -> One("1000")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "Overpayment total is: £1,000",
        "Overpayment is: £1,000"
      ),
      "sum-of-non-repeated-field.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"           -> Many(List("0")),
        "1_personLocationChoice" -> Many(List("0")),
        "1_personName"           -> One("AAA"),
        "1_personNino"           -> One("CC111111C"),
        "2_personLocationChoice" -> Many(List("0")),
        "2_personName"           -> One("BBB"),
        "2_personNino"           -> One("SE101010A")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ap0.1.2",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ap0.2.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_personChangeOrRemoveChoice" -> Hidden,
        "1_personLocationChoice"       -> OptionResult(List("0")),
        "1_personNationalId"           -> Hidden,
        "1_personNino"                 -> StringResult("CC111111C"),
        "2_personChangeOrRemoveChoice" -> Hidden,
        "2_personLocationChoice"       -> OptionResult(List("0")),
        "2_personNationalId"           -> Hidden,
        "2_personNino"                 -> StringResult("SE101010A")
      ),
      List(
        "AAA",
        "1 - AAA - National Insurance number: CC111111C",
        "<p>1 - AAA - National Insurance number: CC111111C</p>",
        "BBB",
        "2 - BBB - National Insurance number: SE101010A",
        "<p>2 - BBB - National Insurance number: SE101010A</p>"
      ),
      "duplicate-exists-or-duplicate-exists.json Generated"
    ),
    (
      MongoUserData(
        "businessType" -> Many(List("0")),
        "1_director"   -> One("John"),
        "1_directorDOB" -> One(
          ""
        ), // This should not be in the data, but unfortunately it is. So there is special logic in the expression evaluation to ignore it.
        "1_directorDOB-day"   -> One("1"),
        "1_directorDOB-month" -> One("1"),
        "1_directorDOB-year"  -> One("2000")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_director"          -> StringResult("John"),
        "1_directorDOB-day"   -> NumberResult(1),
        "1_directorDOB-month" -> NumberResult(1),
        "1_directorDOB-year"  -> NumberResult(2000),
        "businessType"        -> OptionResult(List("0"))
      ),
      List.empty[String],
      "group-with-date.json Generated"
    ),
    (
      MongoUserData(
        "1_repeating"       -> One("1"),
        "2_repeating"       -> One("2"),
        "repeatsNumber"     -> One("2"),
        "showRepeatingPage" -> Many(List("0"))
      ),
      List(
        "n0",
        "n1",
        "r2.0",
        "r2.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_repeating"       -> NumberResult(1),
        "2_repeating"       -> NumberResult(2),
        "repeatsNumber"     -> NumberResult(2),
        "showRepeatingPage" -> OptionResult(List("0"))
      ),
      List(
        "repeating.sum : 3",
        "<p>repeating.sum : 3</p>"
      ),
      "repeated-section-with-include-if-2.json repeating section is visible"
    ),
    (
      MongoUserData(
        "1_repeating"       -> One("1"),
        "2_repeating"       -> One("2"),
        "repeatsNumber"     -> One("2"),
        "showRepeatingPage" -> Many(List("1"))
      ),
      List(
        "n0",
        "n1",
        "r2.0",
        "r2.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_repeating"       -> Hidden,
        "2_repeating"       -> Hidden,
        "repeatsNumber"     -> Hidden,
        "showRepeatingPage" -> OptionResult(List("1"))
      ),
      List(
        "repeating.sum : 0",
        "<p>repeating.sum : 0</p>"
      ),
      "repeated-section-with-include-if-2.json repeating section is hidden"
    ),
    (
      MongoUserData(
        "number"   -> One("150"),
        "textBoxC" -> One("CCC"),
        "textBoxF" -> One("FFF"),
        "textBoxG" -> One("GGG"),
        "textBoxH" -> One("HHH"),
        "textBoxI" -> One("III")
      ),
      List(
        "n0",
        "n3",
        "n6",
        "n7",
        "n8",
        "n9"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "number"   -> NumberResult(150),
        "textBoxA" -> Hidden,
        "textBoxB" -> Hidden,
        "textBoxC" -> Hidden,
        "textBoxD" -> Hidden,
        "textBoxE" -> StringResult("EEE"),
        "textBoxF" -> StringResult("FFF"),
        "textBoxG" -> StringResult("GGG"),
        "textBoxH" -> StringResult("HHH"),
        "textBoxI" -> Hidden,
        "textBoxJ" -> Hidden
      ),
      List(
        "number: 150",
        "textBoxA: ",
        "textBoxB: ",
        "textBoxC: ",
        "textBoxD: ",
        "textBoxE: EEE",
        "textBoxF: FFF",
        "textBoxG: GGG",
        "textBoxH: HHH",
        "textBoxI: ",
        "textBoxJ: ",
        "Is textBoxA visible: No : sixteenUp < number * 0.1",
        "Is textBoxB visible: No : sixteenDown < number * 0.1",
        "Is textBoxC visible: No : sixteenUp <= number * 0.1",
        "Is textBoxD visible: No : sixteenDown <= number * 0.1",
        "Is textBoxE visible: Yes : sixteenUp > number * 0.1",
        "Is textBoxF visible: Yes : sixteenDown > number * 0.1",
        "Is textBoxG visible: Yes : sixteenUp >= number * 0.1",
        "Is textBoxH visible: Yes : sixteenDown >= number * 0.1",
        "Is textBoxI visible: No : sixteenUp = number * 0.1",
        "Is textBoxJ visible: No : sixteenDown = number * 0.1",
        "number * 0.1 : 15"
      ),
      "text-number-rounding-rhs.json 150"
    ),
    (
      MongoUserData(
        "number"   -> One("151"),
        "textBoxC" -> One("CCC"),
        "textBoxF" -> One("FFF"),
        "textBoxG" -> One("GGG"),
        "textBoxH" -> One("HHH"),
        "textBoxI" -> One("III")
      ),
      List(
        "n0",
        "n3",
        "n6",
        "n7",
        "n8",
        "n9"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "number"   -> NumberResult(151),
        "textBoxA" -> Hidden,
        "textBoxB" -> Hidden,
        "textBoxC" -> StringResult("CCC"),
        "textBoxD" -> Hidden,
        "textBoxE" -> Hidden,
        "textBoxF" -> StringResult("FFF"),
        "textBoxG" -> StringResult("GGG"),
        "textBoxH" -> StringResult("HHH"),
        "textBoxI" -> StringResult("III"),
        "textBoxJ" -> Hidden
      ),
      List(
        "number: 151",
        "textBoxA: ",
        "textBoxB: ",
        "textBoxC: CCC",
        "textBoxD: ",
        "textBoxE: ",
        "textBoxF: FFF",
        "textBoxG: GGG",
        "textBoxH: HHH",
        "textBoxI: III",
        "textBoxJ: ",
        "Is textBoxA visible: No : sixteenUp < number * 0.1",
        "Is textBoxB visible: No : sixteenDown < number * 0.1",
        "Is textBoxC visible: Yes : sixteenUp <= number * 0.1",
        "Is textBoxD visible: No : sixteenDown <= number * 0.1",
        "Is textBoxE visible: No : sixteenUp > number * 0.1",
        "Is textBoxF visible: Yes : sixteenDown > number * 0.1",
        "Is textBoxG visible: Yes : sixteenUp >= number * 0.1",
        "Is textBoxH visible: Yes : sixteenDown >= number * 0.1",
        "Is textBoxI visible: Yes : sixteenUp = number * 0.1",
        "Is textBoxJ visible: No : sixteenDown = number * 0.1",
        "number * 0.1 : 15.1"
      ),
      "text-number-rounding-rhs.json 151"
    ),
    (
      MongoUserData(
        "number"   -> One("160"),
        "textBoxC" -> One("CCC"),
        "textBoxF" -> One("FFF"),
        "textBoxG" -> One("GGG"),
        "textBoxH" -> One("HHH"),
        "textBoxI" -> One("III")
      ),
      List(
        "n0",
        "n3",
        "n6",
        "n7",
        "n8",
        "n9"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "number"   -> NumberResult(160),
        "textBoxA" -> Hidden,
        "textBoxB" -> Hidden,
        "textBoxC" -> StringResult("CCC"),
        "textBoxD" -> StringResult("DDD"),
        "textBoxE" -> Hidden,
        "textBoxF" -> Hidden,
        "textBoxG" -> StringResult("GGG"),
        "textBoxH" -> StringResult("HHH"),
        "textBoxI" -> StringResult("III"),
        "textBoxJ" -> StringResult("JJJ")
      ),
      List(
        "number: 160",
        "textBoxA: ",
        "textBoxB: ",
        "textBoxC: CCC",
        "textBoxD: DDD",
        "textBoxE: ",
        "textBoxF: ",
        "textBoxG: GGG",
        "textBoxH: HHH",
        "textBoxI: III",
        "textBoxJ: JJJ",
        "Is textBoxA visible: No : sixteenUp < number * 0.1",
        "Is textBoxB visible: No : sixteenDown < number * 0.1",
        "Is textBoxC visible: Yes : sixteenUp <= number * 0.1",
        "Is textBoxD visible: Yes : sixteenDown <= number * 0.1",
        "Is textBoxE visible: No : sixteenUp > number * 0.1",
        "Is textBoxF visible: No : sixteenDown > number * 0.1",
        "Is textBoxG visible: Yes : sixteenUp >= number * 0.1",
        "Is textBoxH visible: Yes : sixteenDown >= number * 0.1",
        "Is textBoxI visible: Yes : sixteenUp = number * 0.1",
        "Is textBoxJ visible: Yes : sixteenDown = number * 0.1",
        "number * 0.1 : 16"
      ),
      "text-number-rounding-rhs.json 160"
    ),
    (
      MongoUserData(
        "number"   -> One("161"),
        "textBoxC" -> One("CCC"),
        "textBoxF" -> One("FFF"),
        "textBoxG" -> One("GGG"),
        "textBoxH" -> One("HHH"),
        "textBoxI" -> One("III")
      ),
      List(
        "n0",
        "n3",
        "n6",
        "n7",
        "n8",
        "n9"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "number"   -> NumberResult(161),
        "textBoxA" -> StringResult("AAA"),
        "textBoxB" -> Hidden,
        "textBoxC" -> StringResult("CCC"),
        "textBoxD" -> StringResult("DDD"),
        "textBoxE" -> Hidden,
        "textBoxF" -> Hidden,
        "textBoxG" -> Hidden,
        "textBoxH" -> StringResult("HHH"),
        "textBoxI" -> Hidden,
        "textBoxJ" -> StringResult("JJJ")
      ),
      List(
        "number: 161",
        "textBoxA: AAA",
        "textBoxB: ",
        "textBoxC: CCC",
        "textBoxD: DDD",
        "textBoxE: ",
        "textBoxF: ",
        "textBoxG: ",
        "textBoxH: HHH",
        "textBoxI: ",
        "textBoxJ: JJJ",
        "Is textBoxA visible: Yes : sixteenUp < number * 0.1",
        "Is textBoxB visible: No : sixteenDown < number * 0.1",
        "Is textBoxC visible: Yes : sixteenUp <= number * 0.1",
        "Is textBoxD visible: Yes : sixteenDown <= number * 0.1",
        "Is textBoxE visible: No : sixteenUp > number * 0.1",
        "Is textBoxF visible: No : sixteenDown > number * 0.1",
        "Is textBoxG visible: No : sixteenUp >= number * 0.1",
        "Is textBoxH visible: Yes : sixteenDown >= number * 0.1",
        "Is textBoxI visible: No : sixteenUp = number * 0.1",
        "Is textBoxJ visible: Yes : sixteenDown = number * 0.1",
        "number * 0.1 : 16.1"
      ),
      "text-number-rounding-rhs.json 161"
    ),
    (
      MongoUserData(
        "number"   -> One("170"),
        "textBoxC" -> One("CCC"),
        "textBoxF" -> One("FFF"),
        "textBoxG" -> One("GGG"),
        "textBoxH" -> One("HHH"),
        "textBoxI" -> One("III")
      ),
      List(
        "n0",
        "n3",
        "n6",
        "n7",
        "n8",
        "n9"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "number"   -> NumberResult(170),
        "textBoxA" -> StringResult("AAA"),
        "textBoxB" -> StringResult("BBB"),
        "textBoxC" -> StringResult("CCC"),
        "textBoxD" -> StringResult("DDD"),
        "textBoxE" -> Hidden,
        "textBoxF" -> Hidden,
        "textBoxG" -> Hidden,
        "textBoxH" -> Hidden,
        "textBoxI" -> Hidden,
        "textBoxJ" -> Hidden
      ),
      List(
        "number: 170",
        "textBoxA: AAA",
        "textBoxB: BBB",
        "textBoxC: CCC",
        "textBoxD: DDD",
        "textBoxE: ",
        "textBoxF: ",
        "textBoxG: ",
        "textBoxH: ",
        "textBoxI: ",
        "textBoxJ: ",
        "Is textBoxA visible: Yes : sixteenUp < number * 0.1",
        "Is textBoxB visible: Yes : sixteenDown < number * 0.1",
        "Is textBoxC visible: Yes : sixteenUp <= number * 0.1",
        "Is textBoxD visible: Yes : sixteenDown <= number * 0.1",
        "Is textBoxE visible: No : sixteenUp > number * 0.1",
        "Is textBoxF visible: No : sixteenDown > number * 0.1",
        "Is textBoxG visible: No : sixteenUp >= number * 0.1",
        "Is textBoxH visible: No : sixteenDown >= number * 0.1",
        "Is textBoxI visible: No : sixteenUp = number * 0.1",
        "Is textBoxJ visible: No : sixteenDown = number * 0.1",
        "number * 0.1 : 17"
      ),
      "text-number-rounding-rhs.json 170"
    ),
    (
      MongoUserData(
        "numberA"  -> One("150"),
        "numberB"  -> One("150"),
        "textBoxA" -> One("AAA"),
        "textBoxB" -> One("BBB"),
        "textBoxC" -> One("CCC"),
        "textBoxD" -> One("DDD"),
        "textBoxE" -> One("EEE"),
        "textBoxF" -> One("FFF"),
        "textBoxG" -> One("GGG"),
        "textBoxH" -> One("HHH"),
        "textBoxI" -> One("III"),
        "textBoxJ" -> One("JJJ")
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3",
        "n4",
        "n5",
        "n6",
        "n7",
        "n8",
        "n9",
        "n10"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "numberA"  -> NumberResult(150),
        "numberB"  -> NumberResult(150),
        "textBoxA" -> StringResult("AAA"),
        "textBoxB" -> StringResult("BBB"),
        "textBoxC" -> StringResult("CCC"),
        "textBoxD" -> StringResult("DDD"),
        "textBoxE" -> Hidden,
        "textBoxF" -> Hidden,
        "textBoxG" -> Hidden,
        "textBoxH" -> Hidden,
        "textBoxI" -> Hidden,
        "textBoxJ" -> Hidden
      ),
      List(
        "numberA - Down: 150",
        "numberB - Up: 150",
        "textBoxA: AAA",
        "textBoxB: BBB",
        "textBoxC: CCC",
        "textBoxD: DDD",
        "textBoxE: ",
        "textBoxF: ",
        "textBoxG: ",
        "textBoxH: ",
        "textBoxI: ",
        "textBoxJ: ",
        "Is textBoxA visible: Yes : numberA * 0.1 < 16",
        "Is textBoxB visible: Yes : numberB * 0.1 < 16",
        "Is textBoxC visible: Yes : numberA * 0.1 <= 16",
        "Is textBoxD visible: Yes : numberB * 0.1 <= 16",
        "Is textBoxE visible: No : numberA * 0.1 > 16",
        "Is textBoxF visible: No : numberB * 0.1 > 16",
        "Is textBoxG visible: No : numberA * 0.1 >= 16",
        "Is textBoxH visible: No : numberB * 0.1 >= 16",
        "Is textBoxI visible: No : numberA * 0.1 = 16",
        "Is textBoxJ visible: No : numberB * 0.1 = 16",
        "numberA * 0.1: 15",
        "numberB * 0.1: 15",
        "numberA * 0.123: 18",
        "numberB * 0.123: 19",
        "0.123 * numberA: 18.45",
        "0.123 * numberB: 18.45"
      ),
      "text-number-rounding-lhs.json 150"
    ),
    (
      MongoUserData(
        "numberA"  -> One("151"),
        "numberB"  -> One("151"),
        "textBoxA" -> One("AAA"),
        "textBoxB" -> One("BBB"),
        "textBoxC" -> One("CCC"),
        "textBoxD" -> One("DDD"),
        "textBoxE" -> One("EEE"),
        "textBoxF" -> One("FFF"),
        "textBoxG" -> One("GGG"),
        "textBoxH" -> One("HHH"),
        "textBoxI" -> One("III"),
        "textBoxJ" -> One("JJJ")
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3",
        "n4",
        "n5",
        "n6",
        "n7",
        "n8",
        "n9",
        "n10"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "numberA"  -> NumberResult(151),
        "numberB"  -> NumberResult(151),
        "textBoxA" -> StringResult("AAA"),
        "textBoxB" -> Hidden,
        "textBoxC" -> StringResult("CCC"),
        "textBoxD" -> StringResult("DDD"),
        "textBoxE" -> Hidden,
        "textBoxF" -> Hidden,
        "textBoxG" -> Hidden,
        "textBoxH" -> StringResult("HHH"),
        "textBoxI" -> Hidden,
        "textBoxJ" -> StringResult("JJJ")
      ),
      List(
        "numberA - Down: 151",
        "numberB - Up: 151",
        "textBoxA: AAA",
        "textBoxB: ",
        "textBoxC: CCC",
        "textBoxD: DDD",
        "textBoxE: ",
        "textBoxF: ",
        "textBoxG: ",
        "textBoxH: HHH",
        "textBoxI: ",
        "textBoxJ: JJJ",
        "Is textBoxA visible: Yes : numberA * 0.1 < 16",
        "Is textBoxB visible: No : numberB * 0.1 < 16",
        "Is textBoxC visible: Yes : numberA * 0.1 <= 16",
        "Is textBoxD visible: Yes : numberB * 0.1 <= 16",
        "Is textBoxE visible: No : numberA * 0.1 > 16",
        "Is textBoxF visible: No : numberB * 0.1 > 16",
        "Is textBoxG visible: No : numberA * 0.1 >= 16",
        "Is textBoxH visible: Yes : numberB * 0.1 >= 16",
        "Is textBoxI visible: No : numberA * 0.1 = 16",
        "Is textBoxJ visible: Yes : numberB * 0.1 = 16",
        "numberA * 0.1: 15",
        "numberB * 0.1: 16",
        "numberA * 0.123: 18",
        "numberB * 0.123: 19",
        "0.123 * numberA: 18.57",
        "0.123 * numberB: 18.57"
      ),
      "text-number-rounding-lhs.json 151"
    ),
    (
      MongoUserData(
        "numberA"  -> One("160"),
        "numberB"  -> One("160"),
        "textBoxA" -> One("AAA"),
        "textBoxB" -> One("BBB"),
        "textBoxC" -> One("CCC"),
        "textBoxD" -> One("DDD"),
        "textBoxE" -> One("EEE"),
        "textBoxF" -> One("FFF"),
        "textBoxG" -> One("GGG"),
        "textBoxH" -> One("HHH"),
        "textBoxI" -> One("III"),
        "textBoxJ" -> One("JJJ")
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3",
        "n4",
        "n5",
        "n6",
        "n7",
        "n8",
        "n9",
        "n10"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "numberA"  -> NumberResult(160),
        "numberB"  -> NumberResult(160),
        "textBoxA" -> Hidden,
        "textBoxB" -> Hidden,
        "textBoxC" -> StringResult("CCC"),
        "textBoxD" -> StringResult("DDD"),
        "textBoxE" -> Hidden,
        "textBoxF" -> Hidden,
        "textBoxG" -> StringResult("GGG"),
        "textBoxH" -> StringResult("HHH"),
        "textBoxI" -> StringResult("III"),
        "textBoxJ" -> StringResult("JJJ")
      ),
      List(
        "numberA - Down: 160",
        "numberB - Up: 160",
        "textBoxA: ",
        "textBoxB: ",
        "textBoxC: CCC",
        "textBoxD: DDD",
        "textBoxE: ",
        "textBoxF: ",
        "textBoxG: GGG",
        "textBoxH: HHH",
        "textBoxI: III",
        "textBoxJ: JJJ",
        "Is textBoxA visible: No : numberA * 0.1 < 16",
        "Is textBoxB visible: No : numberB * 0.1 < 16",
        "Is textBoxC visible: Yes : numberA * 0.1 <= 16",
        "Is textBoxD visible: Yes : numberB * 0.1 <= 16",
        "Is textBoxE visible: No : numberA * 0.1 > 16",
        "Is textBoxF visible: No : numberB * 0.1 > 16",
        "Is textBoxG visible: Yes : numberA * 0.1 >= 16",
        "Is textBoxH visible: Yes : numberB * 0.1 >= 16",
        "Is textBoxI visible: Yes : numberA * 0.1 = 16",
        "Is textBoxJ visible: Yes : numberB * 0.1 = 16",
        "numberA * 0.1: 16",
        "numberB * 0.1: 16",
        "numberA * 0.123: 19",
        "numberB * 0.123: 20",
        "0.123 * numberA: 19.68",
        "0.123 * numberB: 19.68"
      ),
      "text-number-rounding-lhs.json 160"
    ),
    (
      MongoUserData(
        "numberA"  -> One("161"),
        "numberB"  -> One("161"),
        "textBoxA" -> One("AAA"),
        "textBoxB" -> One("BBB"),
        "textBoxC" -> One("CCC"),
        "textBoxD" -> One("DDD"),
        "textBoxE" -> One("EEE"),
        "textBoxF" -> One("FFF"),
        "textBoxG" -> One("GGG"),
        "textBoxH" -> One("HHH"),
        "textBoxI" -> One("III"),
        "textBoxJ" -> One("JJJ")
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3",
        "n4",
        "n5",
        "n6",
        "n7",
        "n8",
        "n9",
        "n10"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "numberA"  -> NumberResult(161),
        "numberB"  -> NumberResult(161),
        "textBoxA" -> Hidden,
        "textBoxB" -> Hidden,
        "textBoxC" -> StringResult("CCC"),
        "textBoxD" -> Hidden,
        "textBoxE" -> Hidden,
        "textBoxF" -> StringResult("FFF"),
        "textBoxG" -> StringResult("GGG"),
        "textBoxH" -> StringResult("HHH"),
        "textBoxI" -> StringResult("III"),
        "textBoxJ" -> Hidden
      ),
      List(
        "numberA - Down: 161",
        "numberB - Up: 161",
        "textBoxA: ",
        "textBoxB: ",
        "textBoxC: CCC",
        "textBoxD: ",
        "textBoxE: ",
        "textBoxF: FFF",
        "textBoxG: GGG",
        "textBoxH: HHH",
        "textBoxI: III",
        "textBoxJ: ",
        "Is textBoxA visible: No : numberA * 0.1 < 16",
        "Is textBoxB visible: No : numberB * 0.1 < 16",
        "Is textBoxC visible: Yes : numberA * 0.1 <= 16",
        "Is textBoxD visible: No : numberB * 0.1 <= 16",
        "Is textBoxE visible: No : numberA * 0.1 > 16",
        "Is textBoxF visible: Yes : numberB * 0.1 > 16",
        "Is textBoxG visible: Yes : numberA * 0.1 >= 16",
        "Is textBoxH visible: Yes : numberB * 0.1 >= 16",
        "Is textBoxI visible: Yes : numberA * 0.1 = 16",
        "Is textBoxJ visible: No : numberB * 0.1 = 16",
        "numberA * 0.1: 16",
        "numberB * 0.1: 17",
        "numberA * 0.123: 19",
        "numberB * 0.123: 20",
        "0.123 * numberA: 19.8",
        "0.123 * numberB: 19.8"
      ),
      "text-number-rounding-lhs.json 161"
    ),
    (
      MongoUserData(
        "numberA"  -> One("170"),
        "numberB"  -> One("170"),
        "textBoxA" -> One("AAA"),
        "textBoxB" -> One("BBB"),
        "textBoxC" -> One("CCC"),
        "textBoxD" -> One("DDD"),
        "textBoxE" -> One("EEE"),
        "textBoxF" -> One("FFF"),
        "textBoxG" -> One("GGG"),
        "textBoxH" -> One("HHH"),
        "textBoxI" -> One("III"),
        "textBoxJ" -> One("JJJ")
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3",
        "n4",
        "n5",
        "n6",
        "n7",
        "n8",
        "n9",
        "n10"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "numberA"  -> NumberResult(170),
        "numberB"  -> NumberResult(170),
        "textBoxA" -> Hidden,
        "textBoxB" -> Hidden,
        "textBoxC" -> Hidden,
        "textBoxD" -> Hidden,
        "textBoxE" -> StringResult("EEE"),
        "textBoxF" -> StringResult("FFF"),
        "textBoxG" -> StringResult("GGG"),
        "textBoxH" -> StringResult("HHH"),
        "textBoxI" -> Hidden,
        "textBoxJ" -> Hidden
      ),
      List(
        "numberA - Down: 170",
        "numberB - Up: 170",
        "textBoxA: ",
        "textBoxB: ",
        "textBoxC: ",
        "textBoxD: ",
        "textBoxE: EEE",
        "textBoxF: FFF",
        "textBoxG: GGG",
        "textBoxH: HHH",
        "textBoxI: ",
        "textBoxJ: ",
        "Is textBoxA visible: No : numberA * 0.1 < 16",
        "Is textBoxB visible: No : numberB * 0.1 < 16",
        "Is textBoxC visible: No : numberA * 0.1 <= 16",
        "Is textBoxD visible: No : numberB * 0.1 <= 16",
        "Is textBoxE visible: Yes : numberA * 0.1 > 16",
        "Is textBoxF visible: Yes : numberB * 0.1 > 16",
        "Is textBoxG visible: Yes : numberA * 0.1 >= 16",
        "Is textBoxH visible: Yes : numberB * 0.1 >= 16",
        "Is textBoxI visible: No : numberA * 0.1 = 16",
        "Is textBoxJ visible: No : numberB * 0.1 = 16",
        "numberA * 0.1: 17",
        "numberB * 0.1: 17",
        "numberA * 0.123: 20",
        "numberB * 0.123: 21",
        "0.123 * numberA: 20.91",
        "0.123 * numberB: 20.91"
      ),
      "text-number-rounding-lhs.json 170"
    ),
    (
      MongoUserData(
        "decimals0" -> One("123.456"),
        "decimals1" -> One("123.456"),
        "decimals2" -> One("123.456"),
        "decimals3" -> One("123.456"),
        "value"     -> One("12")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "decimals0" -> NumberResult(123),
        "decimals1" -> NumberResult(123.4),
        "decimals2" -> NumberResult(123.45),
        "decimals3" -> NumberResult(123.456)
      ),
      List(
        "decimals0: 123",
        "decimals1: 123.4",
        "decimals2: 123.45",
        "decimals3: 123.456"
      ),
      "read-only-expressions.json Generated"
    ),
    (
      MongoUserData(
        "calculated"            -> One("24"),
        "positiveWholeSterling" -> One("12"),
        "sterling"              -> One("12.99")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "calculated"            -> NumberResult(24),
        "positiveWholeSterling" -> NumberResult(12),
        "sterling"              -> NumberResult(12.99)
      ),
      List.empty[String],
      "positive-whole-sterling-calculated.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"      -> Many(List("0")),
        "1_pageA"           -> One("1"),
        "1_pageB"           -> One("2"),
        "2_addAnother"      -> Many(List("1")),
        "2_pageA"           -> One("150"),
        "2_pageC"           -> One("3"),
        "2_pageD"           -> One("4"),
        "2_startDate-day"   -> One("2"),
        "2_startDate-month" -> One("2"),
        "2_startDate-year"  -> One("2024"),
        "dummy"             -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.2",
        "ap0.2.3",
        "ap0.2.4",
        "ar0.2",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageA"           -> NumberResult(1),
        "1_pageB"           -> NumberResult(2),
        "1_pageC"           -> Hidden,
        "1_pageD"           -> Hidden,
        "1_startDate-day"   -> Hidden,
        "1_startDate-month" -> Hidden,
        "1_startDate-year"  -> Hidden,
        "2_pageA"           -> NumberResult(150),
        "2_pageB"           -> Hidden,
        "2_pageC"           -> NumberResult(3),
        "2_pageD"           -> NumberResult(4),
        "2_startDate-day"   -> NumberResult(2),
        "2_startDate-month" -> NumberResult(2),
        "2_startDate-year"  -> NumberResult(2024)
      ),
      List(
        "Sum test: 23",
        "Sum test: 23 : 23 : 3",
        "<p>Sum test: 23 : 23 : 3</p>",
        "Sum test: 190",
        "Sum test: 190 : 167 : 157",
        "<p>Sum test: 190 : 167 : 157</p>",
        "externalExpression.sum = 190",
        "externalExpression = 23, 167",
        "externalExpression1: 3, 157",
        "pageA: 1, 150",
        "pageB: 2, 0",
        "pageC: 0, 3",
        "pageD: 0, 4"
      ),
      "sum-reference-atl.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"          -> Many(List("0")),
        "1_pptRateAtl"          -> One("200.00"),
        "1_taxYear"             -> Many(List("1")),
        "1_weightOfFinishedPPT" -> One("1000"),
        "2_addAnother"          -> Many(List("1")),
        "2_pptRateAtl"          -> One("210.82"),
        "2_taxYear"             -> Many(List("0")),
        "2_weightOfFinishedPPT" -> One("1000"),
        "dummy"                 -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pptRateAtl" -> NumberResult(BigDecimal("200.00")),
        "1_taxYear"    -> OptionResult(List("1")),
        "2_pptRateAtl" -> NumberResult(210.82),
        "2_taxYear"    -> OptionResult(List("0"))
      ),
      List(
        "Tax period 1 - Calculated amount due: £200.00",
        "<p>Tax period 1 - Calculated amount due: £200.00</p>",
        "Tax period 2 - Calculated amount due: £210.82",
        "<p>Tax period 2 - Calculated amount due: £210.82</p>",
        "liableAmountAtl.sum = £410.82",
        "liableAmount.sum = £410.82"
      ),
      "atl-outside-reference-chain.json Generated"
    ),
    (
      MongoUserData(
        "1_groupId"       -> One(""),
        "1_repeatedField" -> One("FOO"),
        "1_tax"           -> One("123"),
        "2_groupId"       -> One(""),
        "2_repeatedField" -> One("BAR"),
        "2_tax"           -> One("345")
      ),
      List(
        "n0",
        "r1.0",
        "r1.1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_groupId"       -> StringResult(""),
        "1_repeatedField" -> StringResult("FOO"),
        "2_groupId"       -> StringResult(""),
        "2_repeatedField" -> StringResult("BAR")
      ),
      List(
        "Descriptions: FOO, BAR",
        "<p>Descriptions: FOO, BAR</p>"
      ),
      "group-and-repeated-section.json Generated"
    ),
    (
      MongoUserData(
        "op1" -> One("2"),
        "op2" -> One("")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "add" -> NumberResult(BigDecimal("2.00")),
        "div" -> NumberResult(BigDecimal("0.00")),
        "mul" -> NumberResult(BigDecimal("0.00")),
        "op1" -> NumberResult(2),
        "op2" -> Empty,
        "sub" -> NumberResult(BigDecimal("2.00"))
      ),
      List(
        "£2.00 + £0.00 = £2.00",
        "£0.00 + £2.00 = £2.00",
        "£2.00 - £0.00 = £2.00",
        "£0.00 - £2.00 = -£2.00",
        "£2.00 * £0.00 = £0.00",
        "£0.00 * £2.00 = £0.00",
        "£2.00 / £0.00 = £0.00",
        "£0.00 / £2.00 = £0.00"
      ),
      "arithmetic-optional.json op2 empty"
    ),
    (
      MongoUserData(
        "op1" -> One(""),
        "op2" -> One("2")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "add" -> NumberResult(BigDecimal("2.00")),
        "div" -> NumberResult(BigDecimal("0.00")),
        "mul" -> NumberResult(BigDecimal("0.00")),
        "op1" -> Empty,
        "op2" -> NumberResult(2),
        "sub" -> NumberResult(BigDecimal("-2.00"))
      ),
      List(
        "£0.00 + £2.00 = £2.00",
        "£2.00 + £0.00 = £2.00",
        "£0.00 - £2.00 = -£2.00",
        "£2.00 - £0.00 = £2.00",
        "£0.00 * £2.00 = £0.00",
        "£2.00 * £0.00 = £0.00",
        "£0.00 / £2.00 = £0.00",
        "£2.00 / £0.00 = £0.00"
      ),
      "arithmetic-optional.json op1 empty"
    ),
    (
      MongoUserData(
        "value" -> One("-10"),
        "op2"   -> One("3")
      ),
      List(
        "n0",
        "n2",
        "n3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "value" -> NumberResult(-10),
        "op1"   -> Hidden,
        "op2"   -> NumberResult(3),
        "add"   -> NumberResult(BigDecimal("3.00")),
        "div"   -> NumberResult(BigDecimal("0.00")),
        "mul"   -> NumberResult(BigDecimal("0.00")),
        "sub"   -> NumberResult(BigDecimal("-3.00"))
      ),
      List(
        "£0.00 + £3.00 = £3.00",
        "£3.00 + £0.00 = £3.00",
        "£0.00 - £3.00 = -£3.00",
        "£3.00 - £0.00 = £3.00",
        "£0.00 * £3.00 = £0.00",
        "£3.00 * £0.00 = £0.00",
        "£0.00 / £3.00 = £0.00",
        "£3.00 / £0.00 = £0.00"
      ),
      "arithmetic-visibility.json op1 hidden"
    ),
    (
      MongoUserData(
        "value" -> One("10"),
        "op1"   -> One("2")
      ),
      List(
        "n0",
        "n1",
        "n3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "value" -> NumberResult(10),
        "op1"   -> NumberResult(2),
        "op2"   -> Hidden,
        "add"   -> NumberResult(BigDecimal("2.00")),
        "div"   -> NumberResult(BigDecimal("0.00")),
        "mul"   -> NumberResult(BigDecimal("0.00")),
        "sub"   -> NumberResult(BigDecimal("2.00"))
      ),
      List(
        "£2.00 + £0.00 = £2.00",
        "£0.00 + £2.00 = £2.00",
        "£2.00 - £0.00 = £2.00",
        "£0.00 - £2.00 = -£2.00",
        "£2.00 * £0.00 = £0.00",
        "£0.00 * £2.00 = £0.00",
        "£2.00 / £0.00 = £0.00",
        "£0.00 / £2.00 = £0.00"
      ),
      "arithmetic-visibility.json op2 hidden"
    ),
    (
      MongoUserData(
        "dummy"           -> One("dummy"),
        "endDate-day"     -> One("12"),
        "endDate-month"   -> One("12"),
        "endDate-year"    -> One("2005"),
        "startDate-day"   -> One("1"),
        "startDate-month" -> One("1"),
        "startDate-year"  -> One("2000")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "More than 30",
        "TotalMonths: 71"
      ),
      "period-between-two-dates-if-else.json more than 30"
    ),
    (
      MongoUserData(
        "dummy"           -> One("dummy"),
        "endDate-day"     -> One("12"),
        "endDate-month"   -> One("12"),
        "endDate-year"    -> One("2005"),
        "startDate-day"   -> One("1"),
        "startDate-month" -> One("1"),
        "startDate-year"  -> One("2004")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "Less than 30",
        "TotalMonths: 23"
      ),
      "period-between-two-dates-if-else.json less than 30"
    ),
    (
      MongoUserData(
        "1_addAnother"  -> Many(List("1")),
        "1_file"        -> One("1"),
        "1_secret"      -> One("123"),
        "dummy"         -> One("dummy"),
        "m1_1_file"     -> One("m1_1_file_passport.jpg"),
        "outsideSecret" -> One("500")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ap0.1.2",
        "ar0.1",
        "n1",
        "n2"
      ),
      EvaluationContext.empty.copy(
        componentIdToFileId = FormComponentIdToFileIdMapping(
          Map(
            FileComponentId.Multi(FormComponentId("1_file"), 1) -> FileId("m1_1_file")
          )
        )
      ),
      AnswerMap(
        "1_file"        -> StringResult("1"),
        "1_secret"      -> NumberResult(123),
        "outsideSecret" -> NumberResult(500)
      ),
      List(
        "Files: £123.00",
        "<p>Files: £123.00</p>",
        "1 - passport.jpg £123.00",
        "<p>1 - passport.jpg £123.00</p>",
        "Files: passport.jpg",
        "<p>Files: passport.jpg</p>"
      ),
      "atl-multi-file.json one file"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("1")),
        "1_file"       -> One("1,2"),
        "dummy"        -> One("dummy"),
        "m1_1_file"    -> One("m1_1_file_passport.jpg"),
        "m2_1_file"    -> One("m2_1_file_rust_logo.jpg")
      ),
      List(
        "ap0.1.0",
        "ap0.1.2",
        "ar0.1",
        "n1"
      ),
      EvaluationContext.empty.copy(
        componentIdToFileId = FormComponentIdToFileIdMapping(
          Map(
            FileComponentId.Multi(FormComponentId("1_file"), 1) -> FileId("m1_1_file"),
            FileComponentId.Multi(FormComponentId("1_file"), 2) -> FileId("m2_1_file")
          )
        )
      ),
      AnswerMap(
        "1_file"        -> StringResult("1,2"),
        "1_secret"      -> Hidden,
        "outsideSecret" -> Hidden
      ),
      List(
        "Files: £0.00",
        "<p>Files: £0.00</p>",
        "1 - passport.jpg, rust_logo.jpg £0.00",
        "<p>1 - passport.jpg, rust_logo.jpg £0.00</p>",
        "Files: passport.jpg, rust_logo.jpg",
        "<p>Files: passport.jpg, rust_logo.jpg</p>"
      ),
      "atl-multi-file.json two files"
    ),
    (
      MongoUserData(
        "1_addAnother"  -> Many(List("0")),
        "1_file"        -> One("1_file_passport.jpg"),
        "1_secret"      -> One("123"),
        "2_addAnother"  -> Many(List("1")),
        "2_file"        -> One("2_file_report.pdf"),
        "dummy"         -> One("dummy"),
        "outsideSecret" -> One("500")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ap0.1.2",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.2",
        "ar0.2",
        "n1",
        "n2"
      ),
      EvaluationContext.empty.copy(
        componentIdToFileId = FormComponentIdToFileIdMapping(
          Map(
            FileComponentId.Single(FormComponentId("1_file")) -> FileId("1_file"),
            FileComponentId.Single(FormComponentId("2_file")) -> FileId("2_file")
          )
        )
      ),
      AnswerMap(
        "1_file"        -> StringResult("1_file_passport.jpg"),
        "1_secret"      -> NumberResult(123),
        "2_file"        -> StringResult("2_file_report.pdf"),
        "2_secret"      -> Hidden,
        "outsideSecret" -> NumberResult(500)
      ),
      List(
        "Files: £123.00",
        "<p>Files: £123.00</p>",
        "1 - passport.jpg £123.00",
        "<p>1 - passport.jpg £123.00</p>",
        "Files: £0.00",
        "<p>Files: £0.00</p>",
        "2 - report.pdf £0.00",
        "<p>2 - report.pdf £0.00</p>",
        "Files: passport.jpg, report.pdf",
        "<p>Files: passport.jpg, report.pdf</p>"
      ),
      "atl-file.json secrets exist"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("1")),
        "1_file"       -> One("1_file_report.pdf"),
        "dummy"        -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ap0.1.2",
        "ar0.1",
        "n1"
      ),
      EvaluationContext.empty.copy(
        componentIdToFileId = FormComponentIdToFileIdMapping(
          Map(
            FileComponentId.Single(FormComponentId("1_file")) -> FileId("1_file")
          )
        )
      ),
      AnswerMap(
        "1_file"        -> StringResult("1_file_report.pdf"),
        "1_secret"      -> Hidden,
        "outsideSecret" -> Hidden
      ),
      List(
        "Files: £0.00",
        "<p>Files: £0.00</p>",
        "1 - report.pdf £0.00",
        "<p>1 - report.pdf £0.00</p>",
        "Files: report.pdf",
        "<p>Files: report.pdf</p>"
      ),
      "atl-file.json secrets do not exists"
    ),
    (
      MongoUserData(
        "cost" -> One("6000000")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "Cost will be £6,000,000",
        "<p>Cost will be £6,000,000</p>"
      ),
      "positive-whole-sterling.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"  -> Many(List("0")),
        "1_choiceField" -> Many(List("1")),
        "1_text"        -> One("FOO"),
        "2_addAnother"  -> Many(List("1")),
        "2_choiceField" -> Many(List("2")),
        "2_dutyType"    -> Many(List("1")),
        "2_text"        -> One("")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_choiceField"        -> OptionResult(List("1")),
        "1_dutyType"           -> Hidden,
        "1_text"               -> StringResult("FOO"),
        "2_choiceField"        -> OptionResult(List("2")),
        "2_dutyType"           -> OptionResult(List("1")),
        "2_text"               -> Hidden,
        "submissionReference"  -> StringResult("BPLX-FGU6-E7Q2"),
        "submissionReference2" -> Empty
      ),
      List(
        "Text",
        "Text",
        "<p>Text</p>",
        "Choice",
        "Choice",
        "<p>Choice</p>",
        "Text box FOO"
      ),
      "revealing-choice-check-outside-atl.json Generated"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty,
      AnswerMap(
        "averageEarn" -> NumberResult(168.09)
      ),
      List(
        "Total claimable advance payment amount is **£155.83**",
        "<p>Total claimable advance payment amount is <strong>£155.83</strong></p>"
      ),
      "total-value.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("1")),
        "1_taxA"       -> One("10"),
        "1_taxB"       -> One("10"),
        "1_taxPayed"   -> One("2.5")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_taxA"     -> NumberResult(10),
        "1_taxB"     -> NumberResult(10),
        "1_taxPayed" -> NumberResult(BigDecimal("25.00"))
      ),
      List(
        "£10.00",
        "Tax 1 of 1",
        "£10.00",
        "<p>£10.00</p>"
      ),
      "total-value-atl.json Generated"
    ),
    (
      MongoUserData(
        "1_endDate-day"     -> One("1"),
        "1_endDate-month"   -> One("1"),
        "1_endDate-year"    -> One("2021"),
        "1_fieldGroup"      -> One(""),
        "1_startDate-day"   -> One("1"),
        "1_startDate-month" -> One("1"),
        "1_startDate-year"  -> One("2020"),
        "2_endDate-day"     -> One("1"),
        "2_endDate-month"   -> One("1"),
        "2_endDate-year"    -> One("2020"),
        "2_fieldGroup"      -> One(""),
        "2_startDate-day"   -> One("1"),
        "2_startDate-month" -> One("1"),
        "2_startDate-year"  -> One("2018"),
        "3_endDate-day"     -> One("1"),
        "3_endDate-month"   -> One("2"),
        "3_endDate-year"    -> One("2019"),
        "3_fieldGroup"      -> One(""),
        "3_startDate-day"   -> One("1"),
        "3_startDate-month" -> One("1"),
        "3_startDate-year"  -> One("2018")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "textBox" -> StringResult("BPLX-FGU6-E7Q2")
      ),
      List(
        """|Text box Period sum : P4Y1M
           |
           |Period totalMonths : 49
           |
           |Period years : 4
           |
           |Period months : 1
           |
           |Period days : 0""".stripMargin
      ),
      "date-date-period-group.json Generated"
    ),
    (
      MongoUserData(
        "1_endDate-day"     -> One("1"),
        "1_endDate-month"   -> One("1"),
        "1_endDate-year"    -> One("2021"),
        "1_addAnother"      -> Many(List("0")),
        "1_startDate-day"   -> One("1"),
        "1_startDate-month" -> One("2"),
        "1_startDate-year"  -> One("2020"),
        "2_endDate-day"     -> One("1"),
        "2_endDate-month"   -> One("1"),
        "2_endDate-year"    -> One("2020"),
        "2_addAnother"      -> Many(List("0")),
        "2_startDate-day"   -> One("1"),
        "2_startDate-month" -> One("1"),
        "2_startDate-year"  -> One("2018"),
        "3_endDate-day"     -> One("1"),
        "3_endDate-month"   -> One("2"),
        "3_endDate-year"    -> One("2019"),
        "3_startDate-day"   -> One("1"),
        "3_startDate-month" -> One("1"),
        "3_startDate-year"  -> One("2018")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        """|This is for AddToList Period sum : P4Y
           |
           |Period totalMonths : 48
           |
           |Period years : 4
           |
           |Period months : 0
           |
           |Period days : 0""".stripMargin,
        "1 February 2020 ... 1 January 2021",
        "<p>1 February 2020 ... 1 January 2021</p>",
        """|This is for AddToList Period sum : P4Y
           |
           |Period totalMonths : 48
           |
           |Period years : 4
           |
           |Period months : 0
           |
           |Period days : 0""".stripMargin,
        "1 January 2018 ... 1 January 2020",
        "<p>1 January 2018 ... 1 January 2020</p>",
        """|This is for AddToList Period sum : P4Y
           |
           |Period totalMonths : 48
           |
           |Period years : 4
           |
           |Period months : 0
           |
           |Period days : 0""".stripMargin,
        "1 January 2018 ... 1 February 2019",
        "<p>1 January 2018 ... 1 February 2019</p>"
      ),
      "date-date-period.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"               -> Many(List("0")),
        "1_tradesUnderDifferentName" -> Many(List("0")),
        "2_addAnother"               -> Many(List("0")),
        "2_tradesUnderDifferentName" -> Many(List("1")),
        "3_tradesUnderDifferentName" -> Many(List("2"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "Yes",
        "1 - Yes",
        "<p>1 - Yes</p>",
        "No",
        "2 - No",
        "<p>2 - No</p>",
        "Don't know",
        "3 - Don't know",
        "<p>3 - Don't know</p>"
      ),
      "revealing-choice-in-atl.json Generated"
    ),
    (
      MongoUserData(
        "ctutrSummary" -> One("12345678"),
        "dummy"        -> One("dummy")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(
        retrievals = authenticatedRetrievals.copy(
          enrolments = Enrolments(
            Set(
              Enrolment(
                "IR-CT",
                Seq(EnrolmentIdentifier("UTR", "1234567895")),
                "Activated",
                None
              )
            )
          )
        )
      ),
      AnswerMap(
        "ctutrSummary" -> StringResult("1234567895")
      ),
      List(
        "User instruction 1234567895"
      ),
      "summary-info-only.json Generated"
    ),
    (
      MongoUserData(
        "vatNumber" -> One("123")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "vatNumber"       -> StringResult("123"),
        "secretUser"      -> Hidden,
        "secretDelegated" -> Hidden,
        "secretMongo"     -> Hidden,
        "secretSeiss"     -> Hidden
      ),
      List(
        "123 in user.enrolments.HMCE-VATDEC-ORG.VatRegNo: No",
        "123 in delegated.classic.enrolments.HMCE-VATDEC-ORG.VatRegNo: No",
        "123 in service.seiss: No",
        "123 in mongo.mtdVatNumber: No",
        "Tax year: 5 April 2025"
      ),
      "in-boolean-expr.json in no"
    ),
    (
      MongoUserData(
        "vatNumber" -> One("123")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          booleanExprCache = BooleanExprCache(
            Map(
              DataSource.Enrolment(ServiceName("HMCE-VATDEC-ORG"), IdentifierName("VatRegNo")) -> Map(
                "123" -> true
              ),
              DataSource.DelegatedEnrolment(ServiceName("HMCE-VATDEC-ORG"), IdentifierName("VatRegNo")) -> Map(
                "123" -> true
              ),
              DataSource.SeissEligible -> Map(
                "123" -> true
              ),
              DataSource.Mongo(CollectionName("mtdVatNumber")) -> Map(
                "123" -> true
              )
            )
          )
        )
      ),
      AnswerMap(
        "vatNumber"       -> StringResult("123"),
        "secretUser"      -> Empty,
        "secretDelegated" -> Empty,
        "secretSeiss"     -> Empty,
        "secretMongo"     -> Empty
      ),
      List(
        "123 in user.enrolments.HMCE-VATDEC-ORG.VatRegNo: Yes",
        "123 in delegated.classic.enrolments.HMCE-VATDEC-ORG.VatRegNo: Yes",
        "123 in service.seiss: Yes",
        "123 in mongo.mtdVatNumber: Yes",
        "Tax year: 5 April 2012"
      ),
      "in-boolean-expr.json in yes"
    ),
    (
      MongoUserData(
        "instruction" -> One("AAA")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "instructionPDF"    -> Hidden,
        "notInstructionPDF" -> Empty
      ),
      List(
        "User instruction"
      ),
      "form-phase.json no form phase"
    ),
    (
      MongoUserData(
        "instruction" -> One("AAA")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(formPhase = Some(FormPhase(InstructionPDF))),
      AnswerMap(
        "instructionPDF"    -> StringResult("123"),
        "notInstructionPDF" -> Hidden
      ),
      List(
        "Operator instruction"
      ),
      "form-phase.json instruction pdf form phase" // TODO JoVl This form's instruction pdf generation is not working as expected, it need to be fixed!!!
    ),
    (
      MongoUserData(
        "1_addAnotherCountry" -> Many(List("0")),
        "1_country"           -> One("Australia"),
        "1_dummy"             -> One("AAA"),
        "2_addAnotherCountry" -> Many(List("0")),
        "2_country"           -> One("Belgium"),
        "2_dummy"             -> One("BBB"),
        "3_addAnotherCountry" -> Many(List("0")),
        "3_country"           -> One("Czech Republic"),
        "3_dummy"             -> One("CCC"),
        "4_country"           -> One("France"),
        "4_dummy"             -> One("DDD"),
        "homeCountry"         -> One("Czech Republic")
      ),
      List(
        "n0",
        "ap1.1.0",
        "ap1.1.1",
        "ar1.1",
        "ap1.2.0",
        "ap1.2.1",
        "ar1.2",
        "ap1.3.0",
        "ap1.3.1",
        "ar1.3",
        "ap1.4.0",
        "ap1.4.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_country"     -> StringResult("Australia"),
        "1_inSecret"    -> Hidden,
        "1_notInSecret" -> Empty,
        "2_country"     -> StringResult("Belgium"),
        "2_inSecret"    -> Hidden,
        "2_notInSecret" -> Empty,
        "3_country"     -> StringResult("Czech Republic"),
        "3_inSecret"    -> Empty,
        "3_notInSecret" -> Hidden,
        "4_country"     -> StringResult("France"),
        "4_inSecret"    -> Empty,
        "4_notInSecret" -> Hidden,
        "homeCountry"   -> StringResult("Czech Republic")
      ),
      List(
        "Home country: Czech Republic",
        "Country: Australia",
        "Home country is in country: No",
        "Home country is not in country: Yes",
        "1: Australia",
        "<p>1: Australia</p>",
        "Home country: Czech Republic",
        "Country: Belgium",
        "Home country is in country: No",
        "Home country is not in country: Yes",
        "2: Belgium",
        "<p>2: Belgium</p>",
        "Home country: Czech Republic",
        "Country: Czech Republic",
        "Home country is in country: Yes",
        "Home country is not in country: No",
        "3: Czech Republic",
        "<p>3: Czech Republic</p>",
        "Home country: Czech Republic",
        "Country: France",
        "Home country is in country: Yes",
        "Home country is not in country: No",
        "4: France",
        "<p>4: France</p>"
      ),
      "has-answer.json Generated"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty,
      AnswerMap(
        "emailSecret" -> Hidden,
        "ggSecret"    -> Hidden
      ),
      List(
        "GG login: No",
        "Email login: No",
        "Unknown login: Yes"
      ),
      "is-login.json unknown login"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty.copy(
        retrievals = EmailRetrievals(EmailId(CIString.empty))
      ),
      AnswerMap(
        "emailSecret" -> Empty,
        "ggSecret"    -> Hidden
      ),
      List(
        "GG login: No",
        "Email login: Yes",
        "Unknown login: No"
      ),
      "is-login.json email login"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty.copy(
        retrievals = AuthenticatedRetrievals(
          GovernmentGatewayId(""),
          Enrolments(Set.empty),
          AffinityGroup.Individual,
          "group-id",
          None,
          OtherRetrievals.empty,
          ConfidenceLevel.L50,
          Some(User)
        )
      ),
      AnswerMap(
        "emailSecret" -> Hidden,
        "ggSecret"    -> Empty
      ),
      List(
        "GG login: Yes",
        "Email login: No",
        "Unknown login: No"
      ),
      "is-login.json gg login"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnother" -> Empty,
        "1_age"        -> Hidden
      ),
      List(
        "1: ",
        "<p>1:</p>",
        "You have told us about 1 names.",
        "<p>You have told us about 1 names.</p>"
      ),
      "atl-count-repeater.json first iteration in progress"
    ),
    (
      MongoUserData(
        "1_name" -> One("AAA")
      ),
      List(
        "ap0.1.0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnother" -> Empty,
        "1_age"        -> Hidden
      ),
      List(
        "1: AAA",
        "<p>1: AAA</p>",
        "You have told us about 1 names.",
        "<p>You have told us about 1 names.</p>"
      ),
      "atl-count-repeater.json first iteration in progress"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_name"       -> One("AAA")
      ),
      List(
        "ap0.1.0",
        "ar0.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnother" -> OptionResult(List("0")),
        "1_age"        -> Hidden
      ),
      List(
        "1: AAA",
        "<p>1: AAA</p>",
        "You have told us about 2 names.",
        "<p>You have told us about 2 names.</p>",
        "2: ",
        "<p>2:</p>",
        "You have told us about 2 names.",
        "<p>You have told us about 2 names.</p>"
      ),
      "atl-count-repeater.json second iteration in progress"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_name"       -> One("AAA"),
        "2_name"       -> One("BBB")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnother" -> OptionResult(List("0")),
        "1_age"        -> Hidden,
        "2_addAnother" -> Empty,
        "2_age"        -> Hidden
      ),
      List(
        "1: AAA",
        "<p>1: AAA</p>",
        "You have told us about 2 names.",
        "<p>You have told us about 2 names.</p>",
        "2: BBB",
        "<p>2: BBB</p>",
        "You have told us about 2 names.",
        "<p>You have told us about 2 names.</p>"
      ),
      "atl-count-repeater.json second iteration in progress"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_name"       -> One("AAA"),
        "2_addAnother" -> Many(List("1")),
        "2_name"       -> One("BBB")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnother" -> OptionResult(List("0")),
        "1_age"        -> Hidden,
        "2_addAnother" -> OptionResult(List("1")),
        "2_age"        -> Hidden
      ),
      List(
        "1: AAA",
        "<p>1: AAA</p>",
        "You have told us about 2 names.",
        "<p>You have told us about 2 names.</p>",
        "2: BBB",
        "<p>2: BBB</p>",
        "You have told us about 2 names.",
        "<p>You have told us about 2 names.</p>"
      ),
      "atl-count-repeater.json second iteration finished"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_name"       -> One("AAA"),
        "2_addAnother" -> Many(List("0")),
        "2_name"       -> One("BBB")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnother" -> OptionResult(List("0")),
        "1_age"        -> Empty,
        "2_addAnother" -> OptionResult(List("0")),
        "2_age"        -> Empty
      ),
      List(
        "1: AAA",
        "<p>1: AAA</p>",
        "You have told us about 3 names.",
        "<p>You have told us about 3 names.</p>",
        "2: BBB",
        "<p>2: BBB</p>",
        "You have told us about 3 names.",
        "<p>You have told us about 3 names.</p>",
        "3: ",
        "<p>3:</p>",
        "You have told us about 3 names.",
        "<p>You have told us about 3 names.</p>"
      ),
      "atl-count-repeater.json third iteration in progress"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_personName" -> One("AAA"),
        "1_personNino" -> One("CC111111C"),
        "2_addAnother" -> Many(List("1")),
        "2_personName" -> One("BBB"),
        "2_personNino" -> One("SE101010A")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1: AAA - CC111111C!",
        "<p>1: AAA - CC111111C!</p>",
        "You have told us about 2.",
        "<p>You have told us about 2.</p>",
        "2: BBB - SE101010A!",
        "<p>2: BBB - SE101010A!</p>",
        "You have told us about 2.",
        "<p>You have told us about 2.</p>",
        "personName = AAA, BBB",
        "personNino = CC111111C, SE101010A",
        "Duplicate exists in personName: No",
        "Duplicate exists in personNino: No",
        "Duplicate exists in (personName, personNino): No"
      ),
      "duplicate-exists.json no duplicates"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_personName" -> One("AAA"),
        "1_personNino" -> One("CC111111C"),
        "2_addAnother" -> Many(List("1")),
        "2_personName" -> One("AAA"),
        "2_personNino" -> One("SE101010A")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1: AAA - CC111111C!",
        "<p>1: AAA - CC111111C!</p>",
        "You have told us about 2.",
        "<p>You have told us about 2.</p>",
        "2: AAA - SE101010A!",
        "<p>2: AAA - SE101010A!</p>",
        "You have told us about 2.",
        "<p>You have told us about 2.</p>",
        "personName = AAA, AAA",
        "personNino = CC111111C, SE101010A",
        "Duplicate exists in personName: Yes",
        "Duplicate exists in personNino: No",
        "Duplicate exists in (personName, personNino): No"
      ),
      "duplicate-exists.json duplicate name"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_personNino" -> One("CC111111C"),
        "1_personName" -> One("AAA"),
        "2_addAnother" -> Many(List("1")),
        "2_personNino" -> One("CC111111C"),
        "2_personName" -> One("BBB")
      ),
      List(
        "ap0.1.0",
        "ac0.1",
        "ar0.1",
        "ap0.2.0",
        "ac0.2",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1: AAA - CC111111C!",
        "<p>1: AAA - CC111111C!</p>",
        "You have told us about 2.",
        "<p>You have told us about 2.</p>",
        "2: BBB - CC111111C!",
        "<p>2: BBB - CC111111C!</p>",
        "You have told us about 2.",
        "<p>You have told us about 2.</p>",
        "personName = AAA, BBB",
        "personNino = CC111111C, CC111111C",
        "Duplicate exists in personName: No",
        "Duplicate exists in personNino: Yes",
        "Duplicate exists in (personName, personNino): No"
      ),
      "duplicate-exists.json duplicate nino"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_personName" -> One("AAA"),
        "1_personNino" -> One("CC111111C"),
        "2_addAnother" -> Many(List("1")),
        "2_personName" -> One("AAA"),
        "2_personNino" -> One("CC111111C")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1: AAA - CC111111C!",
        "<p>1: AAA - CC111111C!</p>",
        "You have told us about 2.",
        "<p>You have told us about 2.</p>",
        "2: AAA - CC111111C!",
        "<p>2: AAA - CC111111C!</p>",
        "You have told us about 2.",
        "<p>You have told us about 2.</p>",
        "personName = AAA, AAA",
        "personNino = CC111111C, CC111111C",
        "Duplicate exists in personName: Yes",
        "Duplicate exists in personNino: Yes",
        "Duplicate exists in (personName, personNino): Yes"
      ),
      "duplicate-exists.json duplicate name and nino"
    ),
    (
      MongoUserData(
        "1_personName" -> One("AAA"),
        "1_personNino" -> One("CC111111C")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_personNino"                 -> StringResult("CC111111C"),
        "1_personChangeOrRemoveChoice" -> Hidden
      ),
      List(
        "What is the National Insurance number for AAA?",
        "1: <strong>AAA</strong> - nino: CC111111C",
        "<p>1: <strong>AAA</strong> - nino: CC111111C</p>",
        "You have told us about 1 of 2. You need to add the remaining persons.",
        "<p>You have told us about 1 of 2. You need to add the remaining persons.</p>"
      ),
      "duplicate-exists-confirmation.json first person"
    ),
    (
      MongoUserData(
        "1_addAnother"                 -> Many(List("0")),
        "1_personNino"                 -> One("CC111111C"),
        "1_personName"                 -> One("AAA"),
        "2_personChangeOrRemoveChoice" -> Many(List("removePerson")),
        "2_personNino"                 -> One("CC111111C"),
        "2_personName"                 -> One("BBB")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ap0.2.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_personChangeOrRemoveChoice" -> Hidden,
        "1_personNino"                 -> StringResult("CC111111C"),
        "2_personChangeOrRemoveChoice" -> OptionResult(List("removePerson")),
        "2_personNino"                 -> StringResult("CC111111C")
      ),
      List(
        "What is the National Insurance number for AAA?",
        "1: <strong>AAA</strong> - nino: CC111111C",
        "<p>1: <strong>AAA</strong> - nino: CC111111C</p>",
        "You have told us about 2 of 2. You have provided the details of all the persons.",
        "<p>You have told us about 2 of 2. You have provided the details of all the persons.</p>",
        "What is the National Insurance number for BBB?",
        "BBB",
        "CC111111C",
        "2: <strong>BBB</strong> - nino: CC111111C",
        "<p>2: <strong>BBB</strong> - nino: CC111111C</p>",
        "You have told us about 2 of 2. You have provided the details of all the persons.",
        "<p>You have told us about 2 of 2. You have provided the details of all the persons.</p>"
      ),
      "duplicate-exists-confirmation.json second person duplicated"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_personName" -> One("AAA"),
        "1_personNino" -> One("CC111111C"),
        "2_addAnother" -> Many(List("1")),
        "2_personName" -> One("BBB"),
        "2_personNino" -> One("SE101010A")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_personNino"                 -> StringResult("CC111111C"),
        "1_personChangeOrRemoveChoice" -> Hidden,
        "2_personNino"                 -> StringResult("SE101010A"),
        "2_personChangeOrRemoveChoice" -> Hidden
      ),
      List(
        "What is the National Insurance number for AAA?",
        "1: <strong>AAA</strong> - nino: CC111111C",
        "<p>1: <strong>AAA</strong> - nino: CC111111C</p>",
        "You have told us about 2 of 2. You have provided the details of all the persons.",
        "<p>You have told us about 2 of 2. You have provided the details of all the persons.</p>",
        "What is the National Insurance number for BBB?",
        "2: <strong>BBB</strong> - nino: SE101010A",
        "<p>2: <strong>BBB</strong> - nino: SE101010A</p>",
        "You have told us about 2 of 2. You have provided the details of all the persons.",
        "<p>You have told us about 2 of 2. You have provided the details of all the persons.</p>"
      ),
      "duplicate-exists-confirmation.json second person unique"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_name"       -> One("AAA"),
        "1_tax"        -> One("100"),
        "2_addAnother" -> Many(List("0")),
        "2_age"        -> One("25"),
        "2_tax"        -> One("200"),
        "3_age"        -> One("35"),
        "3_tax"        -> One("300")
      ),
      List(
        "ap0.1.0",
        "ap0.1.2",
        "ar0.1",
        "ap0.2.1",
        "ap0.2.2",
        "ar0.2",
        "ap0.3.1",
        "ap0.3.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnother" -> OptionResult(List("0")),
        "1_age"        -> Hidden,
        "1_name"       -> StringResult("AAA"),
        "2_addAnother" -> OptionResult(List("0")),
        "2_age"        -> NumberResult(25),
        "2_name"       -> Hidden,
        "3_age"        -> NumberResult(35),
        "3_name"       -> Hidden,
        "3_addAnother" -> Empty
      ),
      List(
        "AAA",
        "1 - AAA - 0 - £100.00",
        "<p>1 - AAA - 0 - £100.00</p>",
        "",
        "2 -  - 25 - £200.00",
        "<p>2 - - 25 - £200.00</p>",
        "",
        "3 -  - 35 - £300.00",
        "<p>3 - - 35 - £300.00</p>"
      ),
      "atl-first.json Generated"
    ),
    (
      MongoUserData(
        "employer" -> Many(List("EMP_2")),
        "nino"     -> One("AA333333A"),
        "secret"   -> Many(List("reveal")),
        "taxYear"  -> One("2000")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employments" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName")   -> "Acme",
                  DataRetrieve.Attribute("sequenceNumber") -> "1234561"
                ),
                Map(
                  DataRetrieve.Attribute("employerName")   -> "Smith Holdings",
                  DataRetrieve.Attribute("sequenceNumber") -> "2345678"
                ),
                Map(
                  DataRetrieve.Attribute("employerName")   -> "Acme",
                  DataRetrieve.Attribute("sequenceNumber") -> "2345678"
                )
              )
          )
        )
      ),
      AnswerMap(
        "employer" -> OptionResult(List("EMP_2")),
        "secret"   -> OptionResult(List("reveal"))
      ),
      List(
        "Acme",
        "Smith Holdings",
        "Acme",
        "Secret 2345678"
      ),
      "index-of-data-retrieve-ctx.json secret is visible"
    ),
    (
      MongoUserData(
        "employer" -> Many(List("EMP_0")),
        "nino"     -> One("AA333333A"),
        "secret"   -> Many(List("reveal")),
        "taxYear"  -> One("2000")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employments" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName")   -> "Acme",
                  DataRetrieve.Attribute("sequenceNumber") -> "1234561"
                )
              )
          )
        )
      ),
      AnswerMap(
        "employer" -> OptionResult(List("EMP_0")),
        "secret"   -> Hidden
      ),
      List(
        "Acme"
      ),
      "index-of-data-retrieve-ctx.json secret is hidden"
    ),
    (
      MongoUserData(
        "op1" -> One(""),
        "op2" -> One("")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "op1"            -> Empty,
        "op2"            -> Empty,
        "secretFallback" -> Empty,
        "secretOp1"      -> Hidden,
        "secretOp2"      -> Hidden
      ),
      List(
        "'0' orElse '0' orElse 'Fallback' = Fallback"
      ),
      "or-else.json fallback"
    ),
    (
      MongoUserData(
        "op1" -> One("1.23"),
        "op2" -> One("")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "op1"            -> NumberResult(1.23),
        "op2"            -> Empty,
        "secretFallback" -> Hidden,
        "secretOp1"      -> Empty,
        "secretOp2"      -> Hidden
      ),
      List(
        "'1.23' orElse '0' orElse 'Fallback' = 1.23"
      ),
      "or-else.json op1"
    ),
    (
      MongoUserData(
        "op1" -> One(""),
        "op2" -> One("2.36")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "op1"            -> Empty,
        "op2"            -> NumberResult(2.36),
        "secretFallback" -> Hidden,
        "secretOp1"      -> Hidden,
        "secretOp2"      -> Empty
      ),
      List(
        "'0' orElse '2.36' orElse 'Fallback' = 2.36"
      ),
      "or-else.json op2"
    ),
    (
      MongoUserData(
        "op1" -> One("1.23"),
        "op2" -> One("2.36")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "op1"            -> NumberResult(1.23),
        "op2"            -> NumberResult(2.36),
        "secretFallback" -> Hidden,
        "secretOp1"      -> Empty,
        "secretOp2"      -> Hidden
      ),
      List(
        "'1.23' orElse '2.36' orElse 'Fallback' = 1.23"
      ),
      "or-else.json op1 and op2"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_employer"   -> One("Foo"),
        "2_addAnother" -> Many(List("0")),
        "2_employer"   -> One("Bar"),
        "3_addAnother" -> Many(List("0")),
        "3_employer"   -> One("Baz"),
        "4_addAnother" -> Many(List("1")),
        "4_employer"   -> One("Foo")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "ap0.4.0",
        "ar0.4"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"        -> StringResult("Foo"),
        "2_employer"        -> StringResult("Bar"),
        "3_employer"        -> StringResult("Baz"),
        "4_employer"        -> StringResult("Foo"),
        "noDuplicatesFalse" -> Empty,
        "noDuplicatesTrue"  -> Empty
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "4 - Foo",
        "<p>4 - Foo</p>",
        "Foo",
        "Bar",
        "Baz",
        "Foo",
        "Foo",
        "Bar",
        "Baz",
        "Foo",
        "table - totalChoicesSelected(noDuplicatesTrue): 0",
        "table - totalChoicesSelected(noDuplicatesFalse): 0",
        "table - choicesCount(noDuplicatesTrue): 3",
        "table - choicesCount(noDuplicatesFalse): 4",
        "table - choicesAvailable(noDuplicatesTrue): 3",
        "table - choicesAvailable(noDuplicatesFalse): 4",
        "table - choicesSelected(noDuplicatesTrue).count: 0",
        "table - choicesSelected(noDuplicatesFalse).count: 0",
        "table - choicesSelected(noDuplicatesTrue): 0",
        "table - choicesSelected(noDuplicatesFalse): 0",
        "totalChoicesSelected(noDuplicatesTrue) = 0",
        "totalChoicesSelected(noDuplicatesFalse) = 0",
        "choicesCount(noDuplicatesTrue) = 3",
        "choicesCount(noDuplicatesFalse) = 4",
        "choicesAvailable(noDuplicatesTrue) = 3",
        "choicesAvailable(noDuplicatesFalse) = 4",
        "choicesSelected(noDuplicatesTrue).count = 0",
        "choicesSelected(noDuplicatesFalse).count = 0",
        "choicesSelected(noDuplicatesTrue) = 0",
        "choicesSelected(noDuplicatesFalse) = 0"
      ),
      "choices-no-duplicate.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"      -> Many(List("0")),
        "1_employer"        -> One("Foo"),
        "2_addAnother"      -> Many(List("0")),
        "2_employer"        -> One("Bar"),
        "3_addAnother"      -> Many(List("0")),
        "3_employer"        -> One("Baz"),
        "4_addAnother"      -> Many(List("0")),
        "4_employer"        -> One("Foo"),
        "5_addAnother"      -> Many(List("1")),
        "5_employer"        -> One("Bar"),
        "noDuplicatesFalse" -> Many(List("employer_2", "employer_5")),
        "noDuplicatesTrue"  -> Many(List("employer_1", "employer_2"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "ap0.4.0",
        "ar0.4",
        "ap0.5.0",
        "ar0.5",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"        -> StringResult("Foo"),
        "2_employer"        -> StringResult("Bar"),
        "3_employer"        -> StringResult("Baz"),
        "4_employer"        -> StringResult("Foo"),
        "5_employer"        -> StringResult("Bar"),
        "noDuplicatesFalse" -> OptionResult(List("employer_2", "employer_5")),
        "noDuplicatesTrue"  -> OptionResult(List("employer_1", "employer_2"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "4 - Foo",
        "<p>4 - Foo</p>",
        "5 - Bar",
        "<p>5 - Bar</p>",
        "Foo",
        "Bar",
        "Baz",
        "Foo",
        "Bar",
        "Foo",
        "Bar",
        "Baz",
        "Foo",
        "Bar",
        "table - totalChoicesSelected(noDuplicatesTrue): 2",
        "table - totalChoicesSelected(noDuplicatesFalse): 2",
        "table - choicesCount(noDuplicatesTrue): 3",
        "table - choicesCount(noDuplicatesFalse): 5",
        "table - choicesAvailable(noDuplicatesTrue): 1",
        "table - choicesAvailable(noDuplicatesFalse): 3",
        "table - choicesSelected(noDuplicatesTrue).count: 2",
        "table - choicesSelected(noDuplicatesFalse).count: 2",
        "table - choicesSelected(noDuplicatesTrue): 2",
        "table - choicesSelected(noDuplicatesFalse): 2",
        "totalChoicesSelected(noDuplicatesTrue) = 2",
        "totalChoicesSelected(noDuplicatesFalse) = 2",
        "choicesCount(noDuplicatesTrue) = 3",
        "choicesCount(noDuplicatesFalse) = 5",
        "choicesAvailable(noDuplicatesTrue) = 1",
        "choicesAvailable(noDuplicatesFalse) = 3",
        "choicesSelected(noDuplicatesTrue).count = 2",
        "choicesSelected(noDuplicatesFalse).count = 2",
        "choicesSelected(noDuplicatesTrue) = 2",
        "choicesSelected(noDuplicatesFalse) = 2"
      ),
      "choices-no-duplicate.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnotherEmployer" -> Many(List("0")),
        "1_employer"           -> One("Foo"),
        "2_addAnotherEmployer" -> Many(List("1")),
        "2_employer"           -> One("Bar")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"     -> StringResult("Foo"),
        "1_typeOfIncome" -> Empty,
        "2_employer"     -> StringResult("Bar")
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "Foo",
        "Bar",
        "table - totalChoicesSelected(typeOfIncome): 0",
        "table - choicesCount(typeOfIncome): 5",
        "table - choicesAvailable(typeOfIncome): 5",
        "table - choicesSelected(typeOfIncome).count: 0",
        "table - choicesSelected(typeOfIncome): 0",
        "totalChoicesSelected(typeOfIncome) = 0",
        "choicesCount(typeOfIncome) = 5",
        "choicesAvailable(typeOfIncome) = 5",
        "choicesSelected(typeOfIncome).count = 0",
        "choicesSelected(typeOfIncome) = 0",
        "miniSummaryList - totalChoicesSelected(typeOfIncome): 0",
        "miniSummaryList - choicesCount(typeOfIncome): 5",
        "miniSummaryList - choicesAvailable(typeOfIncome): 5",
        "miniSummaryList - choicesSelected(typeOfIncome).count: 0",
        "miniSummaryList - choicesSelected(typeOfIncome): 0",
        "What payment did you make in ?",
        "1 -  - 0 :: 0 :: 0",
        "<p>1 - - 0 :: 0 :: 0</p>",
        "table - outside - totalChoicesSelected(typeOfIncome): 0",
        "table - outside - choicesCount(typeOfIncome): 5",
        "table - outside - choicesAvailable(typeOfIncome): 5",
        "table - outside - choicesSelected(typeOfIncome).count: 0",
        "table - outside - choicesSelected(typeOfIncome): 0"
      ),
      "choices-selected-total.json initial state"
    ),
    (
      MongoUserData(
        "1_addAnotherEmployer" -> Many(List("0")),
        "1_employer"           -> One("Foo"),
        "1_typeOfIncome"       -> Many(List("2026")),
        "2_addAnotherEmployer" -> Many(List("1")),
        "2_employer"           -> One("Bar")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap1.1.0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"     -> StringResult("Foo"),
        "1_typeOfIncome" -> OptionResult(List("2026")),
        "2_employer"     -> StringResult("Bar")
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "Foo",
        "Bar",
        "table - totalChoicesSelected(typeOfIncome): 0",
        "table - choicesCount(typeOfIncome): 5",
        "table - choicesAvailable(typeOfIncome): 5",
        "table - choicesSelected(typeOfIncome).count: 1",
        "table - choicesSelected(typeOfIncome): 1",
        "totalChoicesSelected(typeOfIncome) = 1",
        "choicesCount(typeOfIncome) = 5",
        "choicesAvailable(typeOfIncome) = 4",
        "choicesSelected(typeOfIncome).count = 1",
        "choicesSelected(typeOfIncome) = 1",
        "miniSummaryList - totalChoicesSelected(typeOfIncome): 1",
        "miniSummaryList - choicesCount(typeOfIncome): 5",
        "miniSummaryList - choicesAvailable(typeOfIncome): 4",
        "miniSummaryList - choicesSelected(typeOfIncome).count: 1",
        "miniSummaryList - choicesSelected(typeOfIncome): 1",
        "What payment did you make in 2025 to 26?",
        "1 - 2025 to 26 - 1 :: 1 :: 1",
        "<p>1 - 2025 to 26 - 1 :: 1 :: 1</p>",
        "table - outside - totalChoicesSelected(typeOfIncome): 1",
        "table - outside - choicesCount(typeOfIncome): 5",
        "table - outside - choicesAvailable(typeOfIncome): 4",
        "table - outside - choicesSelected(typeOfIncome).count: 1",
        "table - outside - choicesSelected(typeOfIncome): 1"
      ),
      "choices-selected-total.json first iteration in progress"
    ),
    (
      MongoUserData(
        "1_addAnother"         -> Many(List("0")),
        "1_addAnotherEmployer" -> Many(List("0")),
        "1_amount"             -> One("123"),
        "1_employer"           -> One("Foo"),
        "1_typeOfIncome"       -> Many(List("2026")),
        "2_addAnotherEmployer" -> Many(List("1")),
        "2_employer"           -> One("Bar"),
        "2_typeOfIncome"       -> Many(List("employer_2", "other", "none"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap1.1.0",
        "ap1.1.1",
        "ar1.1",
        "ap1.2.0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"     -> StringResult("Foo"),
        "1_typeOfIncome" -> OptionResult(List("2026")),
        "2_employer"     -> StringResult("Bar"),
        "2_typeOfIncome" -> OptionResult(List("employer_2", "other", "none"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "Foo",
        "Bar",
        "table - totalChoicesSelected(typeOfIncome): 3",
        "table - choicesCount(typeOfIncome): 5",
        "table - choicesAvailable(typeOfIncome): 2",
        "table - choicesSelected(typeOfIncome).count: 4",
        "table - choicesSelected(typeOfIncome): 1",
        "totalChoicesSelected(typeOfIncome) = 4",
        "choicesCount(typeOfIncome) = 5",
        "choicesAvailable(typeOfIncome) = 1",
        "choicesSelected(typeOfIncome).count = 4",
        "choicesSelected(typeOfIncome) = 1",
        "miniSummaryList - totalChoicesSelected(typeOfIncome): 4",
        "miniSummaryList - choicesCount(typeOfIncome): 5",
        "miniSummaryList - choicesAvailable(typeOfIncome): 1",
        "miniSummaryList - choicesSelected(typeOfIncome).count: 4",
        "miniSummaryList - choicesSelected(typeOfIncome): 4",
        "What payment did you make in 2025 to 26?",
        "1 - 2025 to 26 - 4 :: 1 :: 4",
        "<p>1 - 2025 to 26 - 4 :: 1 :: 4</p>",
        "Foo",
        "Bar",
        "table - totalChoicesSelected(typeOfIncome): 1",
        "table - choicesCount(typeOfIncome): 5",
        "table - choicesAvailable(typeOfIncome): 4",
        "table - choicesSelected(typeOfIncome).count: 4",
        "table - choicesSelected(typeOfIncome): 3",
        "totalChoicesSelected(typeOfIncome) = 4",
        "choicesCount(typeOfIncome) = 5",
        "choicesAvailable(typeOfIncome) = 1",
        "choicesSelected(typeOfIncome).count = 4",
        "choicesSelected(typeOfIncome) = 3",
        "miniSummaryList - totalChoicesSelected(typeOfIncome): 4",
        "miniSummaryList - choicesCount(typeOfIncome): 5",
        "miniSummaryList - choicesAvailable(typeOfIncome): 1",
        "miniSummaryList - choicesSelected(typeOfIncome).count: 4",
        "miniSummaryList - choicesSelected(typeOfIncome): 4",
        "What payment did you make in Bar, Other, None?",
        "2 - Bar, Other, None - 4 :: 3 :: 4",
        "<p>2 - Bar, Other, None - 4 :: 3 :: 4</p>",
        "table - outside - totalChoicesSelected(typeOfIncome): 4",
        "table - outside - choicesCount(typeOfIncome): 5",
        "table - outside - choicesAvailable(typeOfIncome): 1",
        "table - outside - choicesSelected(typeOfIncome).count: 4",
        "table - outside - choicesSelected(typeOfIncome): 4"
      ),
      "choices-selected-total.json second iteration in progress"
    ),
    (
      MongoUserData(
        "1_addAnother"         -> Many(List("0")),
        "1_addAnotherEmployer" -> Many(List("0")),
        "1_employer"           -> One("Foo"),
        "1_typeOfIncome"       -> Many(List("employer_1", "employer_2")),
        "2_addAnother"         -> Many(List("0")),
        "2_addAnotherEmployer" -> Many(List("0")),
        "2_employer"           -> One("Bar"),
        "2_typeOfIncome"       -> Many(List("other", "none")),
        "3_addAnother"         -> Many(List("0")),
        "3_addAnotherEmployer" -> Many(List("1")),
        "3_employer"           -> One("Baz"),
        "3_typeOfIncome"       -> Many(List("employer_1", "employer_2", "employer_3", "other")),
        "4_addAnother"         -> Many(List("1")),
        "4_typeOfIncome"       -> Many(List("employer_1", "employer_2", "none", "employer_3", "other"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "ap1.4.0",
        "ar1.4",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"     -> StringResult("Foo"),
        "1_secret"       -> Empty,
        "1_typeOfIncome" -> OptionResult(List("employer_1", "employer_2")),
        "2_employer"     -> StringResult("Bar"),
        "2_secret"       -> Empty,
        "2_typeOfIncome" -> OptionResult(List("other", "none")),
        "3_employer"     -> StringResult("Baz"),
        "3_secret"       -> Empty,
        "3_typeOfIncome" -> OptionResult(List("employer_1", "employer_2", "employer_3", "other")),
        "4_secret"       -> Empty,
        "4_typeOfIncome" -> OptionResult(List("employer_1", "employer_2", "none", "employer_3", "other"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Other source of income 1 / 13 :: 2",
        "Foo",
        "Bar",
        "Baz",
        "Choices selected: 13 :: 2",
        "<p>Choices selected: 13 :: 2</p>",
        "1 - Foo, Bar - 13 :: 2",
        "<p>1 - Foo, Bar - 13 :: 2</p>",
        "Other source of income 2 / 13 :: 2",
        "Foo",
        "Bar",
        "Baz",
        "Choices selected: 13 :: 2",
        "<p>Choices selected: 13 :: 2</p>",
        "2 - Other, None - 13 :: 2",
        "<p>2 - Other, None - 13 :: 2</p>",
        "Other source of income 3 / 13 :: 4",
        "Foo",
        "Bar",
        "Baz",
        "Choices selected: 13 :: 4",
        "<p>Choices selected: 13 :: 4</p>",
        "3 - Foo, Bar, Baz, Other - 13 :: 4",
        "<p>3 - Foo, Bar, Baz, Other - 13 :: 4</p>",
        "Other source of income 4 / 13 :: 5",
        "Foo",
        "Bar",
        "Baz",
        "Choices selected: 13 :: 5",
        "<p>Choices selected: 13 :: 5</p>",
        "4 - Foo, Bar, None, Baz, Other - 13 :: 5",
        "<p>4 - Foo, Bar, None, Baz, Other - 13 :: 5</p>",
        "Choices selected: 13 :: 13",
        "<p>Choices selected: 13 :: 13</p>"
      ),
      "choices-selected-count.json secrets are visible"
    ),
    (
      MongoUserData(
        "1_addAnother"         -> Many(List("0")),
        "1_addAnotherEmployer" -> Many(List("0")),
        "1_employer"           -> One("Foo"),
        "1_typeOfIncome"       -> Many(List("employer_1", "employer_2")),
        "2_addAnother"         -> Many(List("1")),
        "2_addAnotherEmployer" -> Many(List("0")),
        "2_employer"           -> One("Bar"),
        "2_typeOfIncome"       -> Many(List("employer_3", "other")),
        "3_addAnotherEmployer" -> Many(List("1")),
        "3_employer"           -> One("Baz"),
        "dummy"                -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"     -> StringResult("Foo"),
        "1_secret"       -> Hidden,
        "1_typeOfIncome" -> OptionResult(List("employer_1", "employer_2")),
        "2_employer"     -> StringResult("Bar"),
        "2_secret"       -> Hidden,
        "2_typeOfIncome" -> OptionResult(List("employer_3", "other")),
        "3_employer"     -> StringResult("Baz")
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Other source of income 1 / 4 :: 2",
        "Foo",
        "Bar",
        "Baz",
        "Choices selected: 4 :: 2",
        "<p>Choices selected: 4 :: 2</p>",
        "1 - Foo, Bar - 4 :: 2",
        "<p>1 - Foo, Bar - 4 :: 2</p>",
        "Other source of income 2 / 4 :: 2",
        "Foo",
        "Bar",
        "Baz",
        "Choices selected: 4 :: 2",
        "<p>Choices selected: 4 :: 2</p>",
        "2 - Baz, Other - 4 :: 2",
        "<p>2 - Baz, Other - 4 :: 2</p>",
        "Choices selected: 4 :: 4",
        "<p>Choices selected: 4 :: 4</p>"
      ),
      "choices-selected-count.json secrets are hidden"
    ),
    (
      MongoUserData(
        "1_addAnother"         -> Many(List("0")),
        "1_addAnotherEmployer" -> Many(List("0")),
        "1_employer"           -> One("Foo"),
        "1_typeOfIncome"       -> Many(List("employer_1", "employer_2")),
        "2_addAnother"         -> Many(List("1")),
        "2_addAnotherEmployer" -> Many(List("0")),
        "2_employer"           -> One("Bar"),
        "2_typeOfIncome"       -> Many(List("other", "none")),
        "3_addAnotherEmployer" -> Many(List("1")),
        "3_employer"           -> One("Baz"),
        "visibilityOptions"    -> Many(List("showAll"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "ap2.1.0",
        "ap2.1.1",
        "ar2.1",
        "ap2.2.0",
        "ap2.2.1",
        "ar2.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_secret"          -> Empty,
        "1_employer"        -> StringResult("Foo"),
        "1_typeOfIncome"    -> OptionResult(List("employer_1", "employer_2")),
        "2_secret"          -> Empty,
        "2_employer"        -> StringResult("Bar"),
        "2_typeOfIncome"    -> OptionResult(List("other", "none")),
        "3_employer"        -> StringResult("Baz"),
        "visibilityOptions" -> OptionResult(List("showAll"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Other source of income 1 / 1",
        "Foo",
        "Bar",
        "Baz",
        "Choices available: 3",
        "<p>Choices available: 3</p>",
        "Secret choices available: 3",
        "<p>Secret choices available: 3</p>",
        "1 - Foo, Bar - 1",
        "<p>1 - Foo, Bar - 1</p>",
        "Other source of income 2 / 1",
        "Foo",
        "Bar",
        "Baz",
        "Choices available: 3",
        "<p>Choices available: 3</p>",
        "Secret choices available: 3",
        "<p>Secret choices available: 3</p>",
        "2 - Other, None - 1",
        "<p>2 - Other, None - 1</p>"
      ),
      "choices-available-conditional.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"         -> Many(List("1")),
        "1_addAnotherEmployer" -> Many(List("0")),
        "1_employer"           -> One("Foo"),
        "1_typeOfIncome"       -> Many(List("employer_1")),
        "2_addAnotherEmployer" -> Many(List("0")),
        "2_employer"           -> One("Bar"),
        "3_addAnotherEmployer" -> Many(List("1")),
        "3_employer"           -> One("Baz"),
        "visibilityOptions"    -> Many(List("showAll"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "ap2.1.0",
        "ar2.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"        -> StringResult("Foo"),
        "1_secret"          -> Hidden,
        "1_typeOfIncome"    -> OptionResult(List("employer_1")),
        "2_employer"        -> StringResult("Bar"),
        "3_employer"        -> StringResult("Baz"),
        "visibilityOptions" -> OptionResult(List("showAll"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Other source of income 1 / 4",
        "Foo",
        "Bar",
        "Baz",
        "Choices available: 5",
        "<p>Choices available: 5</p>",
        "1 - Foo - 4",
        "<p>1 - Foo - 4</p>"
      ),
      "choices-available-conditional.json one option selected"
    ),
    (
      MongoUserData(
        "1_addAnother"         -> Many(List("0")),
        "1_addAnotherEmployer" -> Many(List("0")),
        "1_employer"           -> One("Foo"),
        "1_typeOfIncome"       -> Many(List("employer_1")),
        "2_addAnother"         -> Many(List("1")),
        "2_addAnotherEmployer" -> Many(List("0")),
        "2_employer"           -> One("Bar"),
        "2_typeOfIncome"       -> Many(List("employer_2", "employer_3")),
        "3_addAnotherEmployer" -> Many(List("1")),
        "3_employer"           -> One("Baz"),
        "visibilityOptions"    -> Many(List("showAll"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "ap2.1.0",
        "ar2.1",
        "ap2.2.0",
        "ar2.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"        -> StringResult("Foo"),
        "1_secret"          -> Hidden,
        "1_typeOfIncome"    -> OptionResult(List("employer_1")),
        "2_employer"        -> StringResult("Bar"),
        "2_secret"          -> Hidden,
        "2_typeOfIncome"    -> OptionResult(List("employer_2", "employer_3")),
        "3_employer"        -> StringResult("Baz"),
        "visibilityOptions" -> OptionResult(List("showAll"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Other source of income 1 / 2",
        "Foo",
        "Bar",
        "Baz",
        "Choices available: 3",
        "<p>Choices available: 3</p>",
        "1 - Foo - 2",
        "<p>1 - Foo - 2</p>",
        "Other source of income 2 / 2",
        "Foo",
        "Bar",
        "Baz",
        "Choices available: 4",
        "<p>Choices available: 4</p>",
        "2 - Bar, Baz - 2",
        "<p>2 - Bar, Baz - 2</p>"
      ),
      "choices-available-conditional.json three options selected"
    ),
    (
      MongoUserData(
        "1_addAnother"         -> Many(List("0")),
        "1_addAnotherEmployer" -> Many(List("0")),
        "1_employer"           -> One("Foo"),
        "1_typeOfIncome"       -> Many(List("employer_1", "employer_2")),
        "2_addAnother"         -> Many(List("0")),
        "2_addAnotherEmployer" -> Many(List("0")),
        "2_employer"           -> One("Bar"),
        "2_typeOfIncome"       -> Many(List("employer_3")),
        "3_addAnother"         -> Many(List("1")),
        "3_addAnotherEmployer" -> Many(List("1")),
        "3_employer"           -> One("Baz"),
        "3_typeOfIncome"       -> Many(List("other")),
        "visibilityOptions"    -> Many(List("showAll"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2",
        "ap1.3.0",
        "ar1.3",
        "ap2.1.0",
        "ar2.1",
        "ap2.2.0",
        "ar2.2",
        "ap2.3.0",
        "ar2.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_employer"        -> StringResult("Foo"),
        "1_secret"          -> Empty,
        "1_typeOfIncome"    -> OptionResult(List("employer_1", "employer_2")),
        "2_employer"        -> StringResult("Bar"),
        "2_secret"          -> Empty,
        "2_typeOfIncome"    -> OptionResult(List("employer_3")),
        "3_employer"        -> StringResult("Baz"),
        "3_secret"          -> Empty,
        "3_typeOfIncome"    -> OptionResult(List("other")),
        "visibilityOptions" -> OptionResult(List("showAll"))
      ),
      List(
        "1 - Foo",
        "<p>1 - Foo</p>",
        "2 - Bar",
        "<p>2 - Bar</p>",
        "3 - Baz",
        "<p>3 - Baz</p>",
        "Other source of income 1 / 1",
        "Foo",
        "Bar",
        "Baz",
        "Choices available: 3",
        "<p>Choices available: 3</p>",
        "Secret choices available: 3",
        "<p>Secret choices available: 3</p>",
        "1 - Foo, Bar - 1",
        "<p>1 - Foo, Bar - 1</p>",
        "Other source of income 2 / 1",
        "Foo",
        "Bar",
        "Baz",
        "Choices available: 2",
        "<p>Choices available: 2</p>",
        "Secret choices available: 2",
        "<p>Secret choices available: 2</p>",
        "2 - Baz - 1",
        "<p>2 - Baz - 1</p>",
        "Other source of income 3 / 1",
        "Foo",
        "Bar",
        "Baz",
        "Choices available: 2",
        "<p>Choices available: 2</p>",
        "Secret choices available: 2",
        "<p>Secret choices available: 2</p>",
        "3 - Other - 1",
        "<p>3 - Other - 1</p>"
      ),
      "choices-available-conditional.json four (all) options selected"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("1")),
        "1_textArea"   -> One("First sentence.\n\nSecond sentence.\nThird sentence.")
      ),
      List(
        "ap0.1.0",
        "ar0.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_textArea" -> StringResult("""|First sentence.
                                        |
                                        |Second sentence.
                                        |Third sentence.""".stripMargin),
        "secret"     -> Hidden
      ),
      List(
        """|<strong>Details</strong>
           |
           |First sentence.<br><br>Second sentence.<br>Third sentence.""".stripMargin,
        """|<p><strong>Details</strong></p>
           |<p>First sentence.<br><br>
           | Second sentence.<br>
           | Third sentence.</p>""".stripMargin
      ),
      "display-as-entered.json Generated"
    ),
    (
      MongoUserData(
        "1_addRoomChoice"    -> Many(List("1")),
        "1_newTenancyChoice" -> Many(List("0")),
        "otherAddresses"     -> Many(List("1")),
        "ukAddress-filter"   -> One(""),
        "ukAddress-postcode" -> One("ZZ9Z 9TT")
      ),
      List(
        "n0",
        "n1",
        "ap3.1.1",
        "ar3.1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          selectedAddresses = Some(
            Map(
              FormComponentId("ukAddress") -> "GB990091234558"
            )
          ),
          postcodeLookup = Some(
            Map(
              FormComponentId("ukAddress") -> mkAddressLookupResult(
                id = "GB990091234558",
                lines = List("1 Testing Lane"),
                town = "Royal Madeuptown",
                postcode = "ZZ9Z 9TT"
              )
            )
          )
        )
      ),
      AnswerMap(
        "1_addAddressChoice"           -> Hidden,
        "otherAddresses"               -> OptionResult(List("1")),
        "1_ukAddressProperty-filter"   -> Hidden,
        "1_ukAddressProperty-postcode" -> Hidden,
        "1_whichAddressChoice"         -> Hidden
      ),
      List(
        "You have added 1 room. Addresses count 0",
        "",
        "Room 1 rented in: 1 Testing Lane, Royal Madeuptown, ZZ9Z 9TT",
        """|<p>Room 1 rented in: 1 Testing Lane<br>
           | Royal Madeuptown<br>
           | ZZ9Z 9TT</p>""".stripMargin
      ),
      "choice-dynamic-atl-based-adress.json room is in UK address"
    ),
    (
      MongoUserData(
        "1_addAddressChoice"           -> Many(List("1")),
        "1_addRoomChoice"              -> Many(List("1")),
        "1_newTenancyChoice"           -> Many(List("1")),
        "1_ukAddressProperty"          -> One(""),
        "1_ukAddressProperty-filter"   -> One(""),
        "1_ukAddressProperty-postcode" -> One("FX1A 7GA"),
        "otherAddresses"               -> Many(List("0")),
        "ukAddress-filter"             -> One(""),
        "ukAddress-postcode"           -> One("ZZ9Z 9TT")
      ),
      List(
        "n0",
        "n1",
        "ap2.1.0",
        "ar2.1",
        "ap3.1.1",
        "ar3.1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          selectedAddresses = Some(
            Map(
              FormComponentId("ukAddress")           -> "GB990091234558",
              FormComponentId("1_ukAddressProperty") -> "GB990091234593"
            )
          ),
          postcodeLookup = Some(
            Map(
              FormComponentId("ukAddress") -> mkAddressLookupResult(
                id = "GB990091234558",
                lines = List("1 Testing Lane"),
                town = "Royal Madeuptown",
                postcode = "ZZ9Z 9TT"
              ),
              FormComponentId("1_ukAddressProperty") -> mkAddressLookupResult(
                id = "GB990091234593",
                lines = List("10 Other Place", "Some District"),
                town = "Anytown",
                postcode = "FX1A 7GA"
              )
            )
          )
        )
      ),
      AnswerMap(
        "1_addAddressChoice"           -> OptionResult(List("1")),
        "otherAddresses"               -> OptionResult(List("0")),
        "1_ukAddressProperty-filter"   -> StringResult(""),
        "1_ukAddressProperty-postcode" -> StringResult("FX1A 7GA"),
        "1_whichAddressChoice"         -> Hidden
      ),
      List(
        "You have added 1 address",
        "10 Other Place, Some District, Anytown, FX1A 7GA",
        """|<p>10 Other Place<br>
           | Some District<br>
           | Anytown<br>
           | FX1A 7GA</p>""".stripMargin,
        "You have added 1 room. Addresses count 1",
        "",
        "Room 1 rented in: 10 Other Place, Some District, Anytown, FX1A 7GA",
        """|<p>Room 1 rented in: 10 Other Place<br>
           | Some District<br>
           | Anytown<br>
           | FX1A 7GA</p>""".stripMargin
      ),
      "choice-dynamic-atl-based-adress.json room is in other single address"
    ),
    (
      MongoUserData(
        "1_addAddressChoice"           -> Many(List("0")),
        "1_addRoomChoice"              -> Many(List("1")),
        "1_newTenancyChoice"           -> Many(List("0")),
        "1_ukAddressProperty-filter"   -> One(""),
        "1_ukAddressProperty-postcode" -> One("FX1A 7GA"),
        "1_whichAddressChoice"         -> Many(List("ukAddressProperty_1")),
        "2_addAddressChoice"           -> Many(List("1")),
        "2_ukAddressProperty-filter"   -> One(""),
        "2_ukAddressProperty-postcode" -> One("FX1A 7GA"),
        "otherAddresses"               -> Many(List("0")),
        "ukAddress-filter"             -> One(""),
        "ukAddress-postcode"           -> One("ZZ9Z 9TT")
      ),
      List(
        "n0",
        "n1",
        "ap2.1.0",
        "ar2.1",
        "ap2.2.0",
        "ar2.2",
        "ap3.1.0",
        "ap3.1.1",
        "ar3.1"
      ),
      EvaluationContext.empty.copy(
        thirdPartyData = ThirdPartyData.empty.copy(
          selectedAddresses = Some(
            Map(
              FormComponentId("ukAddress")           -> "GB990091234558",
              FormComponentId("1_ukAddressProperty") -> "GB990091234588",
              FormComponentId("2_ukAddressProperty") -> "GB990091234589"
            )
          ),
          postcodeLookup = Some(
            Map(
              FormComponentId("ukAddress") -> mkAddressLookupResult(
                id = "GB990091234558",
                lines = List("1 Testing Lane"),
                town = "Royal Madeuptown",
                postcode = "ZZ9Z 9TT"
              ),
              FormComponentId("1_ukAddressProperty") -> mkAddressLookupResult(
                id = "GB990091234588",
                lines = List("Flat 1", "7 Other Place", "Some District"),
                town = "Anytown",
                postcode = "FX1A 7GA"
              ),
              FormComponentId("2_ukAddressProperty") -> mkAddressLookupResult(
                id = "GB990091234589",
                lines = List("Flat 2", "7 Other Place", "Some District"),
                town = "Anytown",
                postcode = "FX1A 7GA"
              )
            )
          )
        )
      ),
      AnswerMap(
        "1_addAddressChoice"           -> OptionResult(List("0")),
        "1_ukAddressProperty-filter"   -> StringResult(""),
        "1_ukAddressProperty-postcode" -> StringResult("FX1A 7GA"),
        "1_whichAddressChoice"         -> OptionResult(List("ukAddressProperty_1")),
        "2_addAddressChoice"           -> OptionResult(List("1")),
        "2_ukAddressProperty-filter"   -> StringResult(""),
        "2_ukAddressProperty-postcode" -> StringResult("FX1A 7GA"),
        "otherAddresses"               -> OptionResult(List("0"))
      ),
      List(
        "You have added 2 addresses",
        "Flat 1, 7 Other Place, Some District, Anytown, FX1A 7GA",
        """|<p>Flat 1<br>
           | 7 Other Place<br>
           | Some District<br>
           | Anytown<br>
           | FX1A 7GA</p>""".stripMargin,
        "You have added 2 addresses",
        "Flat 2, 7 Other Place, Some District, Anytown, FX1A 7GA",
        """|<p>Flat 2<br>
           | 7 Other Place<br>
           | Some District<br>
           | Anytown<br>
           | FX1A 7GA</p>""".stripMargin,
        "Flat 1, 7 Other Place, Some District, Anytown, FX1A 7GA",
        "Flat 2, 7 Other Place, Some District, Anytown, FX1A 7GA",
        "You have added 1 room. Addresses count 2",
        "Flat 1, 7 Other Place, Some District, Anytown, FX1A 7GA",
        "Room 1 rented in: Flat 1, 7 Other Place, Some District, Anytown, FX1A 7GA",
        """|<p>Room 1 rented in: Flat 1<br>
           | 7 Other Place<br>
           | Some District<br>
           | Anytown<br>
           | FX1A 7GA</p>""".stripMargin
      ),
      "choice-dynamic-atl-based-adress.json room is in one of the other addresses"
    )
  )
}
