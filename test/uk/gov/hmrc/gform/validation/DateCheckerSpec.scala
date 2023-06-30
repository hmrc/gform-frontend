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

package uk.gov.hmrc.gform.validation

import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.implicits._
import munit.FunSuite
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.Millis
import org.scalatest.time.Span
import play.api.Configuration
import play.api.Environment
import play.api.http.HttpConfiguration
import play.api.i18n._
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.FormModelSupport
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.VariadicFormDataSupport
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.ids.IndexedComponentId
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DateCheckerSpec extends FunSuite with FormModelSupport with VariadicFormDataSupport with ScalaFutures {

  override implicit val patienceConfig =
    PatienceConfig(timeout = scaled(Span(15000, Millis)), interval = scaled(Span(15, Millis)))

  implicit val l: LangADT = LangADT.En

  private val lookupRegistry = new LookupRegistry(Map.empty)

  implicit val smartStringEvaluator: SmartStringEvaluator = (s: SmartString, markDown: Boolean) =>
    s.rawValue(LangADT.En)

  implicit class FormComponentOps(formComponent: FormComponent) {
    def withErrorFields(
      errorShortName: Option[SmartString],
      errorShortNameStart: Option[SmartString],
      errorExample: Option[SmartString]
    ): FormComponent =
      formComponent.copy(
        errorShortName = errorShortName,
        errorShortNameStart = errorShortNameStart,
        errorExample = errorExample
      )
  }
  val environment = Environment.simple()
  val configuration = Configuration.load(environment)
  val langs = new DefaultLangs()
  val httpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)

  val messagesApi: MessagesApi =
    new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get
  implicit val messages: Messages = messagesApi.preferred(Seq(langs.availables.head))

  private def mkComponentsValidator(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    formComponent: FormComponent,
    cacheData: CacheData
  ): ComponentsValidator[DataOrigin.Mongo, Future] =
    new ComponentsValidator(
      formModelVisibilityOptics,
      formComponent,
      cacheData,
      EnvelopeWithMapping.empty,
      lookupRegistry,
      booleanExprEval,
      ComponentChecker.NonShortCircuitInterpreter
    )

  private def componentsValidator(
    formTemplate: FormTemplate,
    formComponent: FormComponent,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ) = {

    val fmb = mkFormModelFromSections(formTemplate.formKind.allSections.sections)

    val fmvo = fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

    val cacheData = new CacheData(
      EnvelopeId(""),
      ThirdPartyData.empty,
      formTemplate
    )

    mkComponentsValidator(fmvo, formComponent, cacheData)
  }

  val table1: List[
    (
      DateConstraint,
      LocalDate,
      Option[SmartString],
      Option[SmartString],
      Option[SmartString],
      ValidatedType[Unit],
      String
    )
  ] = List(
    (
      DateConstraint(After, Today, OffsetDate(1)),
      LocalDate.now().plusDays(2),
      None,
      None,
      None,
      Valid(()),
      "After Today 1 should accepts dates after tomorrow"
    ),
    (
      DateConstraint(After, Today, OffsetDate(0)),
      LocalDate.now().plusDays(1),
      None,
      None,
      None,
      Valid(()),
      "After Today 0 should accepts dates after today"
    ),
    (
      DateConstraint(After, ConcreteDate(Year.Exact(2017), Month.Exact(6), Day.Exact(16)), OffsetDate(5)),
      LocalDate.of(2017, 6, 16).plusDays(6),
      None,
      None,
      None,
      Valid(()),
      "After 2017-06-16 5 should accepts dates after 2017-06-21"
    ),
    (
      DateConstraint(After, ConcreteDate(Year.Exact(2017), Month.Exact(6), Day.Exact(16)), OffsetDate(5)),
      LocalDate.of(2017, 6, 16).plusDays(2),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date must be after 21 June 2017"
          )
        )
      ),
      "validation for After 2017-06-16 5 should return invalid for dates that aren't after 2017-06-21, AC20"
    ),
    (
      DateConstraint(After, ConcreteDate(Year.Exact(2017), Month.Exact(6), Day.Exact(16)), OffsetDate(5)),
      LocalDate.of(2017, 6, 16).plusDays(2),
      None,
      Some(toSmartString("Date of birth")),
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date of birth must be after 21 June 2017"
          )
        )
      ),
      "validation for After 2017-06-16 5 should return invalid for dates that aren't after 2017-06-21, AC21"
    ),
    (
      DateConstraint(Before, ConcreteDate(Year.Exact(2020), Month.Exact(1), Day.Exact(1)), OffsetDate(0)),
      LocalDate.of(2020, 1, 1),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date must be before 01 January 2020"
          )
        )
      ),
      "validation for Before 2020-01-01 should return invalid for dates that aren't before 2020-01-01, AC22"
    ),
    (
      DateConstraint(Before, ConcreteDate(Year.Exact(2020), Month.Exact(1), Day.Exact(1)), OffsetDate(0)),
      LocalDate.of(2020, 1, 1),
      None,
      Some(toSmartString("Date of birth")),
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date of birth must be before 01 January 2020"
          )
        )
      ),
      "validation for Before 2020-01-01 should return invalid for dates that aren't before 2020-01-01, AC23"
    ),
    (
      DateConstraint(After, Today, OffsetDate(-1)),
      LocalDate.now(),
      None,
      None,
      None,
      Valid(()),
      "After Today -1 should accepts today and dates in future"
    ),
    (
      DateConstraint(After, ConcreteDate(Year.Exact(2017), Month.Exact(6), Day.Exact(16)), OffsetDate(-5)),
      LocalDate.of(2017, 6, 16).plusDays(-4),
      None,
      None,
      None,
      Valid(()),
      "After 2017-06-16 -5 should accepts dates after 2017-06-11"
    ),
    (
      DateConstraint(Before, Today, OffsetDate(1)),
      LocalDate.now(),
      None,
      None,
      None,
      Valid(()),
      "Before Today 1 should accepts today and dates in past"
    ),
    (
      DateConstraint(Before, Today, OffsetDate(0)),
      LocalDate.now().plusDays(-1),
      None,
      None,
      None,
      Valid(()),
      "Before Today 0 should accepts dates before today"
    ),
    (
      DateConstraint(Before, Today, OffsetDate(0)),
      LocalDate.now(),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date must be in the past"
          )
        )
      ),
      "Before Today 0 should reject today, AC12"
    ),
    (
      DateConstraint(Before, Today, OffsetDate(0)),
      LocalDate.now(),
      None,
      Some(toSmartString("Date of birth")),
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date of birth must be in the past"
          )
        )
      ),
      "Before Today 0 should reject today, AC13"
    ),
    (
      DateConstraint(Before, Today, OffsetDate(1)),
      LocalDate.now().plusDays(1),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date must be today or in the past"
          )
        )
      ),
      "Before Today 0 should reject today, AC14"
    ),
    (
      DateConstraint(Before, Today, OffsetDate(1)),
      LocalDate.now().plusDays(1),
      None,
      Some(toSmartString("Date of birth")),
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date of birth must be today or in the past"
          )
        )
      ),
      "Before Today 0 should reject today, AC15"
    ),
    (
      DateConstraint(After, Today, OffsetDate(0)),
      LocalDate.now(),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date must be in the future"
          )
        )
      ),
      "After Today 0 should reject today, AC16"
    ),
    (
      DateConstraint(After, Today, OffsetDate(0)),
      LocalDate.now(),
      None,
      Some(toSmartString("Start date")),
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Start date must be in the future"
          )
        )
      ),
      "After Today 0 should reject today, AC17"
    ),
    (
      DateConstraint(After, Today, OffsetDate(-1)),
      LocalDate.now().minusDays(1),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date must be today or in the future"
          )
        )
      ),
      "After Today 0 should reject today, AC18"
    ),
    (
      DateConstraint(After, Today, OffsetDate(-1)),
      LocalDate.now().minusDays(1),
      None,
      Some(toSmartString("Start date")),
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Start date must be today or in the future"
          )
        )
      ),
      "After Today 0 should reject today, AC19"
    ),
    (
      DateConstraint(Before, Today, OffsetDate(-1)),
      LocalDate.now().plusDays(-2),
      None,
      None,
      None,
      Valid(()),
      "Before Today -1 should accepts dates before yesterday"
    ),
    (
      DateConstraint(Before, ConcreteDate(Year.Exact(2017), Month.Exact(6), Day.Exact(16)), OffsetDate(-5)),
      LocalDate.of(2017, 6, 16).plusDays(-6),
      None,
      None,
      None,
      Valid(()),
      "Before 2017-06-16 -5 should accepts dates before 2017-06-11"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Exact(4), Day.Any), OffsetDate(0)),
      LocalDate.of(2017, 4, 16),
      None,
      None,
      None,
      Valid(()),
      "Precisely YYYY-04-DD should accept any date that in April"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Last), OffsetDate(0)),
      LocalDate.of(2020, 2, 29),
      None,
      None,
      None,
      Valid(()),
      "Precisely YYYY-MM-lastDay should accept any date that is the last day of the given month"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Next, Month.Any, Day.Any), OffsetDate(0)),
      LocalDate.of(LocalDate.now().getYear + 1, 2, 28),
      None,
      None,
      None,
      Valid(()),
      "Precisely next-MM-DD should accept any date that is next year"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Last), OffsetDate(0)),
      LocalDate.of(2020, 2, 26),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date must be the last day of the month"
          )
        )
      ),
      "Date 26-02-2020 should return Is not Valid when lastDay validation is applied"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.First), OffsetDate(0)),
      LocalDate.of(2020, 2, 26),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date must be the first day of the month"
          )
        )
      ),
      "Date 26-02-2020 should return Is not Valid when firstDay validation is applied, AC24"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.First), OffsetDate(0)),
      LocalDate.of(2020, 2, 26),
      None,
      Some(toSmartString("Accounting period start date")),
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Accounting period start date must be the first day of the month"
          )
        )
      ),
      "Date 26-02-2020 should return Is not Valid when firstDay validation is applied, AC25"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Last), OffsetDate(0)),
      LocalDate.of(2020, 2, 26),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date must be the last day of the month"
          )
        )
      ),
      "Date 26-02-2020 should return Is not Valid when lastDay validation is applied, AC26"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Last), OffsetDate(0)),
      LocalDate.of(2020, 2, 26),
      None,
      Some(toSmartString("Accounting period end date")),
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Accounting period end date must be the last day of the month"
          )
        )
      ),
      "Date 26-02-2020 should return Is not Valid when lastDay validation is applied, AC27"
    )
  )

  (
    table1.zipWithIndex
      .traverse[Future, Unit] {
        case (
              (dateConstraint, acceptedAfter, errorShortName, errorShortNameStart, errorExample, expected, description),
              index
            ) =>
          val dateConstraints = List(dateConstraint)
          val constraints = DateConstraints(dateConstraints)
          val date = Date(constraints, Offset(0), None)

          val fieldValue =
            mkFormComponent("accPeriodStartDate", date)
              .withErrorFields(errorShortName, errorShortNameStart, errorExample)

          val data = variadicFormData[SourceOrigin.OutOfDate](
            "accPeriodStartDate-day"   -> acceptedAfter.getDayOfMonth.toString,
            "accPeriodStartDate-month" -> acceptedAfter.getMonthValue.toString,
            "accPeriodStartDate-year"  -> acceptedAfter.getYear.toString
          )

          val obtainedF: Future[ValidatedType[Unit]] =
            componentsValidator(mkFormTemplate(mkSection(fieldValue)), fieldValue, data)
              .validate(GetEmailCodeFieldMatcher.noop)

          obtainedF.map { obtained =>
            test(s"$index. $description") {
              assertEquals(obtained, expected)
            }
          }
      }
    )
    .futureValue

  val table2: List[
    (
      DateConstraint,
      (String, String, String),
      Option[SmartString],
      Option[SmartString],
      Option[SmartString],
      ValidatedType[Unit],
      String
    )
  ] = List(
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("35", "12", "2017"),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Enter a day in the correct format"
          )
        )
      ),
      "Date 35-12-2017 should return Is not Valid, AC3"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("x", "12", "2017"),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Enter a day in the correct format"
          )
        )
      ),
      "Date x-12-2017 should return Is not Valid, AC4"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("15", "5", "222017"),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Enter a year in the correct format"
          )
        )
      ),
      "Date 15-5-222017 should Invalid number of digits, AC4"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("15", "5", "202"),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Enter a year in the correct format"
          )
        )
      ),
      "Date 15-5-202 should Invalid number of digits, AC4"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("Tuesday", "Jan", "2020"),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Enter a day in the correct format"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("month")) -> Set(
            "Enter a month in the correct format"
          )
        )
      ),
      "Date Tuesday-Jan-2020- should Invalid no more than 2 characters, AC4, AC5"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("01", "01", "999"),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Enter a year in the correct format"
          )
        )
      ),
      "Date 01-01-999- should Invalid no more than 2 characters, AC7"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("01", "01", "20240"),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Enter a year in the correct format"
          )
        )
      ),
      "Date 01-01-20240- should Invalid  year, AC8"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("01", "01", "x"),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Enter a year in the correct format"
          )
        )
      ),
      "Date 01-01-x- should Invalid non numerical year, AC9"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("30", "02", "2001"),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date must be a real date"
          )
        )
      ),
      "Date 30-02-2001- should Invalid non real date, AC10"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("30", "02", "2001"),
      None,
      Some(toSmartString("Date of birth")),
      Some(toSmartString("like 01 04 2001")),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Date of birth must be a real date, like 01 04 2001"
          )
        )
      ),
      "Date 30-02-2001- should Invalid non real date, AC11"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("Tu", "Ja", "2020"),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Enter a day in the correct format"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("month")) -> Set(
            "Enter a month in the correct format"
          )
        )
      ),
      "Date Tu-Ja-2020 Invalid must be numeric, AC4"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("", "", ""),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Enter a date",
            "Enter a day in the correct format"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("month")) -> Set(
            "Enter a date",
            "Enter a month in the correct format"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Enter a date",
            "Enter a year in the correct format"
          )
        )
      ),
      "Date validations should return suitable error if all empty, AC1"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("", "", ""),
      Some(toSmartString("start date")),
      None,
      Some(toSmartString("like 01 04 2023")),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Enter start date, like 01 04 2023",
            "Enter a day in the correct format"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("month")) -> Set(
            "Enter start date, like 01 04 2023",
            "Enter a month in the correct format"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Enter start date, like 01 04 2023",
            "Enter a year in the correct format"
          )
        )
      ),
      "Date validations should return suitable error if all empty, AC2"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("15", "", ""),
      None,
      None,
      None,
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("month")) -> Set(
            "Enter a date",
            "Enter a month in the correct format"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Enter a date",
            "Enter a year in the correct format"
          )
        )
      ),
      "Date validations should return suitable error if some fields are empty, AC1"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("15", "", ""),
      Some(toSmartString("start date")),
      None,
      Some(toSmartString("like 01 04 2023")),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("month")) -> Set(
            "Enter start date, like 01 04 2023",
            "Enter a month in the correct format"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Enter start date, like 01 04 2023",
            "Enter a year in the correct format"
          )
        )
      ),
      "Date validations should return suitable error if some fields are empty,  AC2"
    )
  )

  table2.zipWithIndex
    .traverse[Future, Unit] {
      case (
            (
              dateConstraint,
              (day, month, year),
              errorShortName,
              errorShortNameStart,
              errorExample,
              expected,
              description
            ),
            index
          ) =>
        val dateConstraints = List(dateConstraint)
        val constraints = DateConstraints(dateConstraints)
        val date = Date(constraints, Offset(0), None)

        val fieldValue =
          mkFormComponent("accPeriodStartDate", date).withErrorFields(errorShortName, errorShortNameStart, errorExample)

        val data = variadicFormData[SourceOrigin.OutOfDate](
          "accPeriodStartDate-day"   -> day,
          "accPeriodStartDate-month" -> month,
          "accPeriodStartDate-year"  -> year
        )

        val obtainedF: Future[ValidatedType[Unit]] =
          componentsValidator(mkFormTemplate(mkSection(fieldValue)), fieldValue, data)
            .validate(GetEmailCodeFieldMatcher.noop)
        // .futureValue

        obtainedF.map { obtained =>
          test(s"$index. $description") {
            assertEquals(obtained, expected)
          }
        }
    }
    .futureValue

  val table2nonMandatory: List[(DateConstraint, (String, String, String), ValidatedType[Unit], String)] = List(
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("", "", ""),
      Valid(()),
      "Date validations should be valid if all empty"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("15", "", ""),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("month")) -> Set(
            "Enter a month in the correct format"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Enter a year in the correct format"
          )
        )
      ),
      "Date validations should return suitable error if some fields are empty"
    )
  )

  table2nonMandatory.zipWithIndex
    .traverse[Future, Unit] { case ((dateConstraint, (day, month, year), expected, description), index) =>
      val dateConstraints = List(dateConstraint)
      val constraints = DateConstraints(dateConstraints)
      val date = Date(constraints, Offset(0), None)

      val fieldValue = mkFormComponent("accPeriodStartDate", date).copy(mandatory = false)

      val data = variadicFormData[SourceOrigin.OutOfDate](
        "accPeriodStartDate-day"   -> day,
        "accPeriodStartDate-month" -> month,
        "accPeriodStartDate-year"  -> year
      )

      val obtainedF: Future[ValidatedType[Unit]] =
        componentsValidator(mkFormTemplate(mkSection(fieldValue)), fieldValue, data)
          .validate(GetEmailCodeFieldMatcher.noop)

      obtainedF.map { obtained =>
        test(s"$index. $description") {
          assertEquals(obtained, expected)
        }
      }
    }
    .futureValue

  private def mkDateComponentWithConstraint(id: String, dependentId: String) =
    mkFormComponent(
      id,
      Date(
        DateConstraints(List(DateConstraint(After, DateField(FormComponentId(dependentId)), OffsetDate(0)))),
        Offset(0),
        None
      )
    )

  val table3: List[(String, FormTemplate, FormComponent, ValidatedType[Unit])] = List(
    (
      "validateDate should validate dates in component type group, with dependency on another date in the group", {
        val startDateFc = mkFormComponent("startDate", Date(AnyDate, Offset(0), None))
        val endDateFc = mkDateComponentWithConstraint("endDate", startDateFc.id.value)
        val groupComponent = mkFormComponent("groupFcId", Group(List(startDateFc, endDateFc), Some(1), Some(1)))
        mkFormTemplate(mkSection(groupComponent))
      },
      mkDateComponentWithConstraint("1_endDate", "startDate"),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Indexed(BaseComponentId("endDate"), 1), Atom("day")) -> Set(
            "Date must be after 01 January 2020"
          )
        )
      )
    ),
    (
      "validateDate should validate dates in repeating pages, with dependency on another date in same iteration", {
        val startDateFc = mkFormComponent("startDate", Date(AnyDate, Offset(0), None))
        val endDateFc = mkDateComponentWithConstraint("endDate", startDateFc.id.value)
        mkFormTemplate(mkRepeatingPageSection(List(startDateFc, endDateFc), Constant("1")))
      },
      mkDateComponentWithConstraint("1_endDate", "startDate"),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Indexed(BaseComponentId("endDate"), 1), Atom("day")) -> Set(
            "Date must be after 01 January 2020"
          )
        )
      )
    ),
    (
      "validateDate should validate dates in add-to-list pages, with dependency on another date in same iteration", {
        val startDateFc = mkFormComponent("startDate", Date(AnyDate, Offset(0), None))
        val endDateFc = mkDateComponentWithConstraint("endDate", startDateFc.id.value)
        mkFormTemplate(mkAddToListSection("Add another date?", None, List(startDateFc, endDateFc)))
      },
      mkDateComponentWithConstraint("1_endDate", "startDate"),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Indexed(BaseComponentId("endDate"), 1), Atom("day")) -> Set(
            "Date must be after 01 January 2020"
          )
        )
      )
    )
  )

  table3
    .traverse[Future, Unit] { case (description, formTemplate, formComponentToValidate, expected) =>
      val data = variadicFormData[SourceOrigin.OutOfDate](
        "1_startDate-day"   -> "1",
        "1_startDate-month" -> "1",
        "1_startDate-year"  -> "2020",
        "1_endDate-day"     -> "1",
        "1_endDate-month"   -> "1",
        "1_endDate-year"    -> "2020"
      )
      val validationResultF: Future[ValidatedType[Unit]] =
        componentsValidator(formTemplate, formComponentToValidate, data)
          .validate(GetEmailCodeFieldMatcher.noop)

      validationResultF.map { validationResult =>
        test(description) {
          validationResult shouldBe expected
        }
      }
    }
    .futureValue
}
