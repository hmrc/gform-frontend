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

package uk.gov.hmrc.gform.validation

import cats.data.Validated.{ Invalid, Valid }
import cats.implicits._
import java.time.LocalDate
import munit.FunSuite
import play.api.i18n.Messages
import play.api.test.Helpers
import scala.concurrent.Future
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.{ FormModelSupport, SectionSelectorType, VariadicFormDataSupport }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString, SourceOrigin }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import scala.concurrent.ExecutionContext.Implicits.global

class DateValidationSpec extends FunSuite with FormModelSupport with VariadicFormDataSupport {

  implicit val l: LangADT = LangADT.En

  private val lookupRegistry = new LookupRegistry(Map.empty)

  implicit val smartStringEvaluator: SmartStringEvaluator = (s: SmartString, markDown: Boolean) =>
    s.rawValue(LangADT.En)

  implicit val messages: Messages = Helpers.stubMessages(
    Helpers.stubMessagesApi(
      Map(
        "en" -> Map(
          "date.June"                  -> "June",
          "date.January"               -> "January",
          "date.after"                 -> "{0} must be after {1}",
          "date.day"                   -> "day",
          "date.month"                 -> "month",
          "date.year"                  -> "year",
          "date.firstDay"              -> "the first day",
          "date.lastDay"               -> "the last day",
          "date.ofTheMonth"            -> "of the month",
          "date.precisely"             -> "{0} must be {1}",
          "field.error.exactDigits"    -> "{0} must be a {1} digit number",
          "field.error.notGreaterThan" -> "{0} must not be greater than {1}",
          "field.error.number"         -> "{0} must be a number",
          "field.error.required"       -> "{0} must be entered",
          "generic.error.maxLength"    -> "{0} must be no more than {1} characters",
          "helper.order"               -> "{1} {0}"
        )
      )
    )
  )

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
      booleanExprEval
    )

  private def componentsValidator(
    formTemplate: FormTemplate,
    formComponent: FormComponent,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ) = {

    val fmb = mkFormModelFromSections(formTemplate.sections)

    val fmvo = fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

    val cacheData = new CacheData(
      EnvelopeId(""),
      ThirdPartyData.empty,
      formTemplate
    )

    mkComponentsValidator(fmvo, formComponent, cacheData)
  }

  val table1: List[(DateConstraint, LocalDate, ValidatedType[Unit], String)] = List(
    (
      DateConstraint(After, Today, OffsetDate(1)),
      LocalDate.now().plusDays(2),
      Valid(()),
      "After Today 1 should accepts dates after tomorrow"
    ),
    (
      DateConstraint(After, Today, OffsetDate(0)),
      LocalDate.now().plusDays(1),
      Valid(()),
      "After Today 0 should accepts dates after today"
    ),
    (
      DateConstraint(After, ConcreteDate(Year.Exact(2017), Month.Exact(6), Day.Exact(16)), OffsetDate(5)),
      LocalDate.of(2017, 6, 16).plusDays(6),
      Valid(()),
      "After 2017-06-16 5 should accepts dates after 2017-06-21"
    ),
    (
      DateConstraint(After, ConcreteDate(Year.Exact(2017), Month.Exact(6), Day.Exact(16)), OffsetDate(5)),
      LocalDate.of(2017, 6, 16).plusDays(2),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Label must be after 21 June 2017"
          )
        )
      ),
      "validation for After 2017-06-16 5 should return invalid for dates that aren't after 2017-06-21"
    ),
    (
      DateConstraint(After, Today, OffsetDate(-1)),
      LocalDate.now(),
      Valid(()),
      "After Today -1 should accepts today and dates in future"
    ),
    (
      DateConstraint(After, ConcreteDate(Year.Exact(2017), Month.Exact(6), Day.Exact(16)), OffsetDate(-5)),
      LocalDate.of(2017, 6, 16).plusDays(-4),
      Valid(()),
      "After 2017-06-16 -5 should accepts dates after 2017-06-11"
    ),
    (
      DateConstraint(Before, Today, OffsetDate(1)),
      LocalDate.now(),
      Valid(()),
      "Before Today 1 should accepts today and dates in past"
    ),
    (
      DateConstraint(Before, Today, OffsetDate(0)),
      LocalDate.now().plusDays(-1),
      Valid(()),
      "Before Today 0 should accepts dates before today"
    ),
    (
      DateConstraint(Before, Today, OffsetDate(-1)),
      LocalDate.now().plusDays(-2),
      Valid(()),
      "Before Today -1 should accepts dates before yesterday"
    ),
    (
      DateConstraint(Before, ConcreteDate(Year.Exact(2017), Month.Exact(6), Day.Exact(16)), OffsetDate(-5)),
      LocalDate.of(2017, 6, 16).plusDays(-6),
      Valid(()),
      "Before 2017-06-16 -5 should accepts dates before 2017-06-11"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Exact(4), Day.Any), OffsetDate(0)),
      LocalDate.of(2017, 4, 16),
      Valid(()),
      "Precisely YYYY-04-DD should accept any date that in April"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Last), OffsetDate(0)),
      LocalDate.of(2020, 2, 29),
      Valid(()),
      "Precisely YYYY-MM-lastDay should accept any date that is the last day of the given month"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Next, Month.Any, Day.Any), OffsetDate(0)),
      LocalDate.of(LocalDate.now().getYear + 1, 2, 28),
      Valid(()),
      "Precisely next-MM-DD should accept any date that is next year"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Last), OffsetDate(0)),
      LocalDate.of(2020, 2, 26),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Label must be the last day of the month "
          )
        )
      ),
      "Date 26-02-2020 should return Is not Valid when lastDay validation is applied"
    ),
    (
      DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.First), OffsetDate(0)),
      LocalDate.of(2020, 2, 26),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Label must be the first day of the month "
          )
        )
      ),
      "Date 26-02-2020 should return Is not Valid when firstDay validation is applied"
    )
  )

  table1.zipWithIndex.traverse[Future, Unit] { case ((dateConstraint, acceptedAfter, expected, description), index) =>
    val dateConstraints = List(dateConstraint)
    val constraints = DateConstraints(dateConstraints)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = mkFormComponent("accPeriodStartDate", date)

    val data = variadicFormData[SourceOrigin.OutOfDate](
      "accPeriodStartDate-day"   -> acceptedAfter.getDayOfMonth.toString,
      "accPeriodStartDate-month" -> acceptedAfter.getMonthValue.toString,
      "accPeriodStartDate-year"  -> acceptedAfter.getYear.toString
    )

    val obtainedF: Future[ValidatedType[Unit]] =
      componentsValidator(mkFormTemplate(mkSection(fieldValue)), fieldValue, data)
        .validate(GetEmailCodeFieldMatcher.noop)

    obtainedF.map { obtained =>
      test(index + ". " + description) {
        assertEquals(obtained, expected)
      }
    }
  }

  val table2: List[(DateConstraint, (String, String, String), ValidatedType[Unit], String)] = List(
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("35", "12", "2017"),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            " Label day must not be greater than 31"
          )
        )
      ),
      "Date 35-12-2017 should return Is not Valid"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("15", "5", "222017"),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            " Label year must be no more than 4 characters"
          )
        )
      ),
      "Date 15-5-222017 should Invalid number of digits"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("15", "5", "202"),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            " Label year must be a 4 digit number"
          )
        )
      ),
      "Date 15-5-202 should Invalid number of digits"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("Tuesday", "Jan", "2020"),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            " Label day must be no more than 2 characters"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("month")) -> Set(
            " Label month must be no more than 2 characters"
          )
        )
      ),
      "Date Tuesday-Jan-202- should Invalid no more than 2 characters"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("Tu", "Ja", "2020"),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            " Label day must be a number"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("month")) -> Set(
            " Label month must be a number"
          )
        )
      ),
      "Date Tu-Ja-2020 Invalid must be numeric"
    ),
    (
      DateConstraint(After, Today, OffsetDate(1)),
      ("", "", ""),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("day")) -> Set(
            "Label must be entered",
            " Label must be entered"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("month")) -> Set(
            "Label must be entered",
            " Label must be entered"
          ),
          ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId("accPeriodStartDate")), Atom("year")) -> Set(
            "Label must be entered",
            " Label must be entered"
          )
        )
      ),
      "Date validations should return suitable error if empty"
    )
  )

  table2.zipWithIndex.traverse[Future, Unit] {
    case ((dateConstraint, (day, month, year), expected, description), index) =>
      val dateConstraints = List(dateConstraint)
      val constraints = DateConstraints(dateConstraints)
      val date = Date(constraints, Offset(0), None)

      val fieldValue = mkFormComponent("accPeriodStartDate", date)

      val data = variadicFormData[SourceOrigin.OutOfDate](
        "accPeriodStartDate-day"   -> day,
        "accPeriodStartDate-month" -> month,
        "accPeriodStartDate-year"  -> year
      )

      val obtainedF: Future[ValidatedType[Unit]] =
        componentsValidator(mkFormTemplate(mkSection(fieldValue)), fieldValue, data)
          .validate(GetEmailCodeFieldMatcher.noop)

      obtainedF.map { obtained =>
        test(index + ". " + description) {
          assertEquals(obtained, expected)
        }
      }
  }

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
            "Label must be after 01 January 2020"
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
            "Label must be after 01 January 2020"
          )
        )
      )
    ),
    (
      "validateDate should validate dates in add-to-list pages, with dependency on another date in same iteration", {
        val startDateFc = mkFormComponent("startDate", Date(AnyDate, Offset(0), None))
        val endDateFc = mkDateComponentWithConstraint("endDate", startDateFc.id.value)
        mkFormTemplate(mkAddToListSection("Add another date?", List(startDateFc, endDateFc)))
      },
      mkDateComponentWithConstraint("1_endDate", "startDate"),
      Invalid(
        Map(
          ModelComponentId.Atomic(IndexedComponentId.Indexed(BaseComponentId("endDate"), 1), Atom("day")) -> Set(
            "Label must be after 01 January 2020"
          )
        )
      )
    )
  )

  table3.traverse[Future, Unit] { case (description, formTemplate, formComponentToValidate, expected) =>
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
}
