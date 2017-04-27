/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.service

import java.time.LocalDate

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Monoid
import cats.syntax.cartesian._
import cats.syntax.validated._
import cats.instances.all._
import org.slf4j.LoggerFactory
import uk.gov.hmrc.bforms.models.ValidationUtil._
import uk.gov.hmrc.bforms.models._
import uk.gov.hmrc.bforms.models.components._
import uk.gov.hmrc.bforms.typeclasses.Now


import scala.util.{Failure, Success, Try}

/**
  * Created by dimitra on 20/04/17.
  */
object ValidationService {
  lazy val log = LoggerFactory.getLogger(ValidationService.getClass)

  case class CompData(fieldValue: FieldValue, data: Map[FieldId, Seq[String]]) {

    def validateComponents: ValidatedType = {
      fieldValue.`type` match {
        case date@Date(_, _, _) =>
          val reqFieldValidResult = validateDateRequiredField(fieldValue)(data)
          val otherRulesValidResult = validateDate(fieldValue, date)(data)

          Monoid[ValidatedType].combineAll(List(reqFieldValidResult, otherRulesValidResult))

        case Text(_) => validateText(fieldValue)(data)
        case Address => validateAddress(fieldValue)(data)
        case Choice(_, _, _, _) => validateChoice(fieldValue)(data)
      }
    }

    // text
    def validateText(fieldValue: FieldValue)(data: Map[FieldId, Seq[String]]): ValidatedType = {
      val textData = TextData(data.get(fieldValue.id).toList.flatten)

      fieldValue.mandatory match {
        case true => validateRequired(fieldValue.id)(textData.value)
        case false => Valid()
      }
    }

    def validateRequired(fieldId: FieldId)(xs: Seq[String]): ValidatedType = {
      xs.filterNot(_.isEmpty()) match {
        case Nil => Invalid(Map(fieldId -> Set("RequiredFieldException")))
        case value :: Nil => Valid(())
        case value :: rest => Valid(()) // we don't support multiple values yet
      }
    }

    // choice
    def validateChoice(fieldValue: FieldValue)(data: Map[FieldId, Seq[String]]): ValidatedType = {
      val choiceValue = ChoiceComponentData(data.get(fieldValue.id).toList.flatten)

      (fieldValue.mandatory, choiceValue.selected) match {
        case (true, Nil) => Invalid(Map(fieldValue.id -> Set("RequiredFieldException")))
        case _ => Valid(())
      }
    }

    // address
    val dataGetter: FieldValue => String => Seq[String] = fv => suffix => data.get(fv.id.withSuffix(suffix)).toList.flatten

    def validateRF(value: String) = validateRequired(fieldValue.id.withSuffix(value)) _

    def validateAddress(fieldValue: FieldValue)(data: Map[FieldId, Seq[String]]): ValidatedType = {
      val addressValueOf = dataGetter(fieldValue)

      val validatedResult: List[ValidatedType] = List(validateRF("street1")(addressValueOf("street1")),
        validateRF("town")(addressValueOf("town")),
        validateRF("postcode")(addressValueOf("postcode")))

      Monoid[ValidatedType].combineAll(validatedResult)
    }

    def validateDateRequiredField(fieldValue: FieldValue)(data: Map[FieldId, Seq[String]]): ValidatedType={
      val dateValueOf = dataGetter(fieldValue)

      val validatedResult: List[ValidatedType] = List(validateRF("day")(dateValueOf("day")),
        validateRF("month")(dateValueOf("month")),
        validateRF("year")(dateValueOf("year")))

      Monoid[ValidatedType].combineAll(validatedResult)
    }

    def validateDate(fieldValue: FieldValue, date: Date)(data: Map[FieldId, Seq[String]]): ValidatedType = {
      date.constraintType match {
        //      case AnyDate =>
        case DateConstraints(dateConstraintList) =>

          val result = dateConstraintList.map {
            case DateConstraint(beforeOrAfter, dateConstrInfo, offsetDate) =>

              (beforeOrAfter, dateConstrInfo, offsetDate) match {
                case (Before, Today, offset) =>
                  validateInputDate(fieldValue, data)
                    .andThen(inputDate =>
                      validateToday(fieldValue, inputDate, offset, Map(fieldValue.id -> Set("ConcreteDateException")))(isBeforeToday))

                case (Before, concreteDate: ConcreteDate, offset) =>
                  validateConcreteDate(concreteDate, Map(fieldValue.id -> Set("ConcreteDateException")))
                    .andThen(concreteDate =>
                      validateInputDate(fieldValue, data)
                        .andThen(inputDate =>
                          validateConcreteDate(fieldValue, inputDate, concreteDate, offset, Map(fieldValue.id -> Set("ConcreteDateException")))(isBeforeConcreteDate)))

                //              case (Before, AnyWord(value)) =>
                // case (Before, AnyWord(FieldId)) =>

                case (After, Today, offset) =>
                  validateInputDate(fieldValue, data)
                    .andThen(inputDate =>
                      validateToday(fieldValue, inputDate, offset, Map(fieldValue.id -> Set("ConcreteDateException")))(isAfterToday))

                case (After, concreteDate: ConcreteDate, offset) =>

                  validateConcreteDate(concreteDate, Map(fieldValue.id -> Set("ConcreteDateException")))
                    .andThen(concreteDate =>
                      validateInputDate(fieldValue, data)
                        .andThen(inputDate =>
                          validateConcreteDate(fieldValue, inputDate, concreteDate, offset, Map(fieldValue.id -> Set("ConcreteDateException")))(isAfterConcreteDate)))

                //              case (After, AnyWord(value)) =>

              }


          }

          Monoid[ValidatedType].combineAll(result)
      }

    }


    def validateToday(fieldValue: FieldValue, localDate: LocalDate,
                      offset: OffsetDate, dateError: GFormError)
                     (func: (LocalDate, OffsetDate) => Boolean): ValidatedType = {
      func(localDate, offset) match {
        case true => Valid(())
        case false => Invalid(dateError)
      }
    }

    def validateConcreteDate(fieldValue: FieldValue, localDate: LocalDate,
                             concreteDate: LocalDate, offset: OffsetDate, dateError: GFormError)
                            (func: (LocalDate, LocalDate, OffsetDate) => Boolean): ValidatedType = {
      func(localDate, concreteDate, offset) match {
        case true => Valid(())
        case false => Invalid(dateError)
      }
    }

    def isAfterToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean = date.isAfter(now.apply().plusDays(offset.value))

    def isAfterConcreteDate(date: LocalDate, concreteDay: LocalDate, offset: OffsetDate): Boolean = date.isAfter(concreteDay.plusDays(offset.value))

    def isBeforeToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean = date.isBefore(now.apply().plusDays(offset.value))

    def isBeforeConcreteDate(date: LocalDate, concreteDay: LocalDate, offset: OffsetDate): Boolean = date.isBefore(concreteDay.plusDays(offset.value))

    def validateConcreteDate(concreteDate: ConcreteDate, dateError: GFormError): ValidatedLocalDate = {
      Try(LocalDate.of(concreteDate.year, concreteDate.month, concreteDate.day)) match {
        case Success(date) => Valid(date)
        case Failure(ex) => Invalid(dateError)
      }
    }

    def validateInputDate(fieldValue: FieldValue, data: Map[FieldId, Seq[String]]): ValidatedLocalDate = {
      val fieldIdList = Date.fields(fieldValue.id).map(fId => data.get(fId))
      fieldIdList match {
        case Some(year :: Nil) :: Some(month :: Nil) :: Some(day :: Nil) :: Nil =>

          validateLocalDate(fieldValue, day, month, year) match {
            case Valid(concreteDate) => validateConcreteDate(concreteDate, Map(fieldValue.id -> Set("ConcreteDateException")))
            case Invalid(nonEmptyList) => Invalid(nonEmptyList)
          }

        case _ => Invalid(Map(fieldValue.id -> Set("DateMissingException")))
      }
    }

    def validateLocalDate(fv: FieldValue, day: String, month: String, year: String): ValidatedConcreteDate = {
      val d = isNonNumeric(fv, day).andThen(x => hasValidNumberOfDigits(x, 2, Map(fv.id -> Set("Non2DigitsException")))).andThen(y => isValidNumber(y, 31, Map(fv.id -> Set("DateOuOfBoundsException"))))
      val m = isNonNumeric(fv, month).andThen(x => hasValidNumberOfDigits(x, 2, Map(fv.id -> Set("Non2DigitsException")))).andThen(y => isValidNumber(y, 12, Map(fv.id -> Set("MonthOutOfBoundsException"))))
      val y = isNonNumeric(fv, year).andThen(x => hasValidNumberOfDigits(x, 4, Map(fv.id -> Set("Non4DigitsException"))))

      val d2 = isNonNumeric(fv, day).andThen(y => isValidNumber(y, 31))

      // implicit val semigroup: Semigroup[NonEmptyList[DateError]] = SemigroupK[NonEmptyList].algebra[DateError]

      parallelWithApplicative(d, m, y)(ConcreteDate.apply)
    }

    def isNonNumeric(fieldValue: FieldValue, str: String): ValidatedNumeric = {
      Try(str.toInt) match {
        case Success(x) => Valid(x)
        case Failure(_) => Invalid(Map(fieldValue.id -> Set("NonNumericException")))
      }
    }

    def isValidNumber(number: Int, dayOrMonth: Int, dateError: GFormError): ValidatedNumeric = {
      if (number <= dayOrMonth) {
        Valid(number)
      } else {
        Invalid(dateError)
      }
    }

    def hasValidNumberOfDigits(number: Int, digits: Int, dateError: GFormError): ValidatedNumeric = {
      (Math.log10(number) + 1) <= digits match {
        case true => Valid(number)
        case false => Invalid(dateError)
      }
    }

    /* def parallelValidate[E: Semigroup](v1: Validated[E, Int], v2: Validated[E, Int], v3: Validated[E, Int])(f: (Int, Int, Int) => ConcreteDate): Validated[E, ConcreteDate] = {
       (v1, v2, v3) match {
         case (Valid(day), Valid(month), Valid(year)) => Valid(f(year, month, day))
         case (Valid(_), Valid(_), i@Invalid(_)) => i
         case (Valid(_), i@Invalid(_), Valid(_)) => i
         case (Valid(_), Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))

         case (i@Invalid(_), Valid(_), Valid(_)) => i
         case (Invalid(e1), Valid(_), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
         case (Invalid(e1), Invalid(e2), Valid(_)) => Invalid(Semigroup[E].combine(e1, e2))
         case (Invalid(e1), Invalid(e2), Invalid(e3)) => Invalid(e1 |+| e2 |+| e3)
          /* val comb1 = Semigroup[E].combine(e1, e2)
           val comb2 = Semigroup[E].combine(comb1, e3)
           Invalid(comb2)*/
       }
     }*/

    def parallelWithApplicative[E: Semigroup](v1: Validated[E, Int], v2: Validated[E, Int], v3: Validated[E, Int])
                                             (f: (Int, Int, Int) => ConcreteDate):
    Validated[E, ConcreteDate] = {
      (v1 |@| v2 |@| v3).map(f)
    }

    def alwaysOk(fieldValue: FieldValue)(xs: Seq[String]): FormFieldValidationResult = {
      xs match {
        case Nil => FieldOk(fieldValue, "")
        case value :: rest => FieldOk(fieldValue, value) // we don't support multiple values yet
      }
    }

  }

}
