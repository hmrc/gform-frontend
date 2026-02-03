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

package uk.gov.hmrc.gform.testonly

import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData, VariadicValue }

import java.time.LocalDate
import scala.util.Try

/** Implement to provide a list of values for a given component type
  * that will cause different failures and hence generate all error
  * messages.
  */
trait ErrorValueProvider {
  def errorValues(
    fc: FormComponent,
    existingData: VariadicFormData[SourceOrigin.Current]
  ): List[VariadicFormData[SourceOrigin.OutOfDate]]
}

class TextWithTimeConstraintErrorProvider extends ErrorValueProvider {
  private val rawValues = List("12:00", "12:20")

  override def errorValues(
    fc: FormComponent,
    existingData: VariadicFormData[SourceOrigin.Current]
  ): List[VariadicFormData[OutOfDate]] =
    rawValues.map { time =>
      VariadicFormData[OutOfDate](
        Map(fc.id.modelComponentId -> VariadicValue.One(time))
      )
    }
}

class DateErrorProvider extends ErrorValueProvider {
  private val rawValues = List(
    ("", "", ""),
    ("1", "", ""),
    ("", "1", ""),
    ("", "", "2000"),
    ("1", "1", ""),
    ("1", "", "2000"),
    ("", "1", "2000"),
    ("15", "1", "2000"), // Need valid date not first day of month to trigger that constraint failure
    ("29", "2", "2001"),
    ("1", "1", "1940"), // test to fail for after a hard coded date within the form
    ("1", "1", "2060") // test to fail for before a hard coded date within the form
  )

  override def errorValues(
    fc: FormComponent,
    existingData: VariadicFormData[SourceOrigin.Current]
  ): List[VariadicFormData[SourceOrigin.OutOfDate]] = {
    def atoms(fcId: FormComponentId): (ModelComponentId, ModelComponentId, ModelComponentId) =
      (
        FormComponentId(s"${fcId.baseComponentId.value}-day").modelComponentId,
        FormComponentId(s"${fcId.baseComponentId.value}-month").modelComponentId,
        FormComponentId(s"${fcId.baseComponentId.value}-year").modelComponentId
      )

    // Valid if all atoms exist and make a real date
    def isValidDate(fcId: FormComponentId): Boolean = {
      val (day, month, year) = atoms(fcId)
      (existingData.get(day), existingData.get(month), existingData.get(year)) match {
        case (Some(dv), Some(mv), Some(yv)) =>
          Try {
            LocalDate.of(yv.toSeq.head.toInt, mv.toSeq.head.toInt, dv.toSeq.head.toInt)
          }.fold(_ => false, _ => true)
        case _ => false
      }
    }

    def mkValidOrDefaults(fcId: FormComponentId, dStr: String, mStr: String, yStr: String) = {
      val (day, month, year) = atoms(fcId)
      val isValid = isValidDate(fcId)

      val dayDefault = VariadicValue.One(dStr)
      val monthDefault = VariadicValue.One(mStr)
      val yearDefault = VariadicValue.One(yStr)

      if (isValid)
        Map(
          day   -> existingData.get(day).getOrElse(dayDefault),
          month -> existingData.get(month).getOrElse(monthDefault),
          year  -> existingData.get(year).getOrElse(yearDefault)
        )
      else
        Map(
          day   -> dayDefault,
          month -> monthDefault,
          year  -> yearDefault
        )
    }

    val otherFieldValues = fc match {
      case IsDate(Date(DateConstraints(constraints), _, _)) =>
        constraints
          .collect {
            case DateConstraint(Before, DateField(other), _) => mkValidOrDefaults(other, "1", "1", "1950")
            case DateConstraint(After, DateField(other), _)  => mkValidOrDefaults(other, "1", "1", "2050")
          }
          .foldLeft(Map.empty[ModelComponentId, VariadicValue]) { case (a, m) => a ++ m }
      case _ => Map.empty[ModelComponentId, VariadicValue]
    }

    rawValues.map { case (dayValue, monthValue, yearValue) =>
      val (day, month, year) = atoms(fc.id)
      VariadicFormData[OutOfDate](
        Map(
          day   -> VariadicValue.One(dayValue),
          month -> VariadicValue.One(monthValue),
          year  -> VariadicValue.One(yearValue)
        ) ++ otherFieldValues
      )
    }
  }
}
