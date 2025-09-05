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

trait ErrorValueProvider {
  def errorValues(fc: FormComponent): List[VariadicFormData[SourceOrigin.OutOfDate]]
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
    ("29", "2", "2001"),
    ("1", "13", "2001"),
    ("1", "1", "1940"), // cater for after a hard coded date within the form
    ("1", "1", "2060") // cater for before a hard coded date from form
  )

  override def errorValues(fc: FormComponent): List[VariadicFormData[SourceOrigin.OutOfDate]] = {
    val otherFieldValues = fc match {
      case IsDate(Date(DateConstraints(constraints), _, _)) =>
        constraints
          .collect {
            case DateConstraint(Before, DateField(other), _) =>
              Map(
                FormComponentId(s"${other.baseComponentId.value}-day").modelComponentId   -> VariadicValue.One("1"),
                FormComponentId(s"${other.baseComponentId.value}-month").modelComponentId -> VariadicValue.One("1"),
                FormComponentId(s"${other.baseComponentId.value}-year").modelComponentId  -> VariadicValue.One("1960")
              )
            case DateConstraint(After, DateField(other), _) =>
              Map(
                FormComponentId(s"${other.baseComponentId.value}-day").modelComponentId   -> VariadicValue.One("1"),
                FormComponentId(s"${other.baseComponentId.value}-month").modelComponentId -> VariadicValue.One("1"),
                FormComponentId(s"${other.baseComponentId.value}-year").modelComponentId  -> VariadicValue.One("2050")
              )
          }
          .foldLeft(Map.empty[ModelComponentId, VariadicValue]) { case (a, m) => a ++ m }
      case _ => Map.empty[ModelComponentId, VariadicValue]
    }

    rawValues.map { case (day, month, year) =>
      VariadicFormData[OutOfDate](
        Map(
          FormComponentId(s"${fc.baseComponentId.value}-day").modelComponentId   -> VariadicValue.One(day),
          FormComponentId(s"${fc.baseComponentId.value}-month").modelComponentId -> VariadicValue.One(month),
          FormComponentId(s"${fc.baseComponentId.value}-year").modelComponentId  -> VariadicValue.One(year)
        ) ++ otherFieldValues
      )
    }
  }
}
