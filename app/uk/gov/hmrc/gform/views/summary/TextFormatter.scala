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

package uk.gov.hmrc.gform.views.summary

import java.awt.Component
import java.io

import play.api.Logger
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FormFieldValidationResult

object TextFormatter {

  def formatText(validationResult: Option[FormFieldValidationResult]): String = {
    val currentValue = validationResult match {
      case Some(result) => result.getCurrentValue.getOrElse("")
      case None => ""
    }

    def componentText(text: Text) = {
      if (currentValue.isEmpty) {
        currentValue
      } else {
        text.constraint match {
          case PositiveNumber(_, _, Some(unit)) => currentValue + " " + unit
          case Number(_, _, Some(unit)) => currentValue + " " + unit
          case PositiveNumber(_, _, None) | Number(_, _, None) | Sterling =>

            val poundOrComma = "[£,]".r
            val valueWithoutPoundsOrCommas = poundOrComma.replaceAllIn(currentValue, "")
            val sections = valueWithoutPoundsOrCommas.split("""\.""")

            val number = if (sections(0).size >= 5) {
              val integerPart = sections(0).reverse.grouped(3).mkString(",").reverse
              if (sections.size == 2) {
                integerPart + "." + sections(1)
              } else {
                integerPart
              }
            } else valueWithoutPoundsOrCommas

            if (text.constraint == Sterling)
              "£" + number
            else
              number
          case _ =>
            currentValue
        }
      }
    }
    def getValue(componentType: ComponentType): String = componentType match {
      case x: Text => componentText(x)
      case _ => currentValue
    }

    validationResult
      .map(result => getValue(result.fieldValue.`type`))
      .getOrElse("")
  }

  def appendUnit(text: Text): String = text.constraint match {
    case PositiveNumber(_, _, Some(unit)) => unit
    case Number(_, _, Some(unit)) => unit
    case _ => ""
  }

  def isSterling(formComponent: FormComponent) = formComponent.`type` match {
    case Text(Sterling, _) => true
    case _ => false
  }

  def isNumber(formComponent: FormComponent) = formComponent.`type` match {
    case Text(Number(_, _, _), _) | Text(PositiveNumber(_, _, _), _) => true
    case _ => false
  }
}
