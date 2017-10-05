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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ComponentType, Group, Number, PositiveNumber, Sterling, Text, UkBankAccountNumber }
import uk.gov.hmrc.gform.validation.FormFieldValidationResult

object TextFormatter {

  def formatText(component: ComponentType, validationResult: Option[FormFieldValidationResult]): String = {
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

            if (sections(0).size >= 5) {
              val integerPart = sections(0).reverse.grouped(3).mkString(",").reverse
              if (sections.size == 2) {
                integerPart + "." + sections(1)
              } else {
                integerPart
              }
            } else valueWithoutPoundsOrCommas

          case UkBankAccountNumber => currentValue.grouped(4).mkString(" ")

          case _ => currentValue
        }
      }
    }
    component match {
      case x: Text => componentText(x)
      case g: Group => g.fields.find(_.id.value == validationResult.map(x => x.fieldValue.id.value).getOrElse("")).map(x => formatText(x.`type`, validationResult)).mkString
      case _ => currentValue
    }
  }
}
