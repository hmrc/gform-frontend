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

package uk.gov.hmrc.gform.eval.smartstring

import play.api.i18n.Messages
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.gform.commons.MarkDownUtil.escapeMarkdown
import uk.gov.hmrc.gform.eval.{ ExprType, TypeInfo }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }
import uk.gov.hmrc.gform.views.summary.TextFormatter

import java.text.MessageFormat

trait SmartStringEvaluatorFactory {
  def apply(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo]
  )(implicit messages: Messages, l: LangADT): SmartStringEvaluator
}

class RealSmartStringEvaluatorFactory() extends SmartStringEvaluatorFactory {

  def apply(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo]
  )(implicit
    messages: Messages,
    l: LangADT
  ): SmartStringEvaluator =
    new SmartStringEvaluator {

      override def apply(s: SmartString, markDown: Boolean): String = {
        import scala.collection.JavaConverters._
        new MessageFormat(s.rawValue(l))
          .format(
            s.interpolations
              .map { interpolation =>
                formatExpr(interpolation, markDown)
              }
              .asJava
              .toArray
          )

      }

      private def formatExpr(expr: Expr, markDown: Boolean): String = {

        val typeInfo: TypeInfo = formModelVisibilityOptics.formModel.toFirstOperandTypeInfo(expr)

        val interpolated = typeInfo.staticTypeData.exprType match {
          case ExprType.ChoiceSelection =>
            typeInfo.expr match {
              case FormCtx(formComponentId) if typeInfo.staticTypeData.exprType == ExprType.ChoiceSelection =>
                formModelVisibilityOptics.formModel.fcLookup
                  .get(formComponentId)
                  .map {
                    case IsChoice(choice) =>
                      val optionsList = choice.options.zipWithIndex
                        .map {
                          case (OptionData.IndexBased(label, _, _, _), i)        => i.toString -> label
                          case (OptionData.ValueBased(label, _, _, _, value), _) => value      -> label
                        }
                        .toList
                        .toMap
                      mapChoiceSelectedIndexes(
                        typeInfo,
                        _.map(i => apply(optionsList(i), markDown)).mkString(",")
                      )
                    case IsRevealingChoice(revealingChoice) =>
                      revealingChoice.options.map(c => apply(c.choice.label, markDown)).mkString(",")
                    case _ =>
                      stringRepresentation(typeInfo)
                  }
                  .getOrElse("")
              case _ => stringRepresentation(typeInfo)
            }
          case _ =>
            expr match {
              case NumberedList(fcId) =>
                val fcIdTypeInfo = formModelVisibilityOptics.formModel.toFirstOperandTypeInfo(FormCtx(fcId))
                govukListRepresentation(fcIdTypeInfo, markDown = markDown, isBulleted = false)
              case BulletedList(fcId) =>
                val fcIdTypeInfo = formModelVisibilityOptics.formModel.toFirstOperandTypeInfo(FormCtx(fcId))
                govukListRepresentation(fcIdTypeInfo, markDown = markDown, isBulleted = true)
              case _ => stringRepresentation(typeInfo)
            }
        }

        val formatted = typeInfo.staticTypeData.textConstraint.fold(interpolated) { textConstraint =>
          TextFormatter.componentTextReadonly(interpolated, textConstraint)
        }

        expr match {
          case _: NumberedList | _: BulletedList => formatted
          case _ =>
            if (markDown) {
              typeInfo.staticTypeData.exprType match {
                case ExprType.AddressString =>
                  addressRepresentation(typeInfo).map(HtmlFormat.escape).map(_.body).mkString("<br>")
                case _ => escapeMarkdown(formatted)
              }
            } else {
              formatted
            }
        }
      }

      private def stringRepresentation(typeInfo: TypeInfo): String =
        formModelVisibilityOptics.evalAndApplyTypeInfo(typeInfo).stringRepresentation(messages)

      private def addressRepresentation(typeInfo: TypeInfo): List[String] =
        formModelVisibilityOptics.evalAndApplyTypeInfo(typeInfo).addressRepresentation

      private def mapChoiceSelectedIndexes(typeInfo: TypeInfo, f: Seq[String] => String): String =
        formModelVisibilityOptics
          .evalAndApplyTypeInfo(typeInfo)
          .optionRepresentation
          .fold(stringRepresentation(typeInfo))(f(_))

      private def govukListRepresentation(typeInfo: TypeInfo, isBulleted: Boolean, markDown: Boolean): String = {
        val defaultLines = formModelVisibilityOptics
          .evalAndApplyTypeInfo(typeInfo)
          .listRepresentation(messages)
        val lines = typeInfo.staticTypeData.textConstraint
          .map(c => defaultLines.map(v => TextFormatter.componentTextReadonly(v, c)))
          .getOrElse(defaultLines)
        if (markDown)
          govukList(lines.map(HtmlFormat.escape).map(_.body), isBulleted)
        else
          govukList(lines, isBulleted)

      }
      private def govukList(
        list: List[String],
        isBulleted: Boolean
      ): String = {
        val first =
          if (isBulleted) """<ul class="govuk-list govuk-list--bullet">"""
          else """<ol class="govuk-list govuk-list--number">"""
        val last = if (isBulleted) "</ul>" else "</ol>"
        val elements = list.foldLeft("")((a, e) => a + "<li>" + e + "</li>")
        first + elements + last
      }
    }

}
