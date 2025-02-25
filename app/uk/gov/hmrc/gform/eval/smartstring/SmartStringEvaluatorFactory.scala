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
import uk.gov.hmrc.gform.commons.MarkDownUtil.unescapeMarkdownHtml
import uk.gov.hmrc.gform.eval.{ ExprType, StaticTypeData, TypeInfo }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.gform.gform.{ ConcatFormatSubstituter, ConcatFormatSubstitutions, Substituter }
import ConcatFormatSubstituter._
import uk.gov.hmrc.gform.eval.ExprType.ChoiceSelection

import scala.jdk.CollectionConverters._
import java.text.MessageFormat

trait SmartStringEvaluatorFactory {
  def apply(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo]
  )(implicit messages: Messages, l: LangADT): SmartStringEvaluator
}

class RealSmartStringEvaluatorFactory(englishMessages: Messages) extends SmartStringEvaluatorFactory {

  def apply(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo]
  )(implicit
    messages: Messages,
    l: LangADT
  ): SmartStringEvaluator =
    new SmartStringEvaluator {

      val requestLangExecutor = new Executor(formModelVisibilityOptics, messages, l)
      val englishOnlyExecutor = new Executor(formModelVisibilityOptics, englishMessages, LangADT.En)

      override def apply(s: SmartString, markDown: Boolean): String = requestLangExecutor(s, markDown)
      override def evalEnglish(s: SmartString, markDown: Boolean): String = englishOnlyExecutor(s, markDown)
    }

  def noForm(evalExpr: Expr => String)(implicit l: LangADT): SmartStringEvaluator =
    new SmartStringEvaluator {
      override def apply(s: SmartString, markDown: Boolean): String =
        evalSmartString(s.rawDefaultValue(l), s.interpolations(_ => false))

      override def evalEnglish(s: SmartString, markDown: Boolean): String =
        evalSmartString(
          s.rawDefaultValue(LangADT.En),
          s.interpolations(_ => false)
        )

      private def evalSmartString(rawDefaultValue: String, interpolations: List[Expr]) =
        new MessageFormat(rawDefaultValue)
          .format(
            interpolations
              .map(evalExpr)
              .asJava
              .toArray
          )
    }
}

private class Executor(
  formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
  messages: Messages,
  l: LangADT
) {
  def apply(s: SmartString, markDown: Boolean): String = {
    val substitutions = ConcatFormatSubstitutions(concat => formatConcatExpr(concat, markDown))
    new MessageFormat(s.rawValue(formModelVisibilityOptics.booleanExprResolver.resolve(_))(l))
      .format(
        s.interpolations(formModelVisibilityOptics.booleanExprResolver.resolve(_))
          .map(expr => implicitly[Substituter[ConcatFormatSubstitutions, Expr]].substitute(substitutions, expr))
          .map(formatExpr(_, markDown))
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
            evalChoiceAsString(formComponentId, typeInfo, markDown)
          case _ => stringRepresentation(typeInfo)
        }
      case _ =>
        expr match {
          case NumberedList(fcId) =>
            val fcIdTypeInfo = formModelVisibilityOptics.formModel.toFirstOperandTypeInfo(FormCtx(fcId))
            govukListRepresentation(fcIdTypeInfo, markDown = markDown, isBulleted = false, fcId = fcId)
          case BulletedList(fcId) =>
            val fcIdTypeInfo = formModelVisibilityOptics.formModel.toFirstOperandTypeInfo(FormCtx(fcId))
            govukListRepresentation(fcIdTypeInfo, markDown = markDown, isBulleted = true, fcId = fcId)
          case ChoicesRevealedField(fcId) =>
            formModelVisibilityOptics.formModel.fcLookup
              .get(fcId)
              .collect { case IsRevealingChoice(revealingChoice) =>
                revealingChoice.options
                  .map { c =>
                    val label = apply(c.choice.label, markDown)
                    val revealingFieldsValue = c.revealingFields
                      .map(rf =>
                        formModelVisibilityOptics.recData.variadicFormData
                          .one(rf.id.modelComponentId)
                          .getOrElse("")
                      )
                      .mkString("")
                    if (revealingFieldsValue.nonEmpty) s"$label - $revealingFieldsValue" else label
                  }
                  .mkString(", ")
              }
              .getOrElse("")
          case _ => stringRepresentation(typeInfo)
        }
    }

    val formatted = typeInfo.staticTypeData.textConstraint.fold(interpolated) { textConstraint =>
      val intermediateValue: String = TextFormatter.componentTextReadonly(interpolated, textConstraint)(l)
      expr match {
        case HideZeroDecimals(_) => TextFormatter.hideZeroDecimals(textConstraint, intermediateValue)
        case _                   => intermediateValue
      }
    }

    expr match {
      case _: NumberedList | _: BulletedList => formatted
      case _ =>
        if (markDown) {
          typeInfo.staticTypeData.exprType match {
            case ExprType.AddressString =>
              addressRepresentation(typeInfo).map(HtmlFormat.escape).map(_.body).mkString("<br>")
            case _ => unescapeMarkdownHtml(formatted)
          }
        } else {
          formatted
        }
    }
  }

  private def formatConcatExpr(concatExpr: Concat, markDown: Boolean): String = {
    val exprsFormatted = concatExpr.exprs.map {
      case c @ Constant(_) => c
      case expr            => Constant(formatExpr(expr, markDown))
    }
    val concatUpdated = Concat(exprsFormatted)
    formatExpr(concatUpdated, markDown)
  }

  private def evalChoice(fcId: FormComponentId, typeInfo: TypeInfo, markDown: Boolean): List[String] =
    formModelVisibilityOptics.formModel.fcLookup
      .get(fcId)
      .collect {
        case IsChoice(choice) =>
          val optionsList = getChoiceOptions(choice)
          val orderedKeys = optionsList.keys.toSet
          formModelVisibilityOptics
            .evalAndApplyTypeInfo(typeInfo)
            .optionRepresentation
            .map { repr =>
              repr.collect { case key if orderedKeys.contains(key) => apply(optionsList(key), markDown) }
            }
            .getOrElse(Nil)
        case IsRevealingChoice(revealingChoice) =>
          revealingChoice.options.map(c => apply(summaryValueOrLabel(c.choice.label, c.choice.summaryValue), markDown))
        case _ =>
          List(stringRepresentation(typeInfo))
      }
      .getOrElse(Seq.empty)
      .toList

  private def evalChoiceAsString(fcId: FormComponentId, typeInfo: TypeInfo, markDown: Boolean): String =
    evalChoice(fcId, typeInfo, markDown).mkString(", ")

  private def summaryValueOrLabel(label: SmartString, summaryValue: Option[SmartString]): SmartString =
    summaryValue match {
      case Some(ss) => ss
      case _        => label
    }

  private def getChoiceOptions(choice: Choice): Map[String, SmartString] =
    choice.options.zipWithIndex
      .map {
        case (OptionData.IndexBased(label, _, _, _, summaryValue), i) =>
          i.toString -> summaryValueOrLabel(label, summaryValue)
        case (OptionData.ValueBased(label, _, _, _, OptionDataValue.StringBased(value), summaryValue), _) =>
          value -> summaryValueOrLabel(label, summaryValue)
        case (OptionData.ValueBased(label, _, _, _, OptionDataValue.ExprBased(expr), summaryValue), _) =>
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(expr)
            .stringRepresentation(messages) -> summaryValueOrLabel(label, summaryValue)
      }
      .toList
      .toMap

  private def stringRepresentation(typeInfo: TypeInfo): String =
    formModelVisibilityOptics.evalAndApplyTypeInfo(typeInfo).stringRepresentation(messages)

  private def addressRepresentation(typeInfo: TypeInfo): List[String] =
    formModelVisibilityOptics.evalAndApplyTypeInfo(typeInfo).addressRepresentation

  private def govukListRepresentation(
    typeInfo: TypeInfo,
    isBulleted: Boolean,
    markDown: Boolean,
    fcId: FormComponentId
  ): String = {
    val choiceList: List[String] = typeInfo.staticTypeData match {
      case StaticTypeData(ChoiceSelection, None) => evalChoice(fcId, typeInfo, markDown)
      case _ =>
        formModelVisibilityOptics
          .evalAndApplyTypeInfo(typeInfo)
          .listRepresentation(messages)
    }

    val lines = typeInfo.staticTypeData.textConstraint
      .map(c => choiceList.map(value => TextFormatter.componentTextReadonly(value, c)(l)))
      .getOrElse(choiceList)

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

    val elements = list.map(value => s"""<li>${HtmlFormat.escape(value).body}</li>""").mkString("")

    first + elements + last
  }
}
