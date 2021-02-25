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

package uk.gov.hmrc.gform.eval.smartstring

import java.text.MessageFormat
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.commons.MarkDownUtil.escapeMarkdown
import uk.gov.hmrc.gform.eval.TypeInfo
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, SmartString }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormCtx, FormTemplate, IsChoice, IsRevealingChoice }
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.http.HeaderCarrier

trait SmartStringEvaluatorFactory {
  def apply(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    retrievals: MaterialisedRetrievals,
    maybeAccessCode: Option[AccessCode],
    form: Form,
    formTemplate: FormTemplate)(implicit l: LangADT, hc: HeaderCarrier): SmartStringEvaluator

  def apply(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    maybeAccessCode: Option[AccessCode],
    envelopeId: EnvelopeId,
    formTemplate: FormTemplate)(implicit l: LangADT, hc: HeaderCarrier): SmartStringEvaluator
}

class RealSmartStringEvaluatorFactory() extends SmartStringEvaluatorFactory {

  def apply(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    retrievals: MaterialisedRetrievals,
    maybeAccessCode: Option[AccessCode],
    form: Form,
    formTemplate: FormTemplate
  )(
    implicit
    l: LangADT,
    hc: HeaderCarrier
  ): SmartStringEvaluator =
    apply(formModelVisibilityOptics, retrievals, form.thirdPartyData, maybeAccessCode, form.envelopeId, formTemplate)

  def apply(
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    maybeAccessCode: Option[AccessCode],
    envelopeId: EnvelopeId,
    formTemplate: FormTemplate
  )(
    implicit
    l: LangADT,
    hc: HeaderCarrier
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
              .toArray)

      }

      private def formatExpr(expr: Expr, markDown: Boolean): String = {

        val typeInfo: TypeInfo = formModelVisibilityOptics.formModel.toFirstOperandTypeInfo(expr)

        val interpolated = typeInfo.isChoiceSelection.fold(stringRepresentation(typeInfo)) {
          case FormCtx(formComponentId) =>
            formModelVisibilityOptics.formModel.fcLookup
              .get(formComponentId)
              .map({
                case IsChoice(choice) =>
                  val optionsList = choice.options.toList
                  mapChoiceSelectedIndexes(typeInfo, _.map(i => apply(optionsList(i), markDown)).mkString(","))
                case IsRevealingChoice(revealingChoice) =>
                  revealingChoice.options.map(c => apply(c.choice, markDown)).mkString(",")
                case _ =>
                  stringRepresentation(typeInfo)
              })
              .getOrElse("")
        }

        val formatted = typeInfo.staticTypeData.textConstraint.fold(interpolated) { textConstraint =>
          TextFormatter.componentTextReadonly(interpolated, textConstraint)
        }

        if (markDown) {
          escapeMarkdown(formatted)
        } else {
          formatted
        }
      }

      private def stringRepresentation(typeInfo: TypeInfo): String =
        formModelVisibilityOptics.evalAndApplyTypeInfo(typeInfo).stringRepresentation

      private def mapChoiceSelectedIndexes(typeInfo: TypeInfo, f: Seq[Int] => String): String =
        formModelVisibilityOptics
          .evalAndApplyTypeInfo(typeInfo)
          .optionRepresentation
          .fold(stringRepresentation(typeInfo))(f(_))
    }
}
