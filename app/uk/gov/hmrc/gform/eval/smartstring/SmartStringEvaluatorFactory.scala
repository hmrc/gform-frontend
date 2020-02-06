/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.Id
import org.intellij.markdown.html.entities.EntityConverter
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.commons.ExprFormat
import uk.gov.hmrc.gform.commons.FormatType
import uk.gov.hmrc.gform.commons.FormatType.{ Default, FromText }
import uk.gov.hmrc.gform.models.FormModel
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SourceOrigin }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormModelOptics, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormComponentId, FormTemplate }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }
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

  private val markdownControlCharacters =
    List("""\""", "/", "`", "*", "_", "{", "}", "[", "]", "(", ")", "#", "+", "-", ".", "!")

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
                val interpolated = eval(interpolation)
                val formatType = ExprFormat.formatForExpr(interpolation, formModelVisibilityOptics.formModel)
                val formatted = formatType match {
                  case FormatType.Default        => interpolated
                  case FormatType.FromText(text) => TextFormatter.componentText(interpolated, text)
                }

                if (markDown) {
                  escapeMarkdown(formatted)
                } else {
                  formatted
                }
              }
              .asJava
              .toArray)

      }

      private def eval(expr: Expr): String = formModelVisibilityOptics.eval(expr)

      private def escapeMarkdown(s: String): String = {
        val replacedEntities = EntityConverter.INSTANCE.replaceEntities(s.replace("\n", ""), true, false)
        markdownControlCharacters.foldLeft(replacedEntities) {
          case (escaped, specialChar) =>
            escaped.replace(specialChar, "\\" + specialChar)
        }
      }
    }
}
