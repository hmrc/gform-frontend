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
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.graph.Evaluator
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormDataRecalculated, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormComponentId, FormTemplate }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }
import uk.gov.hmrc.http.HeaderCarrier

trait SmartStringEvaluatorFactory {
  def apply(
    recalculatedFormData: FormDataRecalculated,
    retrievals: MaterialisedRetrievals,
    form: Form,
    formTemplate: FormTemplate)(implicit l: LangADT, hc: HeaderCarrier): SmartStringEvaluator

  def apply(
    recalculatedFormData: FormDataRecalculated,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId,
    formTemplate: FormTemplate)(implicit l: LangADT, hc: HeaderCarrier): SmartStringEvaluator
}

class RealSmartStringEvaluatorFactory(evaluator: Evaluator[Id]) extends SmartStringEvaluatorFactory {
  def apply(
    recalculatedFormData: FormDataRecalculated,
    retrievals: MaterialisedRetrievals,
    form: Form,
    formTemplate: FormTemplate)(implicit l: LangADT, hc: HeaderCarrier): SmartStringEvaluator =
    apply(recalculatedFormData, retrievals, form.thirdPartyData, form.envelopeId, formTemplate)

  def apply(
    recalculatedFormData: FormDataRecalculated,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId,
    formTemplate: FormTemplate)(implicit l: LangADT, hc: HeaderCarrier): SmartStringEvaluator =
    new SmartStringEvaluator {
      override def apply(s: SmartString): String = {
        import scala.collection.JavaConverters._
        new MessageFormat(s.rawValue(l).replaceAll("'", "''"))
          .format(
            s.interpolations
              .map(eval(_))
              .asJava
              .toArray)
      }

      private def eval(expr: Expr): String =
        evaluator
          .evalAsString(
            recalculatedFormData,
            FormComponentId("dummy"),
            expr,
            retrievals,
            formTemplate,
            thirdPartyData,
            envelopeId)
          .getOrElse("")
    }
}
