/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import cats.Monad
import cats.syntax.applicative._
import scala.language.higherKinds
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.graph.{ Data, Evaluator, RecData }
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExprEval, Eeitt, FormTemplate }
import uk.gov.hmrc.http.HeaderCarrier

trait GraphSpec {
  private def eeittPrepop[F[_]: Monad](
    eeitt: Eeitt,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    hc: HeaderCarrier): F[String] = "data-returned-from-eeitt".pure[F]

  def evaluator[F[_]: Monad]: Evaluator[F] = new Evaluator[F](eeittPrepop[F])
  def booleanExprEval[F[_]: Monad]: BooleanExprEval[F] = new BooleanExprEval[F](evaluator)

  protected def mkFormDataRecalculated(data: Data): FormDataRecalculated =
    FormDataRecalculated.empty.copy(recData = RecData.fromData(data))

}
