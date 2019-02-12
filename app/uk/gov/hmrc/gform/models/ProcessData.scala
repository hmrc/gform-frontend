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

package uk.gov.hmrc.gform.models

import cats.{ Monad, MonadError }
import cats.instances.future._
import cats.instances.int._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.eq._
import scala.concurrent.Future
import scala.util.Try
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.graph.{ Data, Recalculation }
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.{ FormDataRecalculated, FormField, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, Section }
import uk.gov.hmrc.http.HeaderCarrier

case class ProcessData(data: FormDataRecalculated, sections: List[Section], visitIndex: VisitIndex)

class ProcessDataService[F[_]: Monad, E](recalculation: Recalculation[F, E]) {

  private def toData(xs: Seq[FormField]): Data = xs.map(x => x.id -> List(x.value)).toMap

  def updateSectionVisits(dataRaw: Data, sections: List[Section], mongoSections: List[Section]): Set[Int] = {
    val visitIndex = dataRaw
      .get(FormComponentId(VisitIndex.key))
      .flatMap(_.headOption)
      .map(VisitIndex.fromString)
      .getOrElse(VisitIndex.empty)

    visitIndex.visitsIndex
      .map { index =>
        Try(mongoSections(index)).toOption.fold(-1) { section =>
          val firstComponentId = section.fields.head.id
          sections.indexWhere { s =>
            s.fields.head.id === firstComponentId
          }
        }
      }
      .filterNot(_ === -1)
  }

  def getProcessData(dataRaw: Data, cache: AuthCacheWithForm)(
    implicit hc: HeaderCarrier,
    me: MonadError[F, E]): F[ProcessData] =
    for {
      browserRecalculated <- recalculateDataAndSections(dataRaw, cache)
      mongoRecalculated   <- recalculateDataAndSections(toData(cache.form.formData.fields), cache)
    } yield {

      val (data, sections) = browserRecalculated
      val (_, mongoSections) = mongoRecalculated

      val newVisitIndex = updateSectionVisits(dataRaw, sections, mongoSections)

      ProcessData(data, sections, VisitIndex(newVisitIndex))
    }

  def recalculateDataAndSections(data: Data, cache: AuthCacheWithForm)(
    implicit hc: HeaderCarrier,
    me: MonadError[F, E]): F[(FormDataRecalculated, List[Section])] =
    for {
      formDataRecalculated <- recalculation
                               .recalculateFormData(data, cache.formTemplate, cache.retrievals, cache.form.envelopeId)
    } yield {
      val sections = RepeatingComponentService.getAllSections(cache.formTemplate, formDataRecalculated)
      (formDataRecalculated, sections)
    }

}
