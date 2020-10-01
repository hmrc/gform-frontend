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

package uk.gov.hmrc.gform.models.ids.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.generators.AtomGen

trait ModelComponentIdGen {
  def modelComponentIdPureGen: Gen[ModelComponentId.Pure] =
    for {
      indexedComponentId <- IndexedComponentIdGen.indexedComponentIdGen
    } yield ModelComponentId.Pure(indexedComponentId)
  def modelComponentIdAtomicGen: Gen[ModelComponentId.Atomic] =
    for {
      indexedComponentId <- IndexedComponentIdGen.indexedComponentIdGen
      atom               <- AtomGen.atomGen
    } yield ModelComponentId.Atomic(indexedComponentId, atom)
  def modelComponentIdGen: Gen[ModelComponentId] = Gen.oneOf(modelComponentIdPureGen, modelComponentIdAtomicGen)
}

object ModelComponentIdGen extends ModelComponentIdGen
