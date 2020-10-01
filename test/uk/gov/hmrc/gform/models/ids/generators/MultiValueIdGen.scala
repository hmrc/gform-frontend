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
import uk.gov.hmrc.gform.models.ids.MultiValueId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen

trait MultiValueIdGen {

  def multiValueIdPureGen: Gen[MultiValueId.Pure] =
    for {
      modelComponentPureId <- ModelComponentIdGen.modelComponentIdPureGen
    } yield MultiValueId.Pure(modelComponentPureId)

  def multiValueIdMultiValueGen: Gen[MultiValueId.MultiValue] =
    for {
      modelComponentPureId <- ModelComponentIdGen.modelComponentIdPureGen
      atoms                <- PrimitiveGen.oneOrMoreGen(ModelComponentIdGen.modelComponentIdAtomicGen)
    } yield MultiValueId.MultiValue(modelComponentPureId, atoms)

  def multiValueIdGen: Gen[MultiValueId] = Gen.oneOf(multiValueIdPureGen, multiValueIdMultiValueGen)
}

object MultiValueIdGen extends MultiValueIdGen
