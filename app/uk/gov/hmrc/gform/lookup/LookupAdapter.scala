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

package uk.gov.hmrc.gform.lookup

import com.miguelfonseca.completely.IndexAdapter
import com.miguelfonseca.completely.data.ScoredObject
import com.miguelfonseca.completely.text.`match`.EditDistanceAutomaton
import com.miguelfonseca.completely.text.index.{ FuzzyIndex, PatriciaTrie }
import java.util.Collection

class LookupAdapter[A] extends IndexAdapter[A] {
  private val index: FuzzyIndex[A] = new PatriciaTrie()

  override def get(token: String): Collection[ScoredObject[A]] = {

    // Set threshold according to the token length
    val threshold: Double = Math.log(Math.max(token.length() - 1, 1).toDouble)
    index.getAny(new EditDistanceAutomaton(token, threshold))
  }

  override def put(token: String, value: A): Boolean =
    index.put(token, value)

  override def remove(value: A): Boolean =
    index.remove(value)
}
