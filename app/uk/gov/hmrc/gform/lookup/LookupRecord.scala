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

import com.miguelfonseca.completely.data.Indexable
import java.util.{ List => jList }

import scala.collection.JavaConverters._

class LookupRecord(val value: String, val priority: LookupPriority, val keywords: LookupKeywords) extends Indexable {
  override val getFields: jList[String] =
    List(value, keywords.keywords.getOrElse("")).asJava

  def toLookupLabel = LookupLabel(value)
}
