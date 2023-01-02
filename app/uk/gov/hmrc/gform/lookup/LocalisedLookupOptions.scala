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

package uk.gov.hmrc.gform.lookup

import uk.gov.hmrc.gform.sharedmodel.LangADT

case class LocalisedLookupOptions(m: Map[LangADT, LookupOptions]) extends AnyVal {

  def lookupInfo(label: LookupLabel)(implicit l: LangADT): Option[LookupInfo] = m.get(l).flatMap(_.get(label))

  def fold[A](empty: A)(f: LookupOptions => A)(implicit l: LangADT): A = m.get(l).fold(empty)(f)

  def process[A](f: LookupOptions => List[A])(implicit l: LangADT): List[A] = fold(List.empty[A])(f)
}
