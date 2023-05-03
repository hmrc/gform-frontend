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

package uk.gov.hmrc

import _root_.play.api.libs.typedmap.TypedKey
import _root_.play.twirl.api.{ Html, HtmlFormat }
import cats.Monoid
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateContext

package object gform {
  val FormTemplateKey: TypedKey[FormTemplateContext] = TypedKey[FormTemplateContext]("form-template")

  implicit val monoidHtml: Monoid[Html] = Monoid.instance[Html](HtmlFormat.empty, (x, y) => HtmlFormat.fill(List(x, y)))
}
