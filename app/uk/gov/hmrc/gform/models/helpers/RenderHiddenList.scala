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

package uk.gov.hmrc.gform.models.helpers

import play.twirl.api.Html
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelRenderPageOptics }
import uk.gov.hmrc.gform.views.html

case class RenderHiddenList(renderHidden: List[RenderHidden]) extends AnyVal {
  def render(formModelRenderPageOptics: FormModelRenderPageOptics[DataOrigin.Mongo]): List[Html] =
    renderHidden.flatMap {
      case RenderHidden.EmptyHidden(formComponents) =>
        formComponents.map(fc => html.form.snippets.hidden_field_empty(fc.id))
      case RenderHidden.ValueHidden(formComponents) =>
        formComponents.flatMap(_.multiValueId.toModelComponentIds).map { modelComponentId =>
          val formField = formModelRenderPageOptics.toFormField(modelComponentId)
          html.form.snippets.hidden_field(formField)
        }
    }

}
