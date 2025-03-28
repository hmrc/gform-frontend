/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import java.time.Instant
import java.util.concurrent.{ ConcurrentHashMap, ConcurrentMap }

class FormTemplateContextCacheManager {
  private case class FormTemplateContextCache(instant: Instant, formTemplateContext: FormTemplateContext)
  private val cacheMap: ConcurrentMap[String, FormTemplateContextCache] = new ConcurrentHashMap(16)

  def getFormTemplateContext(formTemplateId: FormTemplateId, instant: Instant): Option[FormTemplateContext] =
    Option(cacheMap.get(formTemplateId.value)) match {
      case Some(cache) if cache.instant == instant => Some(cache.formTemplateContext)
      case _                                       => None
    }

  def putFormTemplateContext(formTemplateContext: FormTemplateContext, instant: Instant): FormTemplateContext =
    cacheMap
      .compute(
        formTemplateContext.formTemplate._id.value,
        (_, existingCache) =>
          Option(existingCache) match {
            case Some(cache) if cache.instant == instant => cache
            case _                                       => FormTemplateContextCache(instant, formTemplateContext)
          }
      )
      .formTemplateContext

}
