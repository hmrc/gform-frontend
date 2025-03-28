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

package uk.gov.hmrc.gform.cache

import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.repo.{ DeleteResult, Repo }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateCache, FormTemplateId }

import scala.concurrent.Future

class FormTemplateCacheService(repo: Repo[FormTemplateCache]) {
  def find(formTemplateId: FormTemplateId): Future[Option[FormTemplateCache]] = repo.find(formTemplateId.value)
  def save(cache: FormTemplateCache): FOpt[Unit] = repo.upsert(cache)
  def delete(formTemplateId: FormTemplateId): FOpt[DeleteResult] = repo.delete(formTemplateId.value)
}
