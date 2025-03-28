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

import org.mongodb.scala.model.{ IndexModel, IndexOptions, Indexes }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateCache

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext

class CacheModule(controllersModule: ControllersModule, mongoModule: MongoModule, configModule: ConfigModule)(implicit
  ex: ExecutionContext
) {
  private val formTemplateMetadataRepo: Repo[FormTemplateCache] =
    new Repo[FormTemplateCache](
      "formTemplateCache",
      mongoModule.mongoComponent,
      _._id.value,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("updatedAt"),
          IndexOptions()
            .background(false)
            .name("updatedAt")
            .expireAfter(configModule.appConfig.formTemplateCacheTTL.toMillis, TimeUnit.MILLISECONDS)
        )
      )
    )

  val formTemplateCacheService = new FormTemplateCacheService(formTemplateMetadataRepo)

  val formTemplateCacheController =
    new FormTemplateCacheController(controllersModule.messagesControllerComponents, formTemplateCacheService)
}
