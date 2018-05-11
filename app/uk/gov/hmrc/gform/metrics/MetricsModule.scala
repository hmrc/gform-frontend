/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.metrics

import com.kenshoo.play.metrics.{ MetricsController, MetricsFilter, MetricsFilterImpl, MetricsImpl }
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule

class MetricsModule(playBuiltInsModule: PlayBuiltInsModule, akkaModule: AkkaModule, configModule: ConfigModule) {

  val metricsFilter: MetricsFilter = new MetricsFilterImpl(metrics)(akkaModule.materializer)

  val metricsController: MetricsController = new MetricsController(metrics)

  val graphiteService = new GraphiteService(
    // format: OFF
    enabled          = metricsPluginEnabled && graphitePublisherEnabled,
    graphiteInterval = configModule.playConfiguration.getLong("microservice.metrics.graphite.interval").getOrElse(10L),
    host             = configModule.typesafeConfig.getString("microservice.metrics.graphite.host"),
    port             = configModule.typesafeConfig.getInt("microservice.metrics.graphite.port"),
    prefix           = configModule.typesafeConfig.getString("microservice.metrics.graphite.prefix"),
    registryName     = configModule.typesafeConfig.getString("metrics.name")
  // format: ON
  )

  private lazy val metrics = new MetricsImpl(
    playBuiltInsModule.context.lifecycle,
    playBuiltInsModule.context.initialConfiguration
  )
  private lazy val metricsPluginEnabled: Boolean = configModule.typesafeConfig.getBoolean("metrics.enabled")
  private lazy val graphitePublisherEnabled: Boolean =
    configModule.typesafeConfig.getBoolean("microservice.metrics.graphite.enabled")

}
