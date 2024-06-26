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

package uk.gov.hmrc.gform.metrics

import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.play.bootstrap.metrics.{ DisabledMetrics, DisabledMetricsFilter, MetricsFilter, MetricsFilterImpl, MetricsImpl }

import scala.concurrent.ExecutionContext

class MetricsModule(
  configModule: ConfigModule,
  akkaModule: AkkaModule,
  ec: ExecutionContext
) {

  val kenshooMetricsEnabled = configModule.playConfiguration.get[Boolean]("metrics.enabled") // metrics collection

  val (metrics, metricsFilter) = if (kenshooMetricsEnabled) {

    val metrics = new MetricsImpl(configModule.context.lifecycle, configModule.context.initialConfiguration)
    val metricsFilter: MetricsFilter = new MetricsFilterImpl(metrics)(akkaModule.materializer, ec)

    (metrics, metricsFilter)
  } else {

    val metrics = new DisabledMetrics()
    val metricsFilter: MetricsFilter = new DisabledMetricsFilter()(akkaModule.materializer)

    (metrics, metricsFilter)
  }

}
