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

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit.{ MILLISECONDS, SECONDS }

import com.codahale.metrics.{ MetricFilter, SharedMetricRegistries }
import com.codahale.metrics.graphite.{ Graphite, GraphiteReporter }
import play.api.Logger

class GraphiteService(
    enabled: Boolean = false,
    graphiteInterval: Long,
    host: String, //    val host = config.getString("graphite.host").getOrElse("graphite")
    port: Int, //  val port = config.getInt("graphite.port").getOrElse(2003)
    prefix: String, // = config.getString("graphite.prefix").getOrElse(s"tax.${configModule.appConfig.appName}")
    registryName: String //config.getString("metrics.name").getOrElse("default")
) {

  def startReporter(): Unit = if (enabled) {
    Logger.info("Graphite metrics enabled, starting the reporter")
    reporter.start(graphiteInterval, SECONDS)
  }

  private val graphite = new Graphite(new InetSocketAddress(host, port))

  private val reporter: GraphiteReporter = GraphiteReporter.forRegistry(SharedMetricRegistries.getOrCreate(registryName))
    .prefixedWith(s"$prefix.${java.net.InetAddress.getLocalHost.getHostName}")
    .convertRatesTo(SECONDS)
    .convertDurationsTo(MILLISECONDS)
    .filter(MetricFilter.ALL)
    .build(graphite)
}
