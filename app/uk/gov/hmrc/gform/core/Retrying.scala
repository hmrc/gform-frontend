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

package uk.gov.hmrc.gform.core

import org.apache.pekko.actor.Scheduler
import org.apache.pekko.pattern.after
import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration.FiniteDuration

// Asynchronous retry for Future in Scala
// https://gist.github.com/viktorklang/9414163
trait Retrying {

  def retry[T](f: => Future[T], delay: FiniteDuration, retries: Int)(implicit
    ec: ExecutionContext,
    s: Scheduler
  ): Future[T] =
    f recoverWith { case _ if retries > 0 => after(delay, s)(retry(f, delay, retries - 1)) }
}
