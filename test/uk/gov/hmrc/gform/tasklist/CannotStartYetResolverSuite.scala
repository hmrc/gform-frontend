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

package uk.gov.hmrc.gform.tasklist

import cats.data.NonEmptyList
import munit.FunSuite
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, TaskNumber, TaskSectionNumber }

class CannotStartYetResolverSuite extends FunSuite {

  val task0 = Coordinates(TaskSectionNumber(0), TaskNumber(0))
  val task1 = Coordinates(TaskSectionNumber(0), TaskNumber(1))

  val initialStatuses = NonEmptyList.of((task0 -> TaskStatus.InProgress), (task1 -> TaskStatus.NotStarted))

  val cannotStartYetResolver = new CannotStartYetResolver(
    Map(
      task0 -> Set(BaseComponentId("foo")),
      task1 -> Set(BaseComponentId("bar"))
    ),
    Map(
      task0 -> Set.empty,
      task1 -> Set(BaseComponentId("foo"))
    )
  )
  test("lookup") {
    val finalStatuses = cannotStartYetResolver.resolveCannotStartYet(initialStatuses)
    assertEquals(finalStatuses, NonEmptyList.of((task0 -> TaskStatus.InProgress), (task1 -> TaskStatus.CannotStartYet)))

  }
}
