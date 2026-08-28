/*
 * Copyright 2026 HM Revenue & Customs
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
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.{ FormModelSupport, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormModelOptics, TaskIdTaskStatusMapping }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, Equals, FormKind, IncludeIf, Section, Task, TaskId, TaskSection, TaskStatus => TaskStatusExpr, Value }
import uk.gov.hmrc.gform.sharedmodel.LangADT

class TaskListUtilsSpec extends AnyWordSpecLike with Matchers with FormModelSupport {

  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))
  implicit val l: LangADT = LangADT.En

  private val section0: Section = mkSection(mkFormComponent("task0_field", Value))
  private val section1: Section = mkSection(mkFormComponent("task1_field", Value))

  private val task0 = Task(
    id = Some(TaskId("task-0")),
    title = toSmartString("Task 0"),
    sections = NonEmptyList.of(section0),
    summarySection = None,
    declarationSection = None,
    includeIf = None,
    caption = None,
    startIf = None,
    notRequiredIf = None,
    hint = None
  )

  private val task1 = Task(
    id = Some(TaskId("task-1")),
    title = toSmartString("Task 1"),
    sections = NonEmptyList.of(section1),
    summarySection = None,
    declarationSection = None,
    includeIf = None,
    caption = None,
    startIf = Some(IncludeIf(Equals(TaskStatusExpr(TaskId("task-0")), Constant("Completed")))),
    notRequiredIf = None,
    hint = None
  )

  private val formTemplate =
    mkFormTemplate(List(section0, section1)).copy(
      formKind = FormKind.TaskList(
        NonEmptyList.of(
          TaskSection(toSmartString("Section"), NonEmptyList.of(task0, task1))
        )
      )
    )

  "CannotStartYetResolver.create" should {
    "respect refreshed TaskIdTaskStatus mapping for startIf(taskStatus(...))" in {
      val cacheStale = mkAuthCacheWithForm(formTemplate)
      val taskCoordinatesMap = TaskListUtils.toTaskCoordinatesMap(formTemplate)

      val staleOptics: FormModelOptics =
        FormModelOptics.mkFormModelOptics[SectionSelectorType.Normal](cacheStale.variadicFormData, cacheStale)
      val staleResolver = CannotStartYetResolver.create(staleOptics, taskCoordinatesMap)

      val cacheWithFreshStatuses = cacheStale.copy(
        form = cacheStale.form.copy(
          taskIdTaskStatus = TaskIdTaskStatusMapping(Map(TaskId("task-0") -> TaskStatus.Completed))
        )
      )
      val refreshedOptics: FormModelOptics =
        FormModelOptics.mkFormModelOptics[SectionSelectorType.Normal](
          cacheWithFreshStatuses.variadicFormData,
          cacheWithFreshStatuses
        )
      val refreshedResolver = CannotStartYetResolver.create(refreshedOptics, taskCoordinatesMap)

      val task0 = TaskListUtils.evalCoordinates(0, 0)
      val task1 = TaskListUtils.evalCoordinates(0, 1)
      val initialStatuses = NonEmptyList.of(task0 -> TaskStatus.Completed, task1 -> TaskStatus.NotStarted)
      val staleStatuses = staleResolver.resolveCannotStartYet(initialStatuses).toList.toMap
      val refreshedStatuses = refreshedResolver.resolveCannotStartYet(initialStatuses).toList.toMap

      staleStatuses(task1) shouldBe TaskStatus.CannotStartYet
      refreshedStatuses(task1) shouldBe TaskStatus.NotStarted
    }
  }
}
