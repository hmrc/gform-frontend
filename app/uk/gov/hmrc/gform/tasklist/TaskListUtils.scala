/*
 * Copyright 2022 HM Revenue & Customs
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

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormKind, FormTemplate, Task, TaskNumber, TaskSection, TaskSectionNumber }

object TaskListUtils {

  def withTask[A](formTemplate: FormTemplate, taskSectionNumber: TaskSectionNumber, taskNumber: TaskNumber)(
    f: Task => A
  ): A =
    withTaskList(formTemplate) { taskList =>
      taskList.sections.toList
        .lift(taskSectionNumber.value)
        .fold(invalidTaskSectionNumber(formTemplate, taskSectionNumber)) { taskSection =>
          taskSection.tasks.toList
            .lift(taskNumber.value)
            .fold(invalidTaskNumber(formTemplate, taskSectionNumber, taskNumber))(f)
        }
    }

  def withTaskSection[A](formTemplate: FormTemplate, taskSectionNumber: TaskSectionNumber)(
    f: TaskSection => A
  ): A =
    withTaskList(formTemplate) { taskList =>
      taskList.sections.toList
        .lift(taskSectionNumber.value)
        .fold(invalidTaskSectionNumber(formTemplate, taskSectionNumber))(f)
    }

  def withTaskList[A](formTemplate: FormTemplate)(f: FormKind.TaskList => A): A =
    formTemplate.formKind
      .fold(_ => throw new Exception(s"Form template: ${formTemplate._id.value} is not task list"))(f)

  private def invalidTaskSectionNumber(formTemplate: FormTemplate, taskSectionNumber: TaskSectionNumber) =
    throw new Exception(s"Task List: ${formTemplate._id.value} does not contains task section $taskSectionNumber")

  private def invalidTaskNumber(
    formTemplate: FormTemplate,
    taskSectionNumber: TaskSectionNumber,
    taskNumber: TaskNumber
  ) =
    throw new Exception(
      s"Task List: ${formTemplate._id.value} task section $taskSectionNumber does not contains task number $taskNumber"
    )

}
