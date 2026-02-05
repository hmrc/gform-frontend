import * as styles from "bundle-text:../section/content.css";
import { useEffect, useRef, useState } from "preact/hooks";
import {
  Coordinates,
  ContentScriptRequest,
  UpdateTaskList,
  TaskSection,
  SectionNumber,
  TaskSectionUpdateRequest,
  SectionDetails,
  Task,
  TaskSectionPart,
  TaskSummarySectionPart,
  TaskDeclarationSectionPart,
  UpdateByPath,
  TemplateBatchUpdate,
} from "../types";
import { MessageKind, SmartString } from "../types";
import { SmartStringDiv, SmartStringInputDeprecated } from "../section/useSmartString";
import { onMessageHandler } from "../background/index";

const taskPath = (coordinates: Coordinates): string => {
  return ".sections[" + coordinates.taskSectionNumber.toString() + "].tasks[" + coordinates.taskNumber.toString() + "]";
};

export const TaskListSectionControllerFactory =
  (
    formTemplateId: string,
    host: string,
    taskSection: TaskSection,
    taskSectionHeadingElement: HTMLHeadingElement,
    taskSectionNumber: number,
    maybeAccessCode: string | null,
  ) =>
  () => {
    const refreshTaskSectionTitle = (htmlContent: string): void => {
      taskSectionHeadingElement.textContent = htmlContent;
    };

    const tasks: Task[] = taskSection.tasks;
    const taskTitles: SmartString[] = tasks.map((task) => task.title);
    const taskCaptions: SmartString[] = tasks.map((task) => (task.caption !== undefined ? task.caption : ""));
    const taskHints: SmartString[] = tasks.map((task) => (task.hint !== undefined ? task.hint : ""));
    const taskSummarySections: boolean[] = tasks.map((task) => task.summarySection !== undefined);
    const taskDeclarationSections: boolean[] = tasks.map((task) => task.declarationSection !== undefined);

    const content = useRef<HTMLDivElement>(null);
    const serverError = useRef<HTMLDivElement>(null);

    const titleInput = useRef<SmartStringDiv>(null);
    const taskTitleInputs = taskTitles.map((task) => useRef<SmartStringDiv>(null));
    const taskCaptionInputs = taskCaptions.map((task) => useRef<SmartStringDiv>(null));
    const taskHintInputs = taskHints.map((task) => useRef<SmartStringDiv>(null));
    const taskSummarySectionsInputs = taskSummarySections.map((task) => useRef<HTMLInputElement>(null));
    const taskDeclarationSectionsInputs = taskDeclarationSections.map((task) => useRef<HTMLInputElement>(null));

    const [titleValue, setTitleValue] = useState(taskSection.title);
    const [taskTitlesValue, setTaskTitlesValue] = useState(taskTitles);
    const [taskCaptionsValue, setTaskCaptionsValue] = useState(taskCaptions);
    const [taskHintsValue, setTaskHintsValue] = useState(taskHints);
    const [taskSummarySectionsValue, setTaskSummarySectionsValue] = useState(taskSummarySections);
    const [taskDeclarationSectionsValue, setTaskDeclarationSectionsValue] = useState(taskDeclarationSections);
    const [buttonLabel, setButtonLabel] = useState("");

    const registerClickHandler = (element: HTMLHeadingElement) => {
      element.addEventListener("click", (event) => {
        content.current?.classList.remove("hidden");
        setButtonLabel("Update section");
      });
    };

    useEffect(() => {
      registerClickHandler(taskSectionHeadingElement);
    }, []);

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      content.current?.classList.add("hidden");
    };

    const updateHandler = (e: MouseEvent) => {
      refreshTaskSection(true);
    };

    const labelKeyUp = (e: KeyboardEvent) => {
      refreshTaskSection(false);
    };

    const refreshTaskSection = (hideContent: boolean) => {
      const taskUpdates: UpdateByPath[] = taskTitleInputs
        .map((taskTitleInput, taskNumber) => {
          return [taskTitleInput, taskCaptionInputs[taskNumber], taskHintInputs[taskNumber]];
        })
        .map(([taskTitleInput, taskCaptionInput, taskHintInput], taskNumber) => {
          const taskSectionPayload: TaskSectionPart = {};
          if (taskTitleInput.current !== null) {
            taskSectionPayload["title"] = taskTitleInput.current.value;
          }
          if (taskCaptionInput.current !== null) {
            taskSectionPayload["caption"] = taskCaptionInput.current.value;
          }
          if (taskHintInput.current !== null) {
            taskSectionPayload["hint"] = taskHintInput.current.value;
          }
          const coordinates: Coordinates = {
            taskSectionNumber: taskSectionNumber,
            taskNumber: taskNumber,
          };
          const sectionPath = taskPath(coordinates);
          const payload: UpdateByPath = {
            payload: taskSectionPayload,
            path: sectionPath,
            focus: "task",
          };
          return payload;
        });

      const taskSummarySectionUpdates: UpdateByPath[] = taskSummarySectionsInputs.map(
        (taskSummarySectionInput, taskNumber) => {
          const taskSectionPayload: TaskSummarySectionPart = {};
          if (taskSummarySectionInput.current !== null) {
            taskSectionPayload["title"] = taskSummarySectionInput.current.checked ? "Check your answers" : "";
          }

          const coordinates: Coordinates = {
            taskSectionNumber: taskSectionNumber,
            taskNumber: taskNumber,
          };
          const sectionPath = taskPath(coordinates);
          const payload: UpdateByPath = {
            payload: taskSectionPayload,
            path: sectionPath,
            focus: "taskSummarySection",
          };
          return payload;
        },
      );

      const taskDeclarationSectionUpdates: UpdateByPath[] = taskDeclarationSectionsInputs.map(
        (taskDeclarationSectionInput, taskNumber) => {
          const taskSectionPayload: TaskDeclarationSectionPart = {};
          if (taskDeclarationSectionInput.current !== null) {
            if (taskDeclarationSectionInput.current.checked) {
              taskSectionPayload["title"] = "Declaration";
              taskSectionPayload["fields"] = [];
            } else {
              taskSectionPayload["title"] = "";
              taskSectionPayload["fields"] = [];
            }
          }
          const coordinates: Coordinates = {
            taskSectionNumber: taskSectionNumber,
            taskNumber: taskNumber,
          };
          const sectionPath = taskPath(coordinates);
          const payload: UpdateByPath = {
            payload: taskSectionPayload,
            path: sectionPath,
            focus: "taskDeclarationSection",
          };
          return payload;
        },
      );

      const taskSectionPayload: TaskSectionPart = {};
      if (titleInput.current !== null) {
        const titleCurrent: SmartStringDiv = titleInput.current;

        const value = titleCurrent.value;
        taskSectionPayload["title"] = value;
      }

      const sectionPath = ".sections[" + taskSectionNumber + "]";

      const taskListSectionNumber = SectionNumber.TaskListSectionNumber(
        taskSectionNumber,
        0,
        SectionNumber.NormalPage(0),
      ); // only taskSectionNumber matters

      const taskSectionUpdates: UpdateByPath = {
        payload: taskSectionPayload,
        path: sectionPath,
        focus: "taskSection",
      };

      const allUpdates = taskUpdates.concat(taskSummarySectionUpdates).concat(taskDeclarationSectionUpdates);
      allUpdates.push(taskSectionUpdates);

      const batchUpdates: TemplateBatchUpdate = {
        updates: allUpdates,
      };

      const data: TaskSectionUpdateRequest = {
        batch: batchUpdates,
        sectionNumber: taskListSectionNumber,
        maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
      };

      const updateRequest: ContentScriptRequest = {
        host: host,
        kind: MessageKind.UpdateTask,
        formTemplateId: formTemplateId,
        data: data,
      };

      onMessageHandler<UpdateTaskList>(updateRequest, (response) => {
        if (response.error === undefined) {
          if (response.taskSectionTitle !== undefined) {
            refreshTaskSectionTitle(response.taskSectionTitle);
          }

          if (response.taskTitles !== undefined) {
            response.taskTitles.map((taskTitle, index) => {
              const liIndex = index + 1;
              const selector = `div > :nth-child(${taskSectionNumber + 1} of ul) > :nth-child(${liIndex} of li) > .govuk-task-list__name-and-hint`;
              const taskListNameAndHint: Element | null = document.querySelector(selector);

              if (taskListNameAndHint !== null) {
                const hasLink = taskListNameAndHint.querySelector("a");
                const hasDiv = taskListNameAndHint.querySelector("div:not(.govuk-task-list__hint)");
                const hasHintDiv = taskListNameAndHint.querySelector("div.govuk-task-list__hint");

                if (hasLink !== null) {
                  hasLink.textContent = taskTitle;
                }
                if (hasDiv !== null) {
                  hasDiv.textContent = taskTitle;
                }

                if (response.taskHints !== undefined && response.taskHints.length >= index + 1) {
                  const newHintValue = response.taskHints[index];
                  if (hasHintDiv !== null) {
                    if (newHintValue.length === 0) {
                      hasHintDiv.parentNode.removeChild(hasHintDiv);
                    } else {
                      hasHintDiv.textContent = newHintValue;
                    }
                  } else {
                    if (newHintValue.length > 0) {
                      const newHintDiv = document.createElement("div");
                      newHintDiv.classList.add("govuk-task-list__hint");
                      newHintDiv.textContent = newHintValue;
                      if (hasLink !== null) {
                        hasLink.parentNode.insertBefore(newHintDiv, hasLink.nextSibling);
                      }
                      if (hasDiv !== null) {
                        hasDiv.parentNode.insertBefore(newHintDiv, hasDiv.nextSibling);
                      }
                    }
                  }
                }
              }
            });
          }

          serverError.current?.classList.add("hidden");
          if (hideContent) {
            content.current?.classList.add("hidden");
          }
        } else {
          if (serverError.current !== null) {
            if (response.error !== undefined) {
              serverError.current.innerText = response.error;
            } else {
              serverError.current.innerText = JSON.stringify(response);
            }
          }
          serverError.current?.classList.remove("hidden");
        }
      });
    };

    const onTitleChange = (e: Event) => {
      if (titleInput.current !== null) {
        setTitleValue(titleInput.current.value);
      }
    };

    const taskTitleKeyUp = (index: number) => (e: Event) => {
      const current = taskTitleInputs[index].current;
      if (current !== null) {
        taskTitlesValue[index] = current.value;
        setTaskTitlesValue(taskTitlesValue);
      }

      refreshTaskSection(false);
    };

    const taskCaptionKeyUp = (index: number) => (e: Event) => {
      const current = taskCaptionInputs[index].current;
      if (current !== null) {
        taskCaptionsValue[index] = current.value;
        setTaskCaptionsValue(taskCaptionsValue);
      }

      refreshTaskSection(false);
    };

    const taskHintKeyUp = (index: number) => (e: Event) => {
      const current = taskHintInputs[index].current;
      if (current !== null) {
        taskHintsValue[index] = current.value;
        setTaskHintsValue(taskHintsValue);
      }

      refreshTaskSection(false);
    };

    const summaryIncludeToggle = (index: number) => (e: MouseEvent) => {
      const current = taskSummarySectionsInputs[index].current;
      if (current !== null) {
        taskSummarySectionsValue[index] = current.checked;
        setTaskSummarySectionsValue(taskSummarySectionsValue);
      }
      refreshTaskSection(false);
    };

    const declarationIncludeToggle = (index: number) => (e: MouseEvent) => {
      const current = taskDeclarationSectionsInputs[index].current;
      if (current !== null) {
        taskDeclarationSectionsValue[index] = current.checked;
        setTaskDeclarationSectionsValue(taskDeclarationSectionsValue);
      }
      refreshTaskSection(false);
    };

    const tasksControls = tasks.map((task, index) => {
      const editTitleId = `edit-title-${index}`;
      const editCaptionId = `edit-caption-${index}`;
      const editHintId = `edit-hint-${index}`;
      const includeSummaryId = `task-summary-section-${index}`;
      const includeDeclarationId = `task-declaration-section-${index}`;
      return (
        <>
          <hr />
          <SmartStringDiv ref={taskTitleInputs[index]}>
            <SmartStringInputDeprecated
              id={editTitleId}
              class="form-control"
              value={taskTitlesValue[index]}
              onKeyUp={taskTitleKeyUp(index)}
            >
              Task title
            </SmartStringInputDeprecated>
          </SmartStringDiv>
          <SmartStringDiv ref={taskCaptionInputs[index]}>
            <SmartStringInputDeprecated
              id={editCaptionId}
              class="form-control"
              value={taskCaptionsValue[index]}
              onKeyUp={taskCaptionKeyUp(index)}
            >
              Task caption
            </SmartStringInputDeprecated>
          </SmartStringDiv>
          <SmartStringDiv ref={taskHintInputs[index]}>
            <SmartStringInputDeprecated
              id={editHintId}
              class="form-control"
              value={taskHintsValue[index]}
              onKeyUp={taskHintKeyUp(index)}
            >
              Task hint
            </SmartStringInputDeprecated>
          </SmartStringDiv>
          <div>
            <input
              type="checkbox"
              id={includeSummaryId}
              ref={taskSummarySectionsInputs[index]}
              checked={taskSummarySectionsValue[index]}
              onClick={summaryIncludeToggle(index)}
            />
            <label for={includeSummaryId}>Include task summary page</label>
          </div>
          <div>
            <input
              type="checkbox"
              id={includeDeclarationId}
              ref={taskDeclarationSectionsInputs[index]}
              checked={taskDeclarationSectionsValue[index]}
              onClick={declarationIncludeToggle(index)}
            />
            <label for={includeDeclarationId}>Include declaration section page</label>
          </div>
        </>
      );
    });

    return (
      <div id="edit-task-list-section" class="info hidden" ref={content}>
        <style>{styles}</style>
        <SmartStringDiv ref={titleInput}>
          <SmartStringInputDeprecated
            id="edit-title"
            class="form-control"
            value={titleValue}
            onChange={onTitleChange}
            onKeyUp={labelKeyUp}
          >
            Task section title
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <div style="text-align: center;">Tasks</div>
        {tasksControls}
        <button id="update-button" class="btn btn-success" onClick={updateHandler}>
          {buttonLabel}
        </button>
        <button id="cancel-button" class="btn btn-secondary" onClick={cancelClickHandler}>
          Cancel
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
