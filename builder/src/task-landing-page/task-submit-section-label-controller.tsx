import * as styles from "bundle-text:../section/content.css";
import { useEffect, useRef, useState } from "preact/hooks";
import {
  UpdateByPath,
  TemplateBatchUpdate,
  TaskSubmitSectionUpdateRequest,
  ContentScriptRequest,
  MessageKind,
  SubmitSection,
  SubmitSectionPart,
  UpdateSubmitSection,
} from "../types";

import { SmartStringDiv, SmartStringInputDeprecated } from "../section/useSmartString";

import { onMessageHandler } from "../background/index";

export const TaskSubmitSectionLabelControllerFactory =
  (
    formTemplateId: string,
    host: string,
    submitSection: SubmitSection,
    submitSectionElement: HTMLHeadingElement,
    maybeAccessCode: string | null,
  ) =>
  () => {
    const refreshSubmitSectionLabel = (htmlContent: string): void => {
      submitSectionElement.textContent = htmlContent;
    };

    const refreshSubmitSectionTaskLabel = (taskLabel: string): void => {
      const selector = "div.govuk-grid-column-two-thirds > ul:last-child > li > .govuk-task-list__name-and-hint";
      const submitSectionTask: Element | null = document.querySelector(selector);

      if (submitSectionTask !== null) {
        const hasLink = submitSectionTask.querySelector("a");
        const hasDiv = submitSectionTask.querySelector("div");
        if (hasLink !== null) {
          hasLink.textContent = taskLabel;
        }
        if (hasDiv !== null) {
          hasDiv.textContent = taskLabel;
        }
      }
    };

    const content = useRef<HTMLDivElement>(null);
    const serverError = useRef<HTMLDivElement>(null);

    const labelInput = useRef<SmartStringDiv>(null);
    const taskLabelInput = useRef<SmartStringDiv>(null);
    const labelContainer = useRef<HTMLDivElement>(null);
    const taskLabelContainer = useRef<HTMLDivElement>(null);

    const [labelValue, setLabelValue] = useState(submitSection.label);
    const [taskLabelValue, setTaskLabelValue] = useState(submitSection.taskLabel);
    const [buttonLabel, setButtonLabel] = useState("");

    const registerLabelClickHandler = (element: HTMLHeadingElement) => {
      element.addEventListener("click", (event) => {
        labelContainer.current?.classList.remove("hidden");
        taskLabelContainer.current?.classList.remove("hidden");
        content.current?.classList.remove("hidden");
        setButtonLabel("Update label");
      });
    };

    useEffect(() => {
      registerLabelClickHandler(submitSectionElement);
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

    const taskLabelKeyUp = (e: KeyboardEvent) => {
      refreshTaskSection(false);
    };

    const refreshTaskSection = (hideContent: boolean) => {
      let submitSectionPart: SubmitSectionPart = {};
      if (labelInput.current !== null) {
        const labelCurrent: SmartStringDiv = labelInput.current;

        submitSectionPart["label"] = labelCurrent.value;
      }
      if (taskLabelInput.current !== null) {
        const taskLabelCurrent: SmartStringDiv = taskLabelInput.current;

        submitSectionPart["taskLabel"] = taskLabelCurrent.value;
      }

      const submitSectionUpdate: UpdateByPath = {
        payload: submitSectionPart,
        path: ".submitSection",
        focus: "submitSection",
      };

      const batchUpdates: TemplateBatchUpdate = {
        updates: [submitSectionUpdate],
      };

      const data: TaskSubmitSectionUpdateRequest = {
        batch: batchUpdates,
        maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
      };

      const updateRequest: ContentScriptRequest = {
        host: host,
        kind: MessageKind.UpdateSubmitSection,
        formTemplateId: formTemplateId,
        data: data,
      };

      onMessageHandler<UpdateSubmitSection>(updateRequest, (response) => {
        if (response.error === undefined) {
          if (response.label !== undefined) {
            refreshSubmitSectionLabel(response.label);
          }
          if (response.taskLabel !== undefined) {
            refreshSubmitSectionTaskLabel(response.taskLabel);
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

    const onLabelChange = (e: Event) => {
      if (labelInput.current !== null) {
        setLabelValue(labelInput.current.value);
      }
    };

    const onTaskLabelChange = (e: Event) => {
      if (taskLabelInput.current !== null) {
        setTaskLabelValue(taskLabelInput.current.value);
      }
    };

    return (
      <div id="edit-submit-section" class="info hidden" ref={content}>
        <style>{styles}</style>
        <SmartStringDiv clazz="hidden" divRef={labelContainer} ref={labelInput}>
          <SmartStringInputDeprecated
            id="edit-label"
            class="form-control"
            value={labelValue}
            onChange={onLabelChange}
            onKeyUp={labelKeyUp}
          >
            Label
          </SmartStringInputDeprecated>
        </SmartStringDiv>

        <SmartStringDiv clazz="hidden" divRef={taskLabelContainer} ref={taskLabelInput}>
          <SmartStringInputDeprecated
            id="edit-task-label"
            class="form-control"
            value={taskLabelValue}
            onChange={onTaskLabelChange}
            onKeyUp={taskLabelKeyUp}
          >
            Task Label
          </SmartStringInputDeprecated>
        </SmartStringDiv>
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
