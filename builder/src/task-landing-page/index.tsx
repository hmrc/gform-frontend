import { render } from "preact";
import type {
  ContentScriptRequest,
  FormTemplate,
  FormTemplateId,
  ServerFormTemplateData,
  SubmitSection,
  Task,
  TaskSection,
} from "../types";
import { MessageKind, SectionNumber, NoteUpdateKind } from "../types";
import { TaskListSectionControllerFactory } from "./task-list-section-controller";
import { TaskSubmitSectionLabelControllerFactory } from "./task-submit-section-label-controller";
import { NoteComponentControllerFactory } from "../section/note-component/note-component-controller";
import { TaskLandingPageControllerFactory } from "./task-landing-page-controller";
import { replaceWithEnglishValue, attachShadowDom } from "../pure-functions";
import { onMessageHandler } from "../background/index";

export const activateTaskLandingPageBuilder = (urlMatch: RegExpMatchArray, url: string) => {
  const host = urlMatch![1];
  const formTemplateId: FormTemplateId = urlMatch![2];
  const accessCode0 = urlMatch![3];
  const accessCode = accessCode0 === "-" ? null : accessCode0;

  const fetchFormTemplate: ContentScriptRequest = {
    host: host,
    kind: MessageKind.FetchFormTemplate,
    formTemplateId: formTemplateId,
    data: accessCode,
  };

  const initiateTaskListSection = (
    inputFormComponentId: string,
    taskSectionHeadingEl: HTMLHeadingElement,
    taskSection: TaskSection,
    taskSectionNumber: number,
    maybeAccessCode: string | null,
  ) => {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", `task-list-section-${taskSectionNumber}-shadow-root`);
    taskSectionHeadingEl.insertAdjacentElement("afterend", attachmentDiv);
    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);
    if (content !== undefined) {
      const TaskListSectionFactory = TaskListSectionControllerFactory(
        inputFormComponentId,
        host,
        taskSection,
        taskSectionHeadingEl,
        taskSectionNumber,
        maybeAccessCode,
      );
      render(<TaskListSectionFactory />, content);
    }
  };

  const initiateSubmitSection = (
    inputFormComponentId: string,
    submitSection: SubmitSection,
    submitSectionEl: HTMLHeadingElement,
    maybeAccessCode: string | null,
  ) => {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "task-list-submit-section-label-shadow-root");
    submitSectionEl.insertAdjacentElement("afterend", attachmentDiv);
    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);
    if (content !== undefined) {
      const TaskSubmitSectionLabelFactory = TaskSubmitSectionLabelControllerFactory(
        inputFormComponentId,
        host,
        submitSection,
        submitSectionEl,
        maybeAccessCode,
      );
      render(<TaskSubmitSectionLabelFactory />, content);
    }
  };

  const initiateLandingPage = (
    inputFormComponentId: string,
    mainContent: HTMLElement,
    displayWidth: string | undefined,
  ) => {
    const headingEl = mainContent.querySelector("h1");
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "task-list-landing-page-shadow-root");
    if (headingEl) {
      headingEl.insertAdjacentElement("afterend", attachmentDiv);
      const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);
      if (content !== undefined) {
        const TaskLandingPageFactory = TaskLandingPageControllerFactory(
          inputFormComponentId,
          host,
          mainContent,
          headingEl,
          displayWidth,
        );
        render(<TaskLandingPageFactory />, content);
      }
    }
  };

  const mainContent: HTMLElement | null = document.querySelector("#main-content");
  if (mainContent) {
    const taskListEl: HTMLElement | null = document.querySelector(".govuk-task-list");
    if (taskListEl instanceof HTMLUListElement) {
      onMessageHandler<ServerFormTemplateData>(fetchFormTemplate, (initialServerPageData) => {
        const formTemplate = initialServerPageData.formTemplate;
        const formTemplateData: FormTemplate = replaceWithEnglishValue(formTemplate) as FormTemplate;

        const taskListSectionNl: NodeListOf<HTMLUListElement> = document.querySelectorAll(".govuk-task-list");
        formTemplateData.sections.forEach((section, taskSectionNumnber) => {
          const taskListSectionEl = taskListSectionNl.item(taskSectionNumnber);
          const taskSection = section as TaskSection;

          if (taskListSectionEl.previousElementSibling) {
            initiateTaskListSection(
              formTemplateId,
              taskListSectionEl.previousElementSibling as HTMLHeadingElement,
              taskSection,
              taskSectionNumnber,
              accessCode,
            );
          }
        });

        const lastSectionNum = taskListSectionNl.length - 1;
        const submitSectionEl = taskListSectionNl[lastSectionNum];
        const submitSectionHeading = submitSectionEl.previousElementSibling as HTMLHeadingElement;
        initiateSubmitSection(formTemplateId, formTemplateData.submitSection, submitSectionHeading, accessCode);

        initiateLandingPage(formTemplateId, mainContent, formTemplate.displayWidth);

        function addNote(element: HTMLElement): void {
          const content: HTMLDivElement | undefined = attachShadowDom(element);
          if (content !== undefined) {
            const NoteControllerComponent = NoteComponentControllerFactory(
              host,
              formTemplateId,
              formTemplate.note || [],
              formTemplate.doneNote || [],
            );

            render(<NoteControllerComponent noteKind={NoteUpdateKind.FormTemplate} noteKindData={null} />, content);
          }
        }

        const noteAttachmentDiv = document.createElement("div");
        noteAttachmentDiv.setAttribute("id", "task-list-note-shadow-root");
        if (mainContent !== null) {
          mainContent.insertAdjacentElement("beforebegin", noteAttachmentDiv);
          addNote(noteAttachmentDiv);
        }
      });
    }
  }
};
