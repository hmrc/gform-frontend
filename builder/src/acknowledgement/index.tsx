import { render } from "preact";
import type {
  ContentScriptRequest,
  FormTemplateId,
  AcknowledgementPanelTitleClickable,
  AcknowledgementSection,
  InfoRenderParam,
} from "../types";
import { MessageKind, NoteUpdateKind, SectionNumber } from "../types";
import { NoteComponentControllerFactory } from "../section/note-component/note-component-controller";
import { initiateInfoComponents } from "../section/info-component-helper";
import { replaceWithEnglishValue, attachShadowDom } from "../pure-functions";
import { AcknowledgementSectionFactory } from "./acknowledgement-section-controller";

import { onMessageHandler } from "../background/index";

export const activateAcknowledgementBuilder = (urlMatch: RegExpMatchArray, url: string) => {
  const host = urlMatch![1];
  const formTemplateId: FormTemplateId = urlMatch![2];
  const queryParams = urlMatch![3];
  const urlParams = new URLSearchParams("?" + queryParams);

  const maybeAccessCode: string | null = urlParams.get("a");

  const contentScriptRequest: ContentScriptRequest = {
    host,
    formTemplateId,
    kind: MessageKind.AcknowledgementSection,
    data: maybeAccessCode,
  };

  const initiateAcknowledgementSection = (
    acknowledgementSection: AcknowledgementSection,
    acknowledgementPanelTitleClickable: AcknowledgementPanelTitleClickable,
  ) => {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "acknowledgement-section-shadow-root");
    acknowledgementPanelTitleClickable.panelTitle.insertAdjacentElement("afterend", attachmentDiv);
    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

    if (content !== undefined) {
      const AcknowledgementSectionController = AcknowledgementSectionFactory(
        host,
        formTemplateId,
        acknowledgementSection,
        acknowledgementPanelTitleClickable,
        maybeAccessCode,
      );
      render(<AcknowledgementSectionController />, content);
    }
  };

  const initiateNote = (acknowledgeComponent: AcknowledgementSection): void => {
    const mainContent = document.getElementById("main-content");
    if (mainContent !== null) {
      const noteAttachmentDiv = document.createElement("div");
      noteAttachmentDiv.setAttribute("id", "acknowledgement-section-note-shadow-root");
      mainContent.insertAdjacentElement("beforebegin", noteAttachmentDiv);

      const content: HTMLDivElement | undefined = attachShadowDom(noteAttachmentDiv);
      if (content !== undefined) {
        const NoteControllerComponent = NoteComponentControllerFactory(
          host,
          formTemplateId,
          acknowledgeComponent.note || [],
          acknowledgeComponent.doneNote || [],
        );

        render(
          <NoteControllerComponent noteKind={NoteUpdateKind.AcknowledgementSection} noteKindData={null} />,
          content,
        );
      }
    }
  };

  onMessageHandler<AcknowledgementSection>(contentScriptRequest, (initialAcknowledgementSectionData) => {
    const acknowledgementSection: AcknowledgementSection = replaceWithEnglishValue(
      initialAcknowledgementSectionData,
    ) as AcknowledgementSection;

    initiateNote(acknowledgementSection);

    const panelTitle: Element | null = document.querySelector(".govuk-panel.govuk-panel--confirmation");

    if (panelTitle instanceof HTMLElement) {
      const acknowledgementPanelTitleClickable: AcknowledgementPanelTitleClickable = {
        panelTitle: panelTitle,
      };

      if (panelTitle instanceof HTMLDivElement) {
        initiateAcknowledgementSection(acknowledgementSection, acknowledgementPanelTitleClickable);

        const sectionNumber: SectionNumber = SectionNumber.AcknowledgementSectionNumber();
        if (acknowledgementSection.fields !== undefined) {
          const params: InfoRenderParam = {
            host,
            formTemplateId,
            sectionNumber,
            kind: MessageKind.UpdateAcknowledgementFormComponent,
          };
          initiateInfoComponents(acknowledgementSection.fields, params, maybeAccessCode);
        }
      }
    }
  });
};
