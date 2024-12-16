import { render } from "preact";
import { signal, Signal } from "@preact/signals";
import type {
  Coordinates,
  ContentScriptRequest,
  FormTemplateId,
  FormComponent,
  FormComponentId,
  SummarySectionFooterClickable,
  SummarySectionHeaderClickable,
  SummarySectionTitleClickable,
  SummarySectionOriginalRequest,
  SummarySection,
  InfoRenderParam,
  TaskSectionNumber,
} from "../types";
import { MessageKind, NoteUpdateKind, SectionNumber } from "../types";
import { InfoComponentControllerFactory } from "../section/info-component-controller";
import { NoteComponentControllerFactory } from "../section/note-component/note-component-controller";
import { replaceWithEnglishValue, mkLookup, attachShadowDom } from "../pure-functions";
import { SummarySectionFooterControllerFactory } from "./footer-controller";
import { SummarySectionHeaderControllerFactory } from "./header-controller";
import { SummarySectionTitleControllerFactory } from "./title-controller";
import { initiateInfoComponents } from "../section/info-component-helper";
import { onMessageHandler } from "../background/index";

export const activateSummaryBuilder = (urlMatch: RegExpMatchArray, url: string) => {
  const host = urlMatch![1];
  const formTemplateId: FormTemplateId = urlMatch![2];

  const queryParams = urlMatch![4];
  const urlParams = new URLSearchParams("?" + queryParams);
  const coordinatesString: string | null = urlParams.get("c");
  const maybeAccessCode: string | null = urlParams.get("a");

  const toCoordinates = (coordinatesString: string): Coordinates => {
    const coordinatesMatch = coordinatesString.match(/(\d+),(\d+)/);
    const taskSectionNumber: number = parseInt(coordinatesMatch![1]);
    const taskNumber: number = parseInt(coordinatesMatch![2]);
    return {
      taskSectionNumber,
      taskNumber,
    };
  };

  const coordinates: Coordinates | null = coordinatesString !== null ? toCoordinates(coordinatesString) : null;

  const summarySectionOriginalRequest: SummarySectionOriginalRequest = {
    coordinates: coordinates,
    maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
  };

  const contentScriptRequest: ContentScriptRequest = {
    host,
    formTemplateId,
    kind: MessageKind.SummarySection,
    data: summarySectionOriginalRequest,
  };

  const initiateSummarySectionTitle = (
    mainContentElement: HTMLDivElement,
    summarySection: SummarySection,
    summarySectionTitleClickable: SummarySectionTitleClickable,
    headerSignal: Signal<string>,
    footerSignal: Signal<string>,
    coordinates: Coordinates | null,
    maybeAccessCode: string | null,
  ) => {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "summary-section-title-shadow-root");
    summarySectionTitleClickable.title.insertAdjacentElement("afterend", attachmentDiv);
    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

    if (content !== undefined) {
      const SummarySectionTitleController = SummarySectionTitleControllerFactory(
        host,
        formTemplateId,
        mainContentElement,
        summarySection,
        summarySectionTitleClickable,
        headerSignal,
        footerSignal,
        coordinates,
        maybeAccessCode,
      );
      render(<SummarySectionTitleController />, content);
    }
  };

  const initiateSummarySectionHeader = (
    summarySectionHeaderClickable: SummarySectionHeaderClickable,
    headerSignal: Signal<string>,
    coordinates: Coordinates | null,
    maybeAccessCode: string | null,
  ) => {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "summary-section-header-shadow-root");
    summarySectionHeaderClickable.header.insertAdjacentElement("afterend", attachmentDiv);
    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

    if (content !== undefined) {
      const SummarySectionHeaderController = SummarySectionHeaderControllerFactory(
        host,
        formTemplateId,
        summarySectionHeaderClickable,
        headerSignal,
        coordinates,
        maybeAccessCode,
      );
      render(<SummarySectionHeaderController />, content);
    }
  };

  const initiateSummarySectionFooter = (
    summarySectionFooterClickable: SummarySectionFooterClickable,
    footerSignal: Signal<string>,
    coordinates: Coordinates | null,
    maybeAccessCode: string | null,
  ) => {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "summary-section-footer-shadow-root");
    summarySectionFooterClickable.footer.insertAdjacentElement("afterend", attachmentDiv);
    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

    if (content !== undefined) {
      const SummarySectionFooterController = SummarySectionFooterControllerFactory(
        host,
        formTemplateId,
        summarySectionFooterClickable,
        footerSignal,
        coordinates,
        maybeAccessCode,
      );
      render(<SummarySectionFooterController />, content);
    }
  };

  const initiateNote = (summarySection: SummarySection, coordinates: Coordinates | null): void => {
    const mainContent = document.getElementById("main-content");
    if (mainContent !== null) {
      const noteAttachmentDiv = document.createElement("div");
      noteAttachmentDiv.setAttribute("id", "summary-section-note-shadow-root");
      mainContent.insertAdjacentElement("beforebegin", noteAttachmentDiv);

      const content: HTMLDivElement | undefined = attachShadowDom(noteAttachmentDiv);
      if (content !== undefined) {
        const NoteControllerComponent = NoteComponentControllerFactory(
          host,
          formTemplateId,
          summarySection.note || [],
          summarySection.doneNote || [],
        );

        render(
          <NoteControllerComponent noteKind={NoteUpdateKind.SummarySection} noteKindData={coordinates} />,
          content,
        );
      }
    }
  };

  onMessageHandler<SummarySection>(contentScriptRequest, (initialSummarySectionData) => {
    const summarySection: SummarySection = replaceWithEnglishValue(initialSummarySectionData) as SummarySection;

    initiateNote(summarySection, coordinates);

    const summaryTitle: Element | null = document.querySelector(".hmrc-page-heading");

    const headerSignal: Signal<string> = signal(summarySection.header || "");
    const footerSignal: Signal<string> = signal(summarySection.footer || "");

    if (summaryTitle instanceof HTMLElement) {
      const formElement = document.getElementById("gf-form");
      if (summarySection.header !== undefined) {
        if (summarySection.header === "") {
        } else {
          const header: Element | null = summaryTitle.nextElementSibling;
          if (header instanceof HTMLDivElement) {
            const summarySectionHeaderClickable: SummarySectionHeaderClickable = {
              header,
            };
            initiateSummarySectionHeader(summarySectionHeaderClickable, headerSignal, coordinates, maybeAccessCode);
          }
        }
      }
      if (summarySection.footer !== undefined) {
        const footer: Element | null | undefined =
          formElement?.lastElementChild?.lastElementChild?.lastElementChild?.previousElementSibling;

        if (footer instanceof HTMLDivElement) {
          const summarySectionFooterClickable: SummarySectionFooterClickable = {
            footer,
          };
          initiateSummarySectionFooter(summarySectionFooterClickable, footerSignal, coordinates, maybeAccessCode);
        }
      }

      const summarySectionTitleClickable: SummarySectionTitleClickable = {
        title: summaryTitle,
      };

      const mainContentElement: Element | null | undefined =
        formElement?.firstElementChild?.nextElementSibling?.firstElementChild;

      if (mainContentElement instanceof HTMLDivElement) {
        initiateSummarySectionTitle(
          mainContentElement,
          summarySection,
          summarySectionTitleClickable,
          headerSignal,
          footerSignal,
          coordinates,
          maybeAccessCode,
        );

        if (summarySection.fields !== undefined) {
          const sectionNumber =
            coordinates === null
              ? SectionNumber.NormalPage(0)
              : SectionNumber.TaskListSectionNumber(
                  coordinates.taskSectionNumber,
                  coordinates.taskNumber,
                  SectionNumber.NormalPage(0),
                );
          const params: InfoRenderParam = {
            host,
            formTemplateId,
            sectionNumber: sectionNumber,
            kind: MessageKind.UpdateSummarySectionFormComponent,
          };

          initiateInfoComponents(summarySection.fields, params, maybeAccessCode);
        }
      }
    }
  });
};
