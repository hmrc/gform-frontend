import { Signal, signal } from "@preact/signals";
import { render } from "preact";

import type {
  InfoLookupHelper,
  Section,
  FormComponent,
  FormComponentId,
  ServerPageData,
  AddressClickable,
  DateClickable,
  TextClickable,
  FileUploadClickable,
  InfoRenderParam,
} from "../types";
import { MessageKind, SectionNumber, NoteUpdateKind, SmartString, FieldInteractionType } from "../types";
import { DateComponentControllerFactory } from "./date-component-controller";
import { TextComponentControllerFactory } from "./text-component-controller";
import { ChoiceComponentControllerFactory } from "./choice-component-controller";
import { AddressComponentControllerFactory } from "./address-component-controller";
import { FileUploadComponentControllerFactory } from "./fileupload-component-controller";
import { NoteComponentControllerFactory } from "./note-component/note-component-controller";
import { mkAddressClickable } from "./address-helper";
import { mkChoiceClickable } from "./choice-helper";
import { mkDateClickable } from "./date-helper";
import { mkTextClickable } from "./text-helper";
import { mkFileUploadClickable } from "./fileupload-helper";
import { fullFormComponentId, attachShadowDom, findApplicableFields, findInfoInteractionType } from "./pure-functions";
import { findInfoElement, infoComponentLookup } from "./info-component-helper";
import { MainPanelFactory } from "./main-panel";
import { SectionControllerFactory } from "./section-panel";
import { ChoicePanelFactory } from "./choice-panel";
import { TextPanelFactory } from "./text-panel";
import { DatePanelFactory } from "./date-panel";
import { AddressPanelFactory } from "./address-panel";
import { FilePanelFactory } from "./file-panel";
import { InfoPanelFactory } from "./info-panel";
import { InfoControllerFactory } from "./info-controller";

const initiateTextComponent = (
  inputFormComponentId: string,
  form: HTMLFormElement,
  textClickable: TextClickable,
  formComponent: FormComponent,
  interactionType: FieldInteractionType,
  serverPageData: ServerPageData,
  currentSection: Section,
  host: string,
  formTemplateId: string,
  sectionNumber: SectionNumber,
  maybeAccessCode: string | null,
) => {
  const attachmentDiv = document.createElement("div");
  attachmentDiv.setAttribute("id", `${inputFormComponentId}-shadow-root`);
  textClickable.parentEl.insertAdjacentElement("afterend", attachmentDiv);
  const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);
  const pageHeadingSignal: Signal<boolean> = signal(formComponent.label === undefined);
  const titleSignal: Signal<SmartString> = signal(currentSection.title || "");
  if (content !== undefined) {
    const SectionPanel = SectionControllerFactory(
      inputFormComponentId,
      host,
      formTemplateId,
      sectionNumber,
      serverPageData,
      maybeAccessCode,
    );

    const TextPanel = TextPanelFactory(
      host,
      formTemplateId,
      formComponent,
      serverPageData,
      sectionNumber,
      maybeAccessCode,
    );

    const MainPanel = MainPanelFactory(inputFormComponentId, formComponent, interactionType, host, formTemplateId);

    const stateParam = {
      section: currentSection,
      formComponent: formComponent,
    };

    const TextComponentController = TextComponentControllerFactory(
      inputFormComponentId,
      currentSection,
      formComponent,
      stateParam,
      interactionType,
      host,
      formTemplateId,
      form,
      titleSignal,
      MainPanel,
      SectionPanel,
      TextPanel,
    );

    render(<TextComponentController />, content);
  }
};

const initiateFileUploadComponent = (
  inputFormComponentId: string,
  form: HTMLFormElement,
  fileUploadClickable: FileUploadClickable,
  formComponent: FormComponent,
  interactionType: FieldInteractionType,
  serverPageData: ServerPageData,
  currentSection: Section,
  host: string,
  formTemplateId: string,
  sectionNumber: SectionNumber,
  maybeAccessCode: string | null,
) => {
  const attachmentDiv = document.createElement("div");
  fileUploadClickable.parentEl.insertAdjacentElement("afterend", attachmentDiv);
  const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);
  const titleSignal: Signal<SmartString> = signal(currentSection.title || "");
  if (content !== undefined) {
    const SectionPanel = SectionControllerFactory(
      inputFormComponentId,
      host,
      formTemplateId,
      sectionNumber,
      serverPageData,
      maybeAccessCode,
    );

    const FilePanel = FilePanelFactory(
      host,
      formTemplateId,
      formComponent,
      serverPageData,
      sectionNumber,
      maybeAccessCode,
    );

    const MainPanel = MainPanelFactory(inputFormComponentId, formComponent, interactionType, host, formTemplateId);

    const stateParam = {
      section: currentSection,
      formComponent: formComponent,
    };

    const FileUploadComponentController = FileUploadComponentControllerFactory(
      inputFormComponentId,
      formComponent,
      stateParam,
      interactionType,
      form,
      fileUploadClickable,
      titleSignal,
      MainPanel,
      SectionPanel,
      FilePanel,
    );
    render(<FileUploadComponentController />, content);
  }
};

const initiateAddressComponent = (
  inputFormComponentId: string,
  addressClickable: AddressClickable,
  formComponent: FormComponent,
  interactionType: FieldInteractionType,
  form: HTMLFormElement,
  currentSection: Section,
  serverPageData: ServerPageData,
  host: string,
  formTemplateId: string,
  sectionNumber: SectionNumber,
  maybeAccessCode: string | null,
) => {
  const attachmentDiv = document.createElement("div");
  addressClickable.parentEl.insertAdjacentElement("afterend", attachmentDiv);
  const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);
  const titleSignal: Signal<SmartString> = signal(currentSection.title || "");
  if (content !== undefined) {
    const SectionPanel = SectionControllerFactory(
      inputFormComponentId,
      host,
      formTemplateId,
      sectionNumber,
      serverPageData,
      maybeAccessCode,
    );

    const AddressPanel = AddressPanelFactory(
      host,
      formTemplateId,
      formComponent,
      serverPageData,
      sectionNumber,
      maybeAccessCode,
    );

    const MainPanel = MainPanelFactory(inputFormComponentId, formComponent, interactionType, host, formTemplateId);

    const stateParam = {
      section: currentSection,
      formComponent: formComponent,
    };

    const AddressComponentController = AddressComponentControllerFactory(
      inputFormComponentId,
      formComponent,
      stateParam,
      interactionType,
      form,
      addressClickable,
      titleSignal,
      MainPanel,
      SectionPanel,
      AddressPanel,
    );
    render(<AddressComponentController />, content);
  }
};

const initiateDateComponent = (
  inputFormComponentId: string,
  dateClickable: DateClickable,
  formComponent: FormComponent,
  interactionType: FieldInteractionType,
  serverPageData: ServerPageData,
  currentSection: Section,
  form: HTMLFormElement,
  host: string,
  formTemplateId: string,
  sectionNumber: SectionNumber,
  maybeAccessCode: string | null,
) => {
  const attachmentDiv = document.createElement("div");
  dateClickable.parentEl.insertAdjacentElement("afterend", attachmentDiv);
  const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);
  const titleSignal: Signal<SmartString> = signal(currentSection.title || "");
  if (content !== undefined) {
    const SectionPanel = SectionControllerFactory(
      inputFormComponentId,
      host,
      formTemplateId,
      sectionNumber,
      serverPageData,
      maybeAccessCode,
    );

    const DatePanel = DatePanelFactory(
      host,
      formTemplateId,
      formComponent,
      serverPageData,
      sectionNumber,
      maybeAccessCode,
    );

    const MainPanel = MainPanelFactory(inputFormComponentId, formComponent, interactionType, host, formTemplateId);

    const stateParam = {
      section: currentSection,
      formComponent: formComponent,
    };

    const DateComponentController = DateComponentControllerFactory(
      inputFormComponentId,
      formComponent,
      stateParam,
      interactionType,
      form,
      titleSignal,
      MainPanel,
      SectionPanel,
      DatePanel,
    );
    render(<DateComponentController />, content);
  }
};

const initiateChoiceComponent = (
  inputFormComponentId: string,
  formComponent: FormComponent,
  interactionType: FieldInteractionType,
  serverPageData: ServerPageData,
  currentSection: Section,
  form: HTMLFormElement,
  host: string,
  formTemplateId: string,
  sectionNumber: SectionNumber,
  maybeAccessCode: string | null,
) => {
  const attachmentDiv = document.createElement("div");
  attachmentDiv.setAttribute("id", `${inputFormComponentId}-shadow-root`);

  const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);
  const choiceClickable = mkChoiceClickable(inputFormComponentId);

  if (choiceClickable !== undefined && content !== undefined) {
    choiceClickable.parentEl.insertAdjacentElement("afterend", attachmentDiv);
    const titleSignal: Signal<SmartString> = signal(currentSection.title || "");

    const SectionPanel = SectionControllerFactory(
      inputFormComponentId,
      host,
      formTemplateId,
      sectionNumber,
      serverPageData,
      maybeAccessCode,
    );

    const ChoicePanel = ChoicePanelFactory(host, formTemplateId, serverPageData, sectionNumber, maybeAccessCode);

    const MainPanel = MainPanelFactory(inputFormComponentId, formComponent, interactionType, host, formTemplateId);

    const stateParam = {
      section: currentSection,
      formComponent: formComponent,
    };

    const ChoiceComponentController = ChoiceComponentControllerFactory(
      inputFormComponentId,
      formComponent,
      stateParam,
      interactionType,
      host,
      formTemplateId,
      sectionNumber,
      form,
      titleSignal,
      MainPanel,
      SectionPanel,
      ChoicePanel,
    );
    render(<ChoiceComponentController />, content);
  }
};

const initiateInfoComponent = (
  infoLookups: InfoLookupHelper,
  element: HTMLDivElement | HTMLDetailsElement,
  inputFormComponentId: string,
  formComponent: FormComponent,
  interactionType: FieldInteractionType,
  serverPageData: ServerPageData,
  currentSection: Section,
  form: HTMLFormElement,
  host: string,
  formTemplateId: string,
  sectionNumber: SectionNumber,
  maybeAccessCode: string | null,
) => {
  const attachmentDiv = document.createElement("div");
  attachmentDiv.setAttribute("id", `${inputFormComponentId}-shadow-root`);
  element.insertAdjacentElement("afterend", attachmentDiv);

  const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

  if (content !== undefined) {
    const titleSignal: Signal<SmartString> = signal(currentSection.title || "");
    const SectionPanel = SectionControllerFactory(
      inputFormComponentId,
      host,
      formTemplateId,
      sectionNumber,
      serverPageData,
      maybeAccessCode,
    );

    const InfoPanel = InfoPanelFactory(
      host,
      formTemplateId,
      formComponent,
      serverPageData,
      sectionNumber,
      maybeAccessCode,
    );

    const MainPanel = MainPanelFactory(inputFormComponentId, formComponent, interactionType, host, formTemplateId);

    const stateParam = {
      section: currentSection,
      formComponent: formComponent,
    };

    const InfoController = InfoControllerFactory(
      infoLookups,
      inputFormComponentId,
      formComponent,
      stateParam,
      interactionType,
      host,
      formTemplateId,
      form,
      titleSignal,
      MainPanel,
      SectionPanel,
      InfoPanel,
    );
    render(<InfoController />, content);
  }
};

export const sectionBootstrap = (
  serverPageData0: ServerPageData,
  formParentEl: HTMLDivElement,
  host: string,
  formTemplateId: string,
  sectionNumber: SectionNumber,
  form: HTMLFormElement,
  maybeAccessCode: string | null,
) => {
  const serverPageData: ServerPageData = {
    ...serverPageData0,
    section: {
      ...serverPageData0.section,
      fields: serverPageData0.section.fields.filter((field) => !serverPageData0.hiddenComponentIds?.includes(field.id)),
    },
  };

  const currentSection = serverPageData.section;

  const infoLookups = infoComponentLookup(currentSection.fields);

  const applicables = findApplicableFields(currentSection.fields);

  const fields = currentSection.fields.filter((formComponent) => formComponent.submitMode !== "summaryinfoonly");
  const sectionTitleEqFieldLabel =
    fields[0].label === undefined && !["info", "table", "miniSummaryList", "postcodeLookup"].includes(fields[0].type);

  fields.forEach((formComponent, index) => {
    const id: FormComponentId = formComponent.id;
    const inputFormComponentId = fullFormComponentId(id, serverPageData.atlIterationIndex);

    const interactionType =
      applicables.length === 1 && index === 0
        ? FieldInteractionType.TitleLabelWithSync
        : applicables.length > 1 && index === 0
          ? FieldInteractionType.TitleLabelNoSync
          : FieldInteractionType.ComponentOnly;

    switch (formComponent.type) {
      case undefined:
      case "text":
        {
          const textClickables: TextClickable | undefined = mkTextClickable(
            inputFormComponentId,
            sectionTitleEqFieldLabel,
            form,
            formComponent.format,
          );

          if (textClickables !== undefined) {
            initiateTextComponent(
              inputFormComponentId,
              form,
              textClickables,
              formComponent,
              interactionType,
              serverPageData,
              currentSection,
              host,
              formTemplateId,
              sectionNumber,
              maybeAccessCode,
            );
          }
        }
        break;
      case "file":
        {
          const fileUploadClickables: FileUploadClickable | undefined = mkFileUploadClickable(
            inputFormComponentId,
            sectionTitleEqFieldLabel,
            form,
          );

          if (fileUploadClickables !== undefined) {
            initiateFileUploadComponent(
              inputFormComponentId,
              form,
              fileUploadClickables,
              formComponent,
              interactionType,
              serverPageData,
              currentSection,
              host,
              formTemplateId,
              sectionNumber,
              maybeAccessCode,
            );
          }
        }
        break;
      case "overseasAddress":
      case "address":
        {
          const addressClickable = mkAddressClickable(inputFormComponentId);

          if (addressClickable !== undefined) {
            initiateAddressComponent(
              inputFormComponentId,
              addressClickable,
              formComponent,
              interactionType,
              form,
              currentSection,
              serverPageData,
              host,
              formTemplateId,
              sectionNumber,
              maybeAccessCode,
            );
          }
        }
        break;
      case "date":
        {
          const dateClickable = mkDateClickable(inputFormComponentId);

          if (dateClickable !== undefined) {
            initiateDateComponent(
              inputFormComponentId,
              dateClickable,
              formComponent,
              interactionType,
              serverPageData,
              currentSection,
              form,
              host,
              formTemplateId,
              sectionNumber,
              maybeAccessCode,
            );
          }
        }
        break;
      case "choice":
      case "revealingChoice":
        {
          initiateChoiceComponent(
            inputFormComponentId,
            formComponent,
            interactionType,
            serverPageData,
            currentSection,
            form,
            host,
            formTemplateId,
            sectionNumber,
            maybeAccessCode,
          );
        }
        break;
      case "info":
        {
          const infoSelectorAndIndex = findInfoElement(inputFormComponentId, formComponent.infoType, infoLookups);

          if (infoSelectorAndIndex !== undefined) {
            const infoElements: NodeListOf<Element> = document.querySelectorAll(infoSelectorAndIndex.selector);
            const infoComponent = infoElements.item(infoSelectorAndIndex.index);

            const interactionType =
              fields[0].type === "info" && fields[0].id === formComponent.id
                ? FieldInteractionType.TitleLabelNoSync
                : FieldInteractionType.ComponentOnly;

            if (infoComponent instanceof HTMLDivElement || infoComponent instanceof HTMLDetailsElement) {
              initiateInfoComponent(
                infoLookups,
                infoComponent,
                inputFormComponentId,
                formComponent,
                interactionType,
                serverPageData,
                currentSection,
                form,
                host,
                formTemplateId,
                sectionNumber,
                maybeAccessCode,
              );
            }
          }
        }
        break;
    }
  });

  function addNote(element: HTMLElement): void {
    const content: HTMLDivElement | undefined = attachShadowDom(element);
    if (content !== undefined) {
      const NoteControllerComponent = NoteComponentControllerFactory(
        host,
        formTemplateId,
        currentSection.note || [],
        currentSection.doneNote || [],
      );

      render(<NoteControllerComponent noteKind={NoteUpdateKind.Section} noteKindData={serverPageData} />, content);
    }
  }
  const noteAttachmentDiv = document.createElement("div");
  noteAttachmentDiv.setAttribute("id", "section-note-shadow-root");
  const mainContent = document.querySelector("#main-content");
  if (mainContent !== null) {
    mainContent.insertAdjacentElement("beforebegin", noteAttachmentDiv);
    addNote(noteAttachmentDiv);
  }
};
