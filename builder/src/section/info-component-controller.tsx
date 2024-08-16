import * as styles from "bundle-text:./content.css";
import { Signal } from "@preact/signals";
import { useRef, useState } from "preact/hooks";
import type {
  ContentScriptRequest,
  FormComponent,
  FormComponentPart,
  FormComponentUpdateRequest,
  SectionNumber,
  UpdateHtmlResponse,
} from "../types";
import { MessageKind } from "../types";
import { ErrorReportLink } from "./error-report-component";
import { SmartStringDiv, SmartStringInputDeprecated, SmartStringTextArea } from "./useSmartString";
import { onMessageHandler } from "../background/index";

interface InfoProps {
  kind: MessageKind;
  requestData?: any;
}

export const InfoComponentControllerFactory =
  (
    formComponent: FormComponent,
    host: string,
    formTemplateId: string,
    sectionNumber: SectionNumber,
    atlIterationIndex: number | undefined,
    infoEl: HTMLDivElement | HTMLDetailsElement,
    abortController: AbortController,
    maybeAccessCode: string | null,
    sectionNumberChange?: Signal<SectionNumber | undefined>,
  ) =>
  ({ kind, requestData }: InfoProps) => {
    if (sectionNumberChange !== undefined && sectionNumberChange.value !== undefined) {
      sectionNumber = sectionNumberChange.value;
    }

    abortController.abort();
    abortController = new AbortController();
    const { signal } = abortController;
    const content = useRef<HTMLDivElement>(null);
    const serverError = useRef<HTMLDivElement>(null);
    const moreOptions = useRef<HTMLButtonElement>(null);

    const labelInput = useRef<SmartStringDiv>(null);
    const infoTextInput = useRef<SmartStringDiv>(null);
    const infoTypeInput = useRef<HTMLSelectElement>(null);

    const labelContainer = useRef<HTMLDivElement>(null);
    const infoTextContainer = useRef<HTMLDivElement>(null);
    const infoTypeContainer = useRef<HTMLDivElement>(null);

    const allContainers = [infoTextContainer, infoTypeContainer];

    const hideAll = () => {
      for (let container of allContainers) {
        container.current?.classList.add("hidden");
      }
    };

    const showAll = () => {
      for (let container of allContainers) {
        container.current?.classList.remove("hidden");
      }
    };

    const showMoreOptionsButton = () => {
      moreOptions.current?.classList.remove("hidden");
    };

    const [labelValue, setLabelValue] = useState(formComponent.label);
    const [labelDep, setLabelDep] = useState(
      formComponent.infoType === "banner" || formComponent.infoType === "long" ? "" : "hidden",
    );

    const infoType = formComponent.infoType || "standard";

    const [infoTextValue, setInfoTextValue] = useState(formComponent.infoText);
    const [infoTypeValue, setInfoTypeValue] = useState(infoType);

    const registerInfoClickHandler = (element: HTMLDivElement | HTMLDetailsElement) => {
      element.addEventListener(
        "click",
        (event) => {
          hideAll();
          showMoreOptionsButton();
          infoTextContainer.current?.classList.remove("hidden");
          content.current?.classList.remove("hidden");
        },
        { signal },
      );
    };

    registerInfoClickHandler(infoEl);

    const infoTextKeyUp = (e: KeyboardEvent) => {
      refreshInfoText(false);
    };

    const updateHandler = (e: MouseEvent) => {
      refreshInfoText(true);
    };

    const refreshInfoText = (hideContent: boolean) => {
      if (content.current !== null) {
        const labelCurrent: SmartStringDiv | null = labelInput.current;
        const infoTextCurrent: SmartStringDiv | null = infoTextInput.current;
        const infoTypeCurrent: HTMLSelectElement | null = infoTypeInput.current;
        const id = formComponent.id;

        const formComponentPart: FormComponentPart = {
          id,
        };

        if (labelCurrent !== null && labelCurrent.checkVisibility()) {
          formComponentPart["label"] = labelCurrent.value;
        }

        if (infoTextCurrent !== null && infoTextCurrent.checkVisibility()) {
          formComponentPart["infoText"] = infoTextCurrent.value;
        }

        if (infoTypeCurrent !== null && infoTypeCurrent.checkVisibility()) {
          formComponentPart["infoType"] = infoTypeCurrent.value;
        }

        const data: FormComponentUpdateRequest = {
          payload: formComponentPart,
          sectionNumber: sectionNumber,
          atlIterationIndex: atlIterationIndex,
          requestData: requestData,
          maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
        };

        const updateRequest: ContentScriptRequest = {
          host,
          kind,
          formTemplateId,
          data,
        };

        onMessageHandler<UpdateHtmlResponse>(updateRequest, (response) => {
          if (response.html !== undefined) {
            infoEl.insertAdjacentHTML("beforebegin", response.html);

            const newInfoEl = infoEl.previousElementSibling;

            if (
              newInfoEl !== null &&
              (newInfoEl instanceof HTMLDivElement || newInfoEl instanceof HTMLDetailsElement)
            ) {
              if (newInfoEl instanceof HTMLDetailsElement) {
                newInfoEl.open = true;
              }
              infoEl.remove();
              registerInfoClickHandler(newInfoEl);
              infoEl = newInfoEl;
              serverError.current?.classList.add("hidden");
              if (hideContent) {
                content.current?.classList.add("hidden");
              }
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
      }
    };

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      showMoreOptionsButton();
      content.current?.classList.add("hidden");
    };

    const moreOptionsHandler = (e: MouseEvent) => {
      moreOptions.current?.classList.add("hidden");
      showAll();
    };

    const onLabelChange = (e: Event) => {
      if (labelInput.current !== null) {
        setLabelValue(labelInput.current.value);
      }
    };

    const onInfoTypeChange = (e: Event) => {
      if (infoTypeInput.current !== null) {
        setInfoTypeValue(infoTypeInput.current.value);
        refreshInfoText(false);

        if (infoTypeInput.current.value === "long" || infoTypeInput.current.value === "banner") {
          setLabelDep("");
        } else {
          setLabelDep("hidden");
        }
      }
    };

    return (
      <div id="edit-info-component" class="info hidden" ref={content}>
        <style>{styles}</style>
        <SmartStringDiv divRef={infoTextContainer} ref={infoTextInput}>
          <SmartStringTextArea
            id="edit-infoText"
            class="form-control"
            rows={4}
            onKeyUp={infoTextKeyUp}
            value={infoTextValue}
            typeId="SmartStringTextArea"
          >
            Info text
          </SmartStringTextArea>
        </SmartStringDiv>
        <SmartStringDiv clazz={labelDep} divRef={labelContainer} ref={labelInput}>
          <SmartStringInputDeprecated
            id="edit-label"
            class="form-control"
            value={labelValue}
            onChange={onLabelChange}
            onKeyUp={infoTextKeyUp}
          >
            Label
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <div class="hidden" ref={infoTypeContainer}>
          <label for="edit-infoType">InfoType</label>
          <select
            id="edit-infoType"
            class="form-control"
            value={infoTypeValue}
            ref={infoTypeInput}
            onChange={onInfoTypeChange}
          >
            <option value="noformat">Text</option>
            <option value="standard">Inset text</option>
            <option value="long">Details</option>
            <option value="important">Warning text</option>
            <option value="banner">Banner</option>
          </select>
        </div>
        <button class="btn btn-success" onClick={updateHandler}>
          Update component
        </button>
        <button class="btn btn-secondary" onClick={cancelClickHandler}>
          Cancel
        </button>
        <button class="btn btn-link" onClick={moreOptionsHandler} ref={moreOptions}>
          Field options
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
