import * as styles from "bundle-text:../section/content.css";
import { useRef, useState, useEffect } from "preact/hooks";
import { RefObject } from "preact";
import { Signal } from "@preact/signals";
import type {
  Coordinates,
  ContentScriptRequest,
  SummarySectionTitleClickable,
  SummarySection,
  SummarySectionRequest,
  UpdateSummarySectionTitleResponse,
} from "../types";
import { MessageKind } from "../types";
import { SmartStringDiv, SmartStringInputDeprecated } from "../section/useSmartString";
import { onMessageHandler } from "../background/index";

const updateTextArea = (signal: Signal<string>, input: RefObject<HTMLTextAreaElement>) => {
  if (input.current !== null) {
    input.current.value = signal.value;
  }
};

export const SummarySectionTitleControllerFactory =
  (
    host: string,
    formTemplateId: string,
    mainContentElement: HTMLDivElement,
    summarySection: SummarySection,
    summarySectionTitleClickable: SummarySectionTitleClickable,
    headerSignal: Signal<string>,
    footerSignal: Signal<string>,
    coordinates: Coordinates | null,
    maybeAccessCode: string | null,
  ) =>
  () => {
    useEffect(() => {
      registerTitleClickHandler(summarySectionTitleClickable.title);
    }, []);

    const serverError = useRef<HTMLDivElement>(null);

    const titleInput = useRef<SmartStringDiv>(null);
    const headerInput = useRef<HTMLTextAreaElement>(null);
    const footerInput = useRef<HTMLTextAreaElement>(null);
    const continueLabelInput = useRef<SmartStringDiv>(null);
    const displayWidthInput = useRef<HTMLSelectElement>(null);
    const keyDisplayWidthInput = useRef<HTMLSelectElement>(null);

    const [windowDisplayed, setWindowDisplayed] = useState(false);
    const [titleValue, setTitleValue] = useState(summarySection.title);
    const [moreOptionsDisplayed, setMoreOptionsDisplayed] = useState(false);
    const [continueLabelValue, setContinueLabelValue] = useState(summarySection.continueLabel);
    const [displayWidthValue, setDisplayWidthValue] = useState(summarySection.displayWidth);
    const [keyDisplayWidthValue, setKeyDisplayWidthValue] = useState(summarySection.keyDisplayWidth);
    const [buttonLabel, setButtonLabel] = useState("");

    updateTextArea(headerSignal, headerInput);
    updateTextArea(footerSignal, footerInput);

    const registerTitleClickHandler = (element: HTMLElement) => {
      element.addEventListener("click", (event) => {
        setWindowDisplayed(true);
        setMoreOptionsDisplayed(false);
        setButtonLabel("Update title");
      });
    };

    const moreOptionsHandler = (e: MouseEvent) => {
      setMoreOptionsDisplayed(true);
      setButtonLabel("Update section");
    };

    const titleKeyUp = (e: KeyboardEvent) => {
      if (titleInput.current !== null) {
        setTitleValue(titleInput.current.value);
      }
      refreshTitle(false);
    };

    const headerKeyUp = (e: KeyboardEvent) => {
      if (headerInput.current !== null) {
        // Change of the signal value will trigger rerendering of the component
        headerSignal.value = headerInput.current.value;
      }
    };

    const footerKeyUp = (e: KeyboardEvent) => {
      if (footerInput.current !== null) {
        // Change of the signal value will trigger rerendering of the component
        footerSignal.value = footerInput.current.value;
      }
    };

    const onContinueLabelChange = (e: Event) => {
      if (continueLabelInput.current !== null) {
        setContinueLabelValue(continueLabelInput.current.value);
      }
    };

    const refreshTitle = (hideContent: boolean) => {
      if (titleInput.current !== null) {
        const titleCurrent: SmartStringDiv = titleInput.current;

        const summarySectionPart: SummarySection = {};

        summarySectionPart["title"] = titleCurrent.value;
        summarySectionPart["header"] = headerSignal.value;
        summarySectionPart["footer"] = footerSignal.value;
        if (continueLabelInput.current !== null) {
          summarySectionPart["continueLabel"] = continueLabelInput.current.value;
        }
        if (displayWidthInput.current !== null) {
          summarySectionPart["displayWidth"] = displayWidthInput.current.value;
        }
        if (keyDisplayWidthInput.current !== null) {
          summarySectionPart["keyDisplayWidth"] = keyDisplayWidthInput.current.value;
        }

        const summarySectionRequest: SummarySectionRequest = {
          payload: summarySectionPart,
          coordinates,
          maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
        };

        const updateRequest: ContentScriptRequest = {
          host: host,
          kind: MessageKind.UpdateSummarySection,
          formTemplateId: formTemplateId,
          data: summarySectionRequest,
        };

        onMessageHandler<UpdateSummarySectionTitleResponse>(updateRequest, (response) => {
          if (response.title !== undefined) {
            refreshComponentHtml(response.title);
            serverError.current?.classList.add("hidden");
            if (hideContent) {
              setWindowDisplayed(false);
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

    const refreshComponentHtml = (title: string) => {
      const h1 = summarySectionTitleClickable.title.querySelector("h1");

      if (h1 !== null) {
        h1.innerHTML = title;
      }
    };

    const updateHandler = (e: MouseEvent) => {
      refreshTitle(true);
    };

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      setWindowDisplayed(false);
    };

    const onDisplayWidthChange = (e: Event) => {
      refreshTitle(false);
      if (displayWidthInput.current !== null) {
        mainContentElement.classList.remove(
          "govuk-grid-column-two-thirds",
          "govuk-grid-column-three-quarters",
          "govuk-grid-column-full",
        );
        const list = mainContentElement.classList;
        switch (displayWidthInput.current.value) {
          case "":
          case "m":
            list.add("govuk-grid-column-two-thirds");
            break;
          case "l":
            list.add("govuk-grid-column-three-quarters");
            break;
          case "xl":
            list.add("govuk-grid-column-full");
            break;
        }
      }
    };

    const onKeyDisplayWidthChange = (e: Event) => {
      refreshTitle(false);
      if (keyDisplayWidthInput.current !== null) {
        const keyElements = document
          .querySelectorAll("#main-content dt.govuk-summary-list__key");

        keyElements.forEach(function (keyEL, currentIndex, listObj) {
          const list = keyEL.classList;
          keyEL.classList.remove(...keyEL.classList);
          list.add("govuk-summary-list__key")
          switch (keyDisplayWidthInput.current.value) {
            case "":
            case "s":
              break;
            case "m":
              list.add("summary-list__key_medium");
              break;
            case "l":
              list.add("summary-list__key_large");
              break;
          }
        });
      }
    }

    const onDisplayWidthBlur = (e: Event) => {
      if (displayWidthInput.current !== null) {
        setDisplayWidthValue(displayWidthInput.current.value);
      }
    };

    const onKeyDisplayWidthBlur = (e: Event) => {
      if (keyDisplayWidthInput.current !== null) {
        setKeyDisplayWidthValue(keyDisplayWidthInput.current.value);
      }
    };

    return (
      <div id="edit-summary-title" class="info" style={{ display: windowDisplayed ? "block" : "none" }}>
        <style>{styles}</style>
        <SmartStringDiv ref={titleInput}>
          <SmartStringInputDeprecated id="edit-title" class="form-control" value={titleValue} onKeyUp={titleKeyUp}>
            Title
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <label for="edit-header">Header</label>
          <textarea id="edit-header" class="form-control" rows={4} ref={headerInput} onKeyUp={headerKeyUp}></textarea>
        </div>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <label for="edit-footer">Footer</label>
          <textarea id="edit-footer" class="form-control" rows={4} ref={footerInput} onKeyUp={footerKeyUp}></textarea>
        </div>
        <SmartStringDiv style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={continueLabelInput}>
          <SmartStringInputDeprecated
            id="edit-continueLabel"
            class="form-control"
            value={continueLabelValue}
            onChange={onContinueLabelChange}
          >
            Continue label
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <label for="edit-displayWidth">Display width</label>
          <select
            id="edit-displayWidth"
            class="form-control"
            value={displayWidthValue}
            ref={displayWidthInput}
            onChange={onDisplayWidthChange}
            onBlur={onDisplayWidthBlur}
          >
            <option value="">Default</option>
            <option value="m">m - Medium</option>
            <option value="l">l - Large</option>
            <option value="xl">xl - Very large</option>
          </select>
        </div>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <label for="edit-keyDisplayWidth">Key display width</label>
          <select
            id="edit-keyDisplayWidth"
            class="form-control"
            value={keyDisplayWidthValue}
            ref={keyDisplayWidthInput}
            onChange={onKeyDisplayWidthChange}
            onBlur={onKeyDisplayWidthBlur}
          >
            <option value="">Default</option>
            <option value="m">m - Medium</option>
            <option value="l">l - Large</option>
          </select>
        </div>
        <button id="update-button" class="btn btn-success" onClick={updateHandler}>
          {buttonLabel}
        </button>
        <button id="cancel-button" class="btn btn-secondary" onClick={cancelClickHandler}>
          Cancel
        </button>
        <button
          style={{ display: moreOptionsDisplayed ? "none" : "inline" }}
          id="options-button"
          class="btn btn-link"
          onClick={moreOptionsHandler}
        >
          Summary page options
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
