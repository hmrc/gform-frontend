import * as styles from "bundle-text:../section/content.css";
import { useRef, useState, useEffect } from "preact/hooks";
import { Ref } from "preact";
import type {
  ContentScriptRequest,
  AcknowledgementTitleClickable,
  AcknowledgementSection,
  AcknowledgementRequest,
  UpdateAcknowledgementResponse,
} from "../types";
import { MessageKind } from "../types";
import { SmartStringDiv, SmartStringInputDeprecated } from "../section/useSmartString";
import { onMessageHandler } from "../background/index";

export const AcknowledgementSectionFactory =
  (
    host: string,
    formTemplateId: string,
    acknowledgementSection: AcknowledgementSection,
    acknowledgementTitleClickable: AcknowledgementTitleClickable,
    maybeAccessCode: string | null,
  ) =>
  () => {
    useEffect(() => {
      registerPanelTitleClickHandler(acknowledgementTitleClickable.title);
    }, []);

    const serverError = useRef<HTMLDivElement>(null);

    const titleInput = useRef<SmartStringDiv>(null);

    const hideReferenceInput = useRef<HTMLInputElement>(null);

    const [windowDisplayed, setWindowDisplayed] = useState(false);
    const [panelTitleValue, setPanelTitleValue] = useState(acknowledgementSection.title);
    const [titleValue, setTitleValue] = useState(acknowledgementSection.title);
    const showReference =
      acknowledgementSection.showReference === undefined ? false : !acknowledgementSection.showReference;
    const [hideReferenceValue, setHideReferenceValue] = useState(showReference);
    const [moreOptionsDisplayed, setMoreOptionsDisplayed] = useState(false);

    const registerPanelTitleClickHandler = (element: HTMLElement) => {
      element.addEventListener("click", (event) => {
        setWindowDisplayed(true);
        setMoreOptionsDisplayed(false);
      });
    };

    const moreOptionsHandler = (e: MouseEvent) => {
      setMoreOptionsDisplayed(true);
    };

    const panelTitleKeyUp = (e: KeyboardEvent) => {
      if (titleInput.current !== null) {
        setTitleValue(titleInput.current.value);
      }
      refreshPanelTitle(false);
    };

    const refreshPanelTitle = (hideContent: boolean) => {
      const acknowledgementComponentPart: AcknowledgementSection = {};
      if (titleInput.current !== null) {
        acknowledgementComponentPart["title"] = titleInput.current.value;
      }

      if (hideReferenceInput.current !== null) {
        acknowledgementComponentPart["showReference"] = !hideReferenceInput.current.checked;
      }
      const acknowledgementComponentRequest: AcknowledgementRequest = {
        payload: acknowledgementComponentPart,
        maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
      };

      const updateRequest: ContentScriptRequest = {
        host: host,
        kind: MessageKind.UpdateAcknowledgement,
        formTemplateId: formTemplateId,
        data: acknowledgementComponentRequest,
      };

      onMessageHandler<UpdateAcknowledgementResponse>(updateRequest, (response) => {
        if (response.panelHtml !== undefined) {
          refreshComponentHtml(response.panelHtml);
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
    };

    const clickHideReferenceToggle = (e: MouseEvent) => {
      setHideReferenceValue(!hideReferenceValue);
    };

    const changeHideReferenceToggle = (e: Event) => {
      refreshPanelTitle(false);
    };

    const refreshComponentHtml = (title: string) => {
      acknowledgementTitleClickable.title.insertAdjacentHTML("afterend", title);
      acknowledgementTitleClickable.title.remove();

      const titleEl: Element | null = document.querySelector(".govuk-panel.govuk-panel--confirmation");

      if (titleEl instanceof HTMLElement) {
        const newAcknowledgementPanelTitleClickable: AcknowledgementTitleClickable = {
          title: titleEl,
        };
        acknowledgementTitleClickable = newAcknowledgementPanelTitleClickable;
        registerPanelTitleClickHandler(acknowledgementTitleClickable.title);
      }
    };

    const updateHandler = (e: MouseEvent) => {
      refreshPanelTitle(true);
    };

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      setWindowDisplayed(false);
    };

    return (
      <div id="edit-acknowledgement-panel-title" class="info" style={{ display: windowDisplayed ? "block" : "none" }}>
        <style>{styles}</style>
        <SmartStringDiv ref={titleInput}>
          <SmartStringInputDeprecated id="edit-title" class="form-control" value={titleValue} onKeyUp={panelTitleKeyUp}>
            Title
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <input
            type="checkbox"
            id="hideReference"
            ref={hideReferenceInput}
            checked={hideReferenceValue}
            onClick={clickHideReferenceToggle}
            onChange={changeHideReferenceToggle}
          />
          <label for="hideReference">Hide submission reference</label>
        </div>

        <button id="update-button" class="btn btn-success" onClick={updateHandler}>
          Update title
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
          Acknowledgement page options
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
