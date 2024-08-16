import * as styles from "bundle-text:../section/content.css";
import { useRef, useState, useEffect } from "preact/hooks";
import { RefObject } from "preact";
import { Signal } from "@preact/signals";
import type {
  Coordinates,
  ContentScriptRequest,
  SummarySectionHeaderClickable,
  SummarySection,
  SummarySectionRequest,
  UpdateSummarySectionTitleResponse,
} from "../types";
import { MessageKind } from "../types";
import { onMessageHandler } from "../background/index";

const updateHeaderValue = (headerSignal: Signal<string>, headerInput: RefObject<HTMLTextAreaElement>) => {
  if (headerInput.current !== null) {
    headerInput.current.value = headerSignal.value;
  }
};

export const SummarySectionHeaderControllerFactory =
  (
    host: string,
    formTemplateId: string,
    summarySectionHeaderClickable: SummarySectionHeaderClickable,
    headerSignal: Signal<string>,
    coordinates: Coordinates | null,
    maybeAccessCode: string | null,
  ) =>
  () => {
    useEffect(() => {
      // headerInput is unmanaged, so we need to populate it manually at the very beginning
      updateHeaderValue(headerSignal, headerInput);
      registerHeaderClickHandler(summarySectionHeaderClickable.header);
    }, []);

    const serverError = useRef<HTMLDivElement>(null);

    const headerInput = useRef<HTMLTextAreaElement>(null);

    const [windowDisplayed, setWindowDisplayed] = useState(false);

    const registerHeaderClickHandler = (element: HTMLDivElement) => {
      element.addEventListener("click", (event) => {
        setWindowDisplayed(true);
      });
    };

    const refreshHeader = (hideContent: boolean) => {
      if (headerInput.current !== null) {
        headerSignal.value = headerInput.current.value;
      }
      const summarySectionPart: SummarySection = {};

      summarySectionPart["header"] = headerSignal.value;

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
        if (response.header !== undefined) {
          refreshComponentHtml(response.header);
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

    updateHeaderValue(headerSignal, headerInput);

    // To automatically refresh page content when signal value changed (in other Controller)
    refreshHeader(false);

    const refreshComponentHtml = (title: string) => {
      summarySectionHeaderClickable.header.innerHTML = title;
    };

    const headerKeyUp = (e: KeyboardEvent) => {
      if (headerInput.current !== null) {
        // Change of the signal value will trigger rerendering of the component
        headerSignal.value = headerInput.current.value;
      }
    };

    const updateHandler = (e: MouseEvent) => {
      refreshHeader(true);
    };

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      setWindowDisplayed(false);
    };

    return (
      <div id="edit-summary-header" class="info" style={{ display: windowDisplayed ? "block" : "none" }}>
        <style>{styles}</style>
        <div>
          <label for="edit-header">Header</label>
          <textarea id="edit-header" class="form-control" rows={4} ref={headerInput} onKeyUp={headerKeyUp}></textarea>
        </div>
        <button id="update-button" class="btn btn-success" onClick={updateHandler}>
          Update header
        </button>
        <button id="cancel-button" class="btn btn-secondary" onClick={cancelClickHandler}>
          Cancel
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
