import * as styles from "bundle-text:../section/content.css";
import { useRef, useState, useEffect } from "preact/hooks";
import { RefObject } from "preact";
import { Signal } from "@preact/signals";
import type {
  Coordinates,
  ContentScriptRequest,
  SummarySectionFooterClickable,
  SummarySection,
  SummarySectionRequest,
  UpdateSummarySectionTitleResponse,
} from "../types";
import { MessageKind } from "../types";
import { onMessageHandler } from "../background/index";

const updateFooterValue = (footerSignal: Signal<string>, footerInput: RefObject<HTMLTextAreaElement>) => {
  if (footerInput.current !== null) {
    footerInput.current.value = footerSignal.value;
  }
};

export const SummarySectionFooterControllerFactory =
  (
    host: string,
    formTemplateId: string,
    summarySectionFooterClickable: SummarySectionFooterClickable,
    footerSignal: Signal<string>,
    coordinates: Coordinates | null,
    maybeAccessCode: string | null,
  ) =>
  () => {
    useEffect(() => {
      updateFooterValue(footerSignal, footerInput);
      registerFooterClickHandler(summarySectionFooterClickable.footer);
    }, []);

    const serverError = useRef<HTMLDivElement>(null);

    const footerInput = useRef<HTMLTextAreaElement>(null);

    const [windowDisplayed, setWindowDisplayed] = useState(false);

    const registerFooterClickHandler = (element: HTMLDivElement) => {
      element.addEventListener("click", (event) => {
        setWindowDisplayed(true);
      });
    };

    const refreshFooter = (hideContent: boolean) => {
      if (footerInput.current !== null) {
        footerSignal.value = footerInput.current.value;
      }

      const summarySectionPart: SummarySection = {};

      summarySectionPart["footer"] = footerSignal.value;

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
        if (response.footer !== undefined) {
          refreshComponentHtml(response.footer);
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

    updateFooterValue(footerSignal, footerInput);

    // To automatically refresh page content when signal value changed (in other Controller)
    refreshFooter(false);

    const refreshComponentHtml = (title: string) => {
      summarySectionFooterClickable.footer.innerHTML = title;
    };

    const footerKeyUp = (e: KeyboardEvent) => {
      if (footerInput.current !== null) {
        // Change of the signal value will trigger rerendering of the component
        footerSignal.value = footerInput.current.value;
      }
    };

    const updateHandler = (e: MouseEvent) => {
      refreshFooter(true);
    };

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      setWindowDisplayed(false);
    };

    return (
      <div id="edit-summary-footer" class="info" style={{ display: windowDisplayed ? "block" : "none" }}>
        <style>{styles}</style>
        <div>
          <label for="edit-footer">Footer</label>
          <textarea id="edit-footer" class="form-control" rows={4} ref={footerInput} onKeyUp={footerKeyUp}></textarea>
        </div>
        <button id="update-button" class="btn btn-success" onClick={updateHandler}>
          Update footer
        </button>
        <button id="cancel-button" class="btn btn-secondary" onClick={cancelClickHandler}>
          Cancel
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
