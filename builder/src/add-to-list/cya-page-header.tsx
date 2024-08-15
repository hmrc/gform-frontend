import * as styles from "bundle-text:../section/content.css";
import { useRef, useState, useEffect } from "preact/hooks";
import { RefObject } from "preact";
import { Signal } from "@preact/signals";
import type {
  ContentScriptRequest,
  CyaPage,
  CyaPagePart,
  AtlCyaPageRequest,
  FormTemplateId,
  UpdateAtlCyaPageResponse,
} from "../types";
import { SectionNumber, MessageKind } from "../types";
import { onMessageHandler } from "../background/index";

const updateHeaderValue = (headerSignal: Signal<string>, headerInput: RefObject<HTMLTextAreaElement>) => {
  if (headerInput.current !== null) {
    headerInput.current.value = headerSignal.value;
  }
};

export const AtlCyaPageHeaderControllerFactory =
  (
    host: string,
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    headerEl: HTMLDivElement,
    cyaPage: CyaPage,
    sectionPath: string,
    headerSignal: Signal<string>,
    maybeAccessCode: string | null,
  ) =>
  () => {
    useEffect(() => {
      updateHeaderValue(headerSignal, headerInput);
      registerHeaderClickHandler(headerEl);
    }, []);

    const registerHeaderClickHandler = (element: HTMLDivElement) => {
      element.addEventListener("click", (event) => {
        setWindowDisplayed(true);
      });
    };

    const serverError = useRef<HTMLDivElement>(null);

    const headerInput = useRef<HTMLTextAreaElement>(null);

    const [windowDisplayed, setWindowDisplayed] = useState(false);

    const refreshHeader = (hideContent: boolean) => {
      if (headerInput.current !== null) {
        headerSignal.value = headerInput.current.value;
      }

      const cyaPagePart: CyaPagePart = {};

      cyaPagePart["header"] = headerSignal.value;

      const atlCyaPageRequest: AtlCyaPageRequest = {
        payload: cyaPagePart,
        sectionNumber: sectionNumber,
        sectionPath: sectionPath,
        maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
      };

      const updateRequest: ContentScriptRequest = {
        host: host,
        kind: MessageKind.UpdateAtlCyaPage,
        formTemplateId: formTemplateId,
        data: atlCyaPageRequest,
      };

      onMessageHandler<UpdateAtlCyaPageResponse>(updateRequest, (response) => {
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

    const refreshComponentHtml = (header: string) => {
      headerEl.innerHTML = header;
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
          ✓ Update header
        </button>
        <button id="cancel-button" class="btn btn-secondary" onClick={cancelClickHandler}>
          ✗ Cancel
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
