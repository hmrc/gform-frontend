import * as styles from "bundle-text:../section/content.css";
import { useEffect, useRef, useState } from "preact/hooks";
import { render, RefObject } from "preact";
import type {
  AtlRepeater,
  AtlDefaultPageRequest,
  UpdateAtlDefaultPageResponse,
  ContentScriptRequest,
  DefaultPage,
  DefaultPagePart,
  FormComponentId,
  FormTemplateId,
  InfoRenderParam,
} from "../types";
import { SectionNumber, MessageKind, NoteUpdateKind, SmartString } from "../types";
import { attachShadowDom } from "../pure-functions";
import { NoteComponentControllerFactory } from "../section/note-component/note-component-controller";
import { initiateInfoComponents } from "../section/info-component-helper";

import { SmartStringDiv, SmartStringInputDeprecated } from "../section/useSmartString";
import { onMessageHandler } from "../background/index";

const initiateNote = (
  sectionPath: string,
  repeater: AtlRepeater,
  host: string,
  formTemplateId: FormTemplateId,
): void => {
  const mainContent = document.getElementById("main-content");
  if (mainContent !== null) {
    const noteAttachmentDiv = document.createElement("div");
    noteAttachmentDiv.setAttribute("id", "atl-default-page-note-shadow-root");
    mainContent.insertAdjacentElement("beforebegin", noteAttachmentDiv);

    const content: HTMLDivElement | undefined = attachShadowDom(noteAttachmentDiv);
    if (content !== undefined) {
      const defaultPage = repeater.defaultPage as DefaultPage;
      const NoteControllerComponent = NoteComponentControllerFactory(
        host,
        formTemplateId,
        defaultPage.note || [],
        defaultPage.doneNote || [],
      );

      render(
        <NoteControllerComponent noteKind={NoteUpdateKind.AddToListDefaultPage} noteKindData={sectionPath} />,
        content,
      );
    }
  }
};

export const atlDefaultPageBootstrap = (
  repeater: AtlRepeater,
  formParentEl: HTMLDivElement,
  host: string,
  formTemplateId: FormTemplateId,
  sectionNumber: SectionNumber,
  sectionPath: string,
  maybeAccessCode: string | null,
) => {
  initiateNote(sectionPath, repeater, host, formTemplateId);

  const defaultPage: DefaultPage = repeater.defaultPage as DefaultPage;

  if (repeater.defaultPage !== undefined) {
    const params: InfoRenderParam = {
      host,
      formTemplateId,
      sectionNumber,
      kind: MessageKind.UpdateAtlDefaultPageFormComponent,
      requestData: sectionPath,
    };
    initiateInfoComponents(defaultPage.fields, params, maybeAccessCode);
  }

  const header: Element | null = document.querySelector("header.hmrc-page-heading");

  if (header !== null) {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "atl-default-page-shadow-root");
    header.insertAdjacentElement("afterend", attachmentDiv);
    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

    if (content !== undefined) {
      const AtlDefaultPageController = AtlDefaultPageControllerFactory(
        host,
        formTemplateId,
        sectionNumber,
        header,
        defaultPage,
        sectionPath,
        maybeAccessCode,
      );

      render(<AtlDefaultPageController />, content);
    }
  }
};

const AtlDefaultPageControllerFactory =
  (
    host: string,
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    header: Element,
    defaultPage: DefaultPage,
    sectionPath: string,
    maybeAccessCode: string | null,
  ) =>
  () => {
    useEffect(() => {
      registerTitleClickHandler(header);
    }, []);

    const serverError = useRef<HTMLDivElement>(null);
    const moreOptions = useRef<HTMLButtonElement>(null);

    const titleInput = useRef<SmartStringDiv>(null);
    const captionInput = useRef<SmartStringDiv>(null);
    const continueLabelInput = useRef<SmartStringDiv>(null);

    const [windowDisplayed, setWindowDisplayed] = useState(false);
    const [captionOnlyDisplayed, setCaptionOnlyDisplayed] = useState(false);
    const [titleOnlyDisplayed, setTitleOnlyDisplayed] = useState(false);
    const [moreOptionsDisplayed, setMoreOptionsDisplayed] = useState(false);
    const [titleValue, setTitleValue] = useState(defaultPage.title);
    const [captionValue, setCaptionValue] = useState(defaultPage.caption);
    const [continueLabelValue, setContinueLabelValue] = useState(defaultPage.continueLabel);

    const defaultState = () => {
      setWindowDisplayed(true);
      setCaptionOnlyDisplayed(false);
      setTitleOnlyDisplayed(false);
      setMoreOptionsDisplayed(false);
    };

    const registerTitleClickHandler = (element: Element) => {
      const caption: Element | null = element.querySelector("p.hmrc-caption");
      const h1: HTMLHeadingElement | null = element.querySelector("h1");

      if (caption !== null) {
        caption.addEventListener("click", (event) => {
          defaultState();
          setCaptionOnlyDisplayed(true);
        });
      }

      if (h1 !== null) {
        h1.addEventListener("click", (event) => {
          defaultState();
          setTitleOnlyDisplayed(true);
        });
      }
    };

    const moreOptionsHandler = (e: MouseEvent) => {
      setMoreOptionsDisplayed(true);
    };

    const titleKeyUp = (e: KeyboardEvent) => {
      if (titleInput.current !== null) {
        setTitleValue(titleInput.current.value);
      }
      refreshDefaultPage(false);
    };

    const captionKeyUp = (e: KeyboardEvent) => {
      if (captionInput.current !== null) {
        setCaptionValue(captionInput.current.value);
      }
      refreshDefaultPage(false);
    };

    const continueLabelKeyUp = (e: KeyboardEvent) => {
      if (continueLabelInput.current !== null) {
        setContinueLabelValue(continueLabelInput.current.value);
      }
      refreshDefaultPage(false);
    };

    const updateHandler = (e: MouseEvent) => {
      refreshDefaultPage(true);
    };

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      setWindowDisplayed(false);
    };

    const setValue = (inputRef: RefObject<SmartStringDiv>): SmartString | undefined => {
      if (inputRef.current !== null) {
        return inputRef.current.value;
      }
    };

    const refreshDefaultPage = (hideContent: boolean) => {
      const defaultPagePart: DefaultPagePart = {};

      defaultPagePart["title"] = setValue(titleInput);
      defaultPagePart["caption"] = setValue(captionInput);
      defaultPagePart["continueLabel"] = setValue(continueLabelInput);

      const atlDefaultPageRequest: AtlDefaultPageRequest = {
        payload: defaultPagePart,
        sectionNumber: sectionNumber,
        sectionPath: sectionPath,
        maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
      };
      const updateRequest: ContentScriptRequest = {
        host: host,
        kind: MessageKind.UpdateAtlDefaultPage,
        formTemplateId: formTemplateId,
        data: atlDefaultPageRequest,
      };

      onMessageHandler<UpdateAtlDefaultPageResponse>(updateRequest, (response) => {
        if (response.pageHeading !== undefined) {
          header.insertAdjacentHTML("afterend", response.pageHeading);
          header.remove();

          const newHeader: Element | null = document.querySelector("header.hmrc-page-heading");
          if (newHeader !== null) {
            header = newHeader;

            registerTitleClickHandler(header);
          }

          if (response.continueLabel !== undefined) {
            const continueLabelElem = document.querySelector("div.govuk-button-group button[type='submit']");
            if (continueLabelElem !== null) {
              continueLabelElem.innerHTML = response.continueLabel;
            }
          }

          if (hideContent) {
            setWindowDisplayed(false);
          }
          serverError.current?.classList.add("hidden");
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

    return (
      <div id="atl-default-page-controller" class="info" style={{ display: windowDisplayed ? "block" : "none" }}>
        <style>{styles}</style>
        <div style="text-align: center;">Add to list default page</div>
        <SmartStringDiv
          style={{ display: moreOptionsDisplayed || captionOnlyDisplayed ? "block" : "none" }}
          ref={captionInput}
        >
          <SmartStringInputDeprecated
            id="edit-caption"
            class="form-control"
            value={captionValue}
            onKeyUp={captionKeyUp}
          >
            Caption
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <SmartStringDiv
          style={{ display: moreOptionsDisplayed || titleOnlyDisplayed ? "block" : "none" }}
          ref={titleInput}
        >
          <SmartStringInputDeprecated id="edit-title" class="form-control" value={titleValue} onKeyUp={titleKeyUp}>
            Title
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <SmartStringDiv style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={continueLabelInput}>
          <SmartStringInputDeprecated
            id="edit-continueLabel"
            class="form-control"
            value={continueLabelValue}
            onKeyUp={continueLabelKeyUp}
          >
            Continue label
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <hr></hr>
        <button id="update-button" class="btn btn-success" onClick={updateHandler}>
          ✓ Update
        </button>
        <button id="cancel-button" class="btn btn-secondary" onClick={cancelClickHandler}>
          ✗ Cancel
        </button>
        <button
          style={{ display: moreOptionsDisplayed ? "none" : "inline" }}
          id="options-button"
          class="btn btn-link"
          onClick={moreOptionsHandler}
        >
          More options
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
