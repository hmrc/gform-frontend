import * as styles from "bundle-text:../section/content.css";
import { useEffect, useRef, useState } from "preact/hooks";
import { render, RefObject } from "preact";
import { signal, Signal } from "@preact/signals";
import type {
  AtlRepeater,
  AtlCyaPageRequest,
  UpdateAtlCyaPageResponse,
  ContentScriptRequest,
  CyaPage,
  CyaPagePart,
  FormComponentId,
  FormTemplateId,
  InfoRenderParam,
  ServerPageData,
  UpdateAtlRepeaterAddAnotherQuestionResponse,
  UpdateAtlRepeaterResponse,
} from "../types";
import { SectionNumber, MessageKind, NoteUpdateKind, SmartString } from "../types";
import { attachShadowDom } from "../pure-functions";
import { NoteComponentControllerFactory } from "../section/note-component/note-component-controller";
import { AtlCyaPageHeaderControllerFactory } from "./cya-page-header";
import { AtlCyaPageFooterControllerFactory } from "./cya-page-footer";
import { SmartStringDiv, SmartStringInputDeprecated } from "../section/useSmartString";
import { onMessageHandler } from "../background/index";

const summaryTableSelector = "div.govuk-body:has(.govuk-summary-list)";

const initiateNote = (
  repeater: AtlRepeater,
  host: string,
  formTemplateId: FormTemplateId,
  sectionPath: string,
): void => {
  const mainContent = document.getElementById("main-content");
  if (mainContent !== null) {
    const noteAttachmentDiv = document.createElement("div");
    noteAttachmentDiv.setAttribute("id", "atl-cya-page-note-shadow-root");
    mainContent.insertAdjacentElement("beforebegin", noteAttachmentDiv);

    const content: HTMLDivElement | undefined = attachShadowDom(noteAttachmentDiv);
    if (content !== undefined) {
      const cyaPage = repeater.cyaPage as CyaPage;
      const NoteControllerComponent = NoteComponentControllerFactory(
        host,
        formTemplateId,
        cyaPage.note || [],
        cyaPage.doneNote || [],
      );

      render(
        <NoteControllerComponent noteKind={NoteUpdateKind.AddToListCyaPage} noteKindData={sectionPath} />,
        content,
      );
    }
  }
};

const initiateCyaPageHeader = (
  repeater: AtlRepeater,
  host: string,
  formTemplateId: FormTemplateId,
  sectionNumber: SectionNumber,
  sectionPath: string,
  headerSignal: Signal<string>,
  maybeAccessCode: string | null,
): void => {
  const initiate = (headerEl: HTMLDivElement, cyaPage: CyaPage): void => {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "atl-cya-page-header-shadow-root");
    headerEl.insertAdjacentElement("afterend", attachmentDiv);

    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

    if (content !== undefined) {
      const AtlCyaPageHeaderController = AtlCyaPageHeaderControllerFactory(
        host,
        formTemplateId,
        sectionNumber,
        headerEl,
        cyaPage,
        sectionPath,
        headerSignal,
        maybeAccessCode,
      );

      render(<AtlCyaPageHeaderController />, content);
    }
  };
  if (repeater.cyaPage !== undefined) {
    const cyaPage = repeater.cyaPage as CyaPage;
    if (cyaPage.header === undefined) {
      // If there is no a header, we need to create its div manually
      const tableElem = document.querySelector(summaryTableSelector);
      if (tableElem !== null) {
        const headerEl = document.createElement("div");
        headerEl.classList.add("govuk-body");
        // .govuk-body has by default 'margin-bottom: 20px;' we need to prevent that
        // so we don't see visible shift when 'headerEl' is added to the page
        headerEl.style.marginBottom = "0px";
        tableElem.insertAdjacentElement("beforebegin", headerEl);
        initiate(headerEl, cyaPage);
      }
    } else {
      const headerEl: HTMLDivElement | null = document.querySelector("main .govuk-body");

      if (headerEl !== null) {
        initiate(headerEl, cyaPage);
      }
    }
  }
};

const initiateCyaPageFooter = (
  repeater: AtlRepeater,
  host: string,
  formTemplateId: FormTemplateId,
  sectionNumber: SectionNumber,
  sectionPath: string,
  footerSignal: Signal<string>,
  maybeAccessCode: string | null,
): void => {
  const initiate = (footerEl: HTMLDivElement, cyaPage: CyaPage): void => {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "atl-cya-page-footer-shadow-root");
    footerEl.insertAdjacentElement("afterend", attachmentDiv);

    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

    if (content !== undefined) {
      const AtlCyaPageFooterController = AtlCyaPageFooterControllerFactory(
        host,
        formTemplateId,
        sectionNumber,
        footerEl,
        cyaPage,
        sectionPath,
        footerSignal,
        maybeAccessCode,
      );

      render(<AtlCyaPageFooterController />, content);
    }
  };
  if (repeater.cyaPage !== undefined) {
    const cyaPage = repeater.cyaPage as CyaPage;
    if (cyaPage.footer === undefined) {
      // If there is no a footer, we need to create its div manually
      const tableElem = document.querySelector(summaryTableSelector);
      if (tableElem !== null) {
        const footerEl = document.createElement("div");
        footerEl.classList.add("govuk-body");
        tableElem.insertAdjacentElement("afterend", footerEl);
        initiate(footerEl, cyaPage);
      }
    } else {
      const footerIndex = cyaPage.header === undefined ? 1 : 2;

      const footerEl: Element | null = document.querySelectorAll("main .govuk-body").item(footerIndex);

      if (footerEl !== null && footerEl instanceof HTMLDivElement) {
        initiate(footerEl, cyaPage);
      }
    }
  }
};

export const atlCyaPageBootstrap = (
  repeater: AtlRepeater,
  formParentEl: HTMLDivElement,
  host: string,
  formTemplateId: FormTemplateId,
  sectionNumber: SectionNumber,
  sectionPath: string,
  maybeAccessCode: string | null,
) => {
  const cyaPage: CyaPage = repeater.cyaPage as CyaPage;

  const headerSignal: Signal<string> = signal(cyaPage.header || "");
  const footerSignal: Signal<string> = signal(cyaPage.footer || "");

  initiateNote(repeater, host, formTemplateId, sectionPath);

  initiateCyaPageHeader(repeater, host, formTemplateId, sectionNumber, sectionPath, headerSignal, maybeAccessCode);

  initiateCyaPageFooter(repeater, host, formTemplateId, sectionNumber, sectionPath, footerSignal, maybeAccessCode);

  const header: Element | null = document.querySelector("header.hmrc-page-heading");

  if (header !== null) {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "atl-cya-page-shadow-root");
    header.insertAdjacentElement("afterend", attachmentDiv);
    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

    if (content !== undefined) {
      const AtlCyaPageController = AtlCyaPageControllerFactory(
        host,
        formTemplateId,
        sectionNumber,
        header,
        cyaPage,
        sectionPath,
        headerSignal,
        footerSignal,
        maybeAccessCode,
      );

      render(<AtlCyaPageController />, content);
    }
  }
};

const updateTextArea = (signal: Signal<string>, input: RefObject<HTMLTextAreaElement>) => {
  if (input.current !== null) {
    input.current.value = signal.value;
  }
};

const AtlCyaPageControllerFactory =
  (
    host: string,
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    header: Element,
    cyaPage: CyaPage,
    sectionPath: string,
    headerSignal: Signal<string>,
    footerSignal: Signal<string>,
    maybeAccessCode: string | null,
  ) =>
  () => {
    const refreshLandingPageLayout = (displayWidth: string): void => {
      const layoutEl = document
        .querySelectorAll("#main-content .govuk-grid-row .govuk-grid-column-full .govuk-grid-row")
        .item(1)
        .children.item(0);

      const className = layoutEl?.className;
      if (className) {
        switch (displayWidth) {
          case "":
          case "m":
            layoutEl?.classList.replace(className, "govuk-grid-column-two-thirds");
            break;
          case "l":
            layoutEl?.classList.replace(className, "govuk-grid-column-three-quarters");
            break;
          case "xl":
            layoutEl?.classList.replace(className, "govuk-grid-column-full");
            break;
        }
      }
    };

    useEffect(() => {
      registerTitleClickHandler(header);
    }, []);

    const serverError = useRef<HTMLDivElement>(null);
    const moreOptions = useRef<HTMLButtonElement>(null);

    const captionInput = useRef<SmartStringDiv>(null);
    const titleInput = useRef<SmartStringDiv>(null);
    const updateTitleInput = useRef<SmartStringDiv>(null);
    const noPIITitleInput = useRef<SmartStringDiv>(null);
    const noPIIUpdateTitleInput = useRef<SmartStringDiv>(null);
    const continueLabelInput = useRef<SmartStringDiv>(null);
    const removeItemIfInput = useRef<HTMLInputElement>(null);
    const presentationHintContainer = useRef<HTMLDivElement>(null);
    const displayWidthInput = useRef<HTMLSelectElement>(null);
    const keyDisplayWidthInput = useRef<HTMLSelectElement>(null);
    const displayWidthContainer = useRef<HTMLDivElement>(null);
    const keyDisplayWidthContainer = useRef<HTMLDivElement>(null);
    const headerInput = useRef<HTMLTextAreaElement>(null);
    const footerInput = useRef<HTMLTextAreaElement>(null);

    const [windowDisplayed, setWindowDisplayed] = useState(false);
    const [captionOnlyDisplayed, setCaptionOnlyDisplayed] = useState(false);
    const [titleOnlyDisplayed, setTitleOnlyDisplayed] = useState(false);
    const [moreOptionsDisplayed, setMoreOptionsDisplayed] = useState(false);
    const [captionValue, setCaptionValue] = useState(cyaPage.caption);
    const [titleValue, setTitleValue] = useState(cyaPage.title);
    const [updateTitleValue, setUpdateTitleValue] = useState(cyaPage.updateTitle);
    const [noPIITitleValue, setNoPIITitleValue] = useState(cyaPage.noPIITitle);
    const [noPIIUpdateTitleValue, setNoPIIUpdateTitleValue] = useState(cyaPage.noPIIUpdateTitle);
    const [continueLabelValue, setContinueLabelValue] = useState(cyaPage.continueLabel);
    const [displayWidthValue, setDisplayWidthValue] = useState(cyaPage.displayWidth);
    const [keyDisplayWidthValue, setKeyDisplayWidthValue] = useState(cyaPage.keyDisplayWidth);
    const [removeItemIfValue, setRemoveItemIfValue] = useState(cyaPage.removeItemIf);

    const invisiblePageTitle = cyaPage.presentationHint === "invisiblePageTitle";
    const [yesChecked, setYesChecked] = useState(!invisiblePageTitle);
    const [noChecked, setNoChecked] = useState(invisiblePageTitle);

    updateTextArea(headerSignal, headerInput);
    updateTextArea(footerSignal, footerInput);

    const cyaState = () => {
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
          cyaState();
          setCaptionOnlyDisplayed(true);
        });
      }

      if (h1 !== null) {
        h1.addEventListener("click", (event) => {
          cyaState();
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
      refreshCyaPage(false);
    };

    const updateTitleKeyUp = (e: KeyboardEvent) => {
      if (updateTitleInput.current !== null) {
        setUpdateTitleValue(updateTitleInput.current.value);
      }
      refreshCyaPage(false);
    };

    const noPIITitleKeyUp = (e: KeyboardEvent) => {
      if (noPIITitleInput.current !== null) {
        setNoPIITitleValue(noPIITitleInput.current.value);
      }
      refreshCyaPage(false);
    };

    const noPIIUpdateTitleKeyUp = (e: KeyboardEvent) => {
      if (noPIIUpdateTitleInput.current !== null) {
        setNoPIIUpdateTitleValue(noPIIUpdateTitleInput.current.value);
      }
      refreshCyaPage(false);
    };

    const captionKeyUp = (e: KeyboardEvent) => {
      if (captionInput.current !== null) {
        setCaptionValue(captionInput.current.value);
      }
      refreshCyaPage(false);
    };

    const continueLabelKeyUp = (e: KeyboardEvent) => {
      if (continueLabelInput.current !== null) {
        setContinueLabelValue(continueLabelInput.current.value);
      }
      refreshCyaPage(false);
    };

    const removeItemIfKeyUp = (e: KeyboardEvent) => {
      if (removeItemIfInput.current !== null) {
        setRemoveItemIfValue(removeItemIfInput.current.value);
      }
      refreshCyaPage(false);
    };

    const updateHandler = (e: MouseEvent) => {
      refreshCyaPage(true);
    };

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      setWindowDisplayed(false);
    };

    const yesToggle = (e: MouseEvent) => {
      setYesChecked(true);
      setNoChecked(false);
    };

    const onYesChange = (e: Event) => {
      refreshCyaPage(false);
    };

    const noToggle = (e: MouseEvent) => {
      setYesChecked(false);
      setNoChecked(true);
    };

    const onNoChange = (e: Event) => {
      refreshCyaPage(false);
    };

    const setValue = (inputRef: RefObject<SmartStringDiv>): SmartString | undefined => {
      if (inputRef.current !== null) {
        return inputRef.current.value;
      }
    };

    const setValueInput = (inputRef: RefObject<HTMLInputElement>): string | undefined => {
      if (inputRef.current !== null) {
        return inputRef.current.value;
      }
    };

    const setValueSelect = (selectRef: RefObject<HTMLSelectElement>): string | undefined => {
      if (selectRef.current !== null) {
        return selectRef.current.value;
      }
    };

    const setValueRadio = (checked: boolean): string => {
      if (checked) {
        return "";
      } else {
        return "invisiblePageTitle";
      }
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

    const refreshCyaPage = (hideContent: boolean) => {
      const cyaPagePart: CyaPagePart = {};

      cyaPagePart["caption"] = setValue(captionInput);
      cyaPagePart["title"] = setValue(titleInput);
      cyaPagePart["updateTitle"] = setValue(updateTitleInput);
      cyaPagePart["noPIITitle"] = setValue(noPIITitleInput);
      cyaPagePart["noPIIUpdateTitle"] = setValue(noPIIUpdateTitleInput);
      cyaPagePart["continueLabel"] = setValue(continueLabelInput);
      cyaPagePart["removeItemIf"] = setValueInput(removeItemIfInput);
      cyaPagePart["displayWidth"] = setValueSelect(displayWidthInput);
      cyaPagePart["keyDisplayWidth"] = setValueSelect(keyDisplayWidthInput);
      cyaPagePart["presentationHint"] = setValueRadio(yesChecked);
      cyaPagePart["header"] = headerSignal.value;
      cyaPagePart["footer"] = footerSignal.value;

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
        if (response.error !== undefined) {
          if (serverError.current !== null) {
            serverError.current.innerText = response.error;
            serverError.current.classList.remove("hidden");
          }
        } else {
          header.insertAdjacentHTML("afterend", response.pageHeading);
          header.remove();

          const newHeader: Element | null = document.querySelector("header.hmrc-page-heading");
          if (newHeader !== null) {
            header = newHeader;
            registerTitleClickHandler(header);
          }

          if (displayWidthInput.current !== null) {
            const displayWidthCurrent: HTMLSelectElement = displayWidthInput.current;
            if (displayWidthCurrent.checkVisibility()) {
              refreshLandingPageLayout(displayWidthCurrent.value);
            }
          }

          if (keyDisplayWidthInput.current !== null) {
            const keyDisplayWidthCurrent: HTMLSelectElement = keyDisplayWidthInput.current;
            if (keyDisplayWidthCurrent.checkVisibility()) {
              refreshLandingPageLayout(keyDisplayWidthCurrent.value);
            }
          }

          const continueLabelElem = document.querySelector("div.govuk-button-group button[type='submit']");
          if (continueLabelElem !== null) {
            continueLabelElem.innerHTML = response.continueLabel;
          }

          const tableElem = document.querySelector(summaryTableSelector);
          if (tableElem !== null) {
            tableElem.innerHTML = response.summaryTable;
          }

          document.title = response.noPIITitle;

          if (hideContent) {
            setWindowDisplayed(false);
          }
          serverError.current?.classList.add("hidden");
        }
      });
    };

    const onDisplayWidthChange = (e: Event) => {
      refreshCyaPage(false);
    };

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
      <div id="atl-cya-page-controller" class="info" style={{ display: windowDisplayed ? "block" : "none" }}>
        <style>{styles}</style>
        <div style="text-align: center;">Add to list cya page</div>
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
            Title{" "}
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <SmartStringDiv style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={updateTitleInput}>
          <SmartStringInputDeprecated
            id="edit-updateTitle"
            class="form-control"
            value={updateTitleValue}
            onKeyUp={updateTitleKeyUp}
          >
            Update Title
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <SmartStringDiv style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={noPIITitleInput}>
          <SmartStringInputDeprecated
            id="edit-noPIITitle"
            class="form-control"
            value={noPIITitleValue}
            onKeyUp={noPIITitleKeyUp}
          >
            No PII Title
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <SmartStringDiv style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={noPIIUpdateTitleInput}>
          <SmartStringInputDeprecated
            id="edit-noPIIUpdateTitle"
            class="form-control"
            value={noPIIUpdateTitleValue}
            onKeyUp={noPIIUpdateTitleKeyUp}
          >
            No PII Update Title
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
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={presentationHintContainer}>
          Display page headers:
          <input type="radio" id="yes" checked={yesChecked} onClick={yesToggle} onChange={onYesChange} />
          <label for="yes">Yes</label>
          <input type="radio" id="no" checked={noChecked} onClick={noToggle} onChange={onNoChange} />
          <label for="no">No</label>
        </div>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={displayWidthContainer}>
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
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={keyDisplayWidthContainer}>
          <label for="edit-keyDisplayWidth">Key display width</label>
          <select
            id="edit-keyDisplayWidth"
            class="form-control"
            value={keyDisplayWidthValue}
            ref={keyDisplayWidthInput}
            onChange={onDisplayWidthChange}
            onBlur={onKeyDisplayWidthBlur}
          >
            <option value="">Default</option>
            <option value="m">m - Medium</option>
            <option value="l">l - Large</option>
          </select>
        </div>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <label for="edit-removeItemIf">Remove item if</label>
          <input
            id="edit-removeItemIf"
            class="form-control"
            value={removeItemIfValue}
            ref={removeItemIfInput}
            onKeyUp={removeItemIfKeyUp}
          />
        </div>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <label for="edit-header">Header</label>
          <textarea id="edit-header" class="form-control" rows={2} ref={headerInput} onKeyUp={headerKeyUp}></textarea>
        </div>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <label for="edit-footer">Footer</label>
          <textarea id="edit-footer" class="form-control" rows={2} ref={footerInput} onKeyUp={footerKeyUp}></textarea>
        </div>
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
