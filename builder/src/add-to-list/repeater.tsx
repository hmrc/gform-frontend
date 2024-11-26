import * as styles from "bundle-text:../section/content.css";
import { useEffect, useRef, useState } from "preact/hooks";
import { signal, Signal } from "@preact/signals";
import { render } from "preact";
import type {
  ServerPageData,
  AddAnotherQuestion,
  AddAnotherQuestionPart,
  AtlRepeater,
  UpdateAtlRepeaterResponse,
  UpdateAtlRepeaterAddAnotherQuestionResponse,
  FormTemplateId,
  FormComponentId,
  AtlRepeaterRequest,
  AtlRepeaterAddAnotherQuestionRequest,
  ContentScriptRequest,
  InfoRenderParam,
} from "../types";
import { SectionNumber, MessageKind, NoteUpdateKind } from "../types";
import { attachShadowDom } from "../pure-functions";
import { NoteComponentControllerFactory } from "../section/note-component/note-component-controller";
import { initiateInfoComponents } from "../section/info-component-helper";
import { SmartStringDiv, SmartStringInputDeprecated, SmartStringTextArea } from "../section/useSmartString";
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
    noteAttachmentDiv.setAttribute("id", "atl-repeater-note-shadow-root");
    mainContent.insertAdjacentElement("beforebegin", noteAttachmentDiv);

    const content: HTMLDivElement | undefined = attachShadowDom(noteAttachmentDiv);
    if (content !== undefined) {
      const NoteControllerComponent = NoteComponentControllerFactory(
        host,
        formTemplateId,
        repeater.note || [],
        repeater.doneNote || [],
      );

      render(
        <NoteControllerComponent noteKind={NoteUpdateKind.AddToListRepeater} noteKindData={sectionPath} />,
        content,
      );
    }
  }
};

export const atlRepeaterBootstrap = (
  repeater: AtlRepeater,
  atlIterationIndex: number,
  addToListId: FormComponentId,
  formParentEl: HTMLDivElement,
  host: string,
  formTemplateId: FormTemplateId,
  sectionNumber: SectionNumber,
  sectionPath: string,
  maybeAccessCode: string | null,
) => {
  const sectionNumberChangeSignal: Signal<SectionNumber | undefined> = signal(undefined);

  initiateNote(sectionPath, repeater, host, formTemplateId);

  if (repeater.fields !== undefined) {
    const params: InfoRenderParam = {
      host,
      formTemplateId,
      sectionNumber,
      kind: MessageKind.UpdateAtlRepeaterFormComponent,
      atlIterationIndex,
      sectionNumberChange: sectionNumberChangeSignal,
      requestData: sectionPath,
    };
    initiateInfoComponents(repeater.fields, params, maybeAccessCode);
  }

  // classes 'hmrc-page-heading' and 'hmrc-caption' are guaranteed to exists, see:
  // play-frontend-hmrc/src/main/twirl/uk/gov/hmrc/hmrcfrontend/views/components/HmrcPageHeading.scala.html
  const header: Element | null = document.querySelector("header.hmrc-page-heading");

  if (header !== null) {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "atl-repeater-header-shadow-root");
    header.insertAdjacentElement("afterend", attachmentDiv);
    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

    if (content !== undefined) {
      const RepeaterController = RepeaterControllerFactory(
        host,
        formTemplateId,
        sectionNumber,
        header,
        repeater,
        atlIterationIndex,
        addToListId,
        sectionNumberChangeSignal,
        sectionPath,
        maybeAccessCode,
      );

      render(<RepeaterController />, content);
    }
  }

  const addAnotherQuestion: Element | null = document.querySelector("form div.govuk-form-group");

  if (addAnotherQuestion !== null) {
    const attachmentDiv = document.createElement("div");
    attachmentDiv.setAttribute("id", "atl-repeater-add-another-question-shadow-root");
    addAnotherQuestion.insertAdjacentElement("afterend", attachmentDiv);
    const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);

    if (content !== undefined && repeater.addAnotherQuestion !== undefined) {
      const RepeaterAddAnotherQuestionController = RepeaterAddAnotherQuestionControllerFactory(
        host,
        formTemplateId,
        sectionNumber,
        addAnotherQuestion,
        repeater.addAnotherQuestion,
        sectionNumberChangeSignal,
        sectionPath,
        maybeAccessCode,
      );

      render(<RepeaterAddAnotherQuestionController />, content);
    }
  }
};

export const RepeaterAddAnotherQuestionControllerFactory =
  (
    host: string,
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    addAnotherQuestion: Element,
    formComponent: AddAnotherQuestion,
    sectionNumberChangeSignal: Signal<SectionNumber | undefined>,
    sectionPath: string,
    maybeAccessCode: string | null,
  ) =>
  () => {
    useEffect(() => {
      registerTitleClickHandler(addAnotherQuestion);
    }, []);

    if (sectionNumberChangeSignal.value !== undefined) {
      sectionNumber = sectionNumberChangeSignal.value;
    }

    const serverError = useRef<HTMLDivElement>(null);
    const moreOptions = useRef<HTMLButtonElement>(null);

    const labelInput = useRef<SmartStringDiv>(null);
    const errorMessageInput = useRef<SmartStringDiv>(null);

    const [windowDisplayed, setWindowDisplayed] = useState(false);

    const [labelValue, setLabelValue] = useState(formComponent.label);
    const [errorMessageValue, setErrorMessageValue] = useState(formComponent.errorMessage);

    const registerTitleClickHandler = (element: Element) => {
      const legend: Element | null = element.querySelector("fieldset legend");

      if (legend !== null) {
        legend.addEventListener("click", (event) => {
          defaultState();
        });
      }
    };

    const defaultState = () => {
      setWindowDisplayed(true);
    };

    const labelKeyUp = (e: KeyboardEvent) => {
      if (labelInput.current !== null) {
        setLabelValue(labelInput.current.value);
      }
      refreshRepeater(false);
    };
    const errorMessageKeyUp = (e: KeyboardEvent) => {
      if (errorMessageInput.current !== null) {
        setErrorMessageValue(errorMessageInput.current.value);
      }
      refreshRepeater(false);
    };

    const updateHandler = (e: MouseEvent) => {
      refreshRepeater(true);
    };

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      setWindowDisplayed(false);
    };

    const refreshRepeater = (hideContent: boolean) => {
      const addAnotherQuestionPart: AddAnotherQuestionPart = {};
      if (labelInput.current !== null) {
        const labelCurrent: SmartStringDiv = labelInput.current;
        addAnotherQuestionPart["label"] = labelCurrent.value;
      }
      if (errorMessageInput.current !== null) {
        const errorMessageCurrent: SmartStringDiv = errorMessageInput.current;
        addAnotherQuestionPart["errorMessage"] = errorMessageCurrent.value;
      }

      const atlRepeaterRequest: AtlRepeaterAddAnotherQuestionRequest = {
        payload: addAnotherQuestionPart,
        sectionNumber: sectionNumber,
        sectionPath: sectionPath,
        maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
      };

      const updateRequest: ContentScriptRequest = {
        host: host,
        kind: MessageKind.UpdateAtlRepeaterAddAnotherQuestion,
        formTemplateId: formTemplateId,
        data: atlRepeaterRequest,
      };

      onMessageHandler<UpdateAtlRepeaterAddAnotherQuestionResponse>(updateRequest, (response) => {
        if (response.label !== undefined) {
          const legend: Element | null = addAnotherQuestion.querySelector("fieldset legend");

          if (legend !== null) {
            legend.innerHTML = response.label;
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
      <div
        id="atl-repeater-add-another-question-controller"
        class="info"
        style={{ display: windowDisplayed ? "block" : "none" }}
      >
        <style>{styles}</style>
        <SmartStringDiv ref={labelInput}>
          <SmartStringInputDeprecated id="edit-label" class="form-control" value={labelValue} onKeyUp={labelKeyUp}>
            Label{" "}
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <SmartStringDiv ref={errorMessageInput}>
          <SmartStringInputDeprecated
            id="edit-errorMessage"
            class="form-control"
            value={errorMessageValue}
            onKeyUp={errorMessageKeyUp}
          >
            Error message
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <hr></hr>
        <button id="update-button" class="btn btn-success" onClick={updateHandler}>
          ✓ Update
        </button>
        <button id="cancel-button" class="btn btn-secondary" onClick={cancelClickHandler}>
          ✗ Cancel
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };

const RepeaterControllerFactory =
  (
    host: string,
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    header: Element,
    repeater: AtlRepeater,
    atlIterationIndex: number,
    addToListId: FormComponentId,
    sectionNumberChangeSignal: Signal<SectionNumber | undefined>,
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
    const summaryNameInput = useRef<HTMLInputElement>(null);
    const summaryDescriptionInput = useRef<HTMLInputElement>(null);
    const shortNameInput = useRef<HTMLInputElement>(null);
    const descriptionInput = useRef<HTMLTextAreaElement>(null);
    const presentationHintContainer = useRef<HTMLDivElement>(null);
    const repeatsWhileInput = useRef<HTMLInputElement>(null);
    const repeatsUntilInput = useRef<HTMLInputElement>(null);
    const pageIdToDisplayAfterRemoveInput = useRef<HTMLInputElement>(null);

    const [windowDisplayed, setWindowDisplayed] = useState(false);
    const [captionOnlyDisplayed, setCaptionOnlyDisplayed] = useState(false);
    const [titleOnlyDisplayed, setTitleOnlyDisplayed] = useState(false);
    const [moreOptionsDisplayed, setMoreOptionsDisplayed] = useState(false);
    const [defaultPageExists, setDefaultPageExists] = useState(repeater.defaultPage !== undefined);
    const [cyaPageExists, setCyaPageExists] = useState(repeater.cyaPage !== undefined);
    const [titleValue, setTitleValue] = useState(repeater.title);
    const [captionValue, setCaptionValue] = useState(repeater.caption);
    const [summaryNameValue, setSummaryNameValue] = useState(repeater.summaryName);
    const [summaryDescriptionValue, setSummaryDescriptionValue] = useState(repeater.summaryDescription);
    const [shortNameValue, setShortNameValue] = useState(repeater.shortName);
    const [descriptionValue, setDescriptionValue] = useState(repeater.description);
    const [repeatsWhileValue, setRepeatsWhileValue] = useState(repeater.repeatsWhile);
    const [repeatsUntilValue, setRepeatsUntilValue] = useState(repeater.repeatsUntil);
    const [pageIdToDisplayAfterRemoveValue, setPageIdToDisplayAfterRemoveValue] = useState(
      repeater.pageIdToDisplayAfterRemove,
    );

    const invisiblePageTitle = repeater.presentationHint === "invisiblePageTitle";
    const [yesChecked, setYesChecked] = useState(!invisiblePageTitle);
    const [noChecked, setNoChecked] = useState(invisiblePageTitle);

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
      refreshRepeater(false);
    };

    const captionKeyUp = (e: KeyboardEvent) => {
      if (captionInput.current !== null) {
        setCaptionValue(captionInput.current.value);
      }
      refreshRepeater(false);
    };

    const summaryNameKeyUp = (e: KeyboardEvent) => {
      if (summaryNameInput.current !== null) {
        setSummaryNameValue(summaryNameInput.current.value);
      }
      refreshRepeater(false);
    };

    const summaryDescriptionKeyUp = (e: KeyboardEvent) => {
      if (summaryDescriptionInput.current !== null) {
        setSummaryDescriptionValue(summaryDescriptionInput.current.value);
      }
      refreshRepeater(false);
    };

    const shortNameKeyUp = (e: KeyboardEvent) => {
      if (shortNameInput.current !== null) {
        setShortNameValue(shortNameInput.current.value);
      }
      refreshRepeater(false);
    };

    const descriptionKeyUp = (e: KeyboardEvent) => {
      if (descriptionInput.current !== null) {
        setDescriptionValue(descriptionInput.current.value);
      }
      refreshRepeater(false);
    };

    const repeatsWhileKeyUp = (e: KeyboardEvent) => {
      if (repeatsWhileInput.current !== null) {
        setRepeatsWhileValue(repeatsWhileInput.current.value);
      }
      refreshRepeater(false);
    };

    const repeatsUntilKeyUp = (e: KeyboardEvent) => {
      if (repeatsUntilInput.current !== null) {
        setRepeatsUntilValue(repeatsUntilInput.current.value);
      }
      refreshRepeater(false);
    };
    const pageIdToDisplayAfterRemoveKeyUp = (e: KeyboardEvent) => {
      if (pageIdToDisplayAfterRemoveInput.current !== null) {
        setPageIdToDisplayAfterRemoveValue(pageIdToDisplayAfterRemoveInput.current.value);
      }
      refreshRepeater(false);
    };

    const yesToggle = (e: MouseEvent) => {
      setYesChecked(true);
      setNoChecked(false);
    };

    const noToggle = (e: MouseEvent) => {
      setYesChecked(false);
      setNoChecked(true);
    };

    const updateHandler = (e: MouseEvent) => {
      refreshRepeater(true);
    };

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      setWindowDisplayed(false);
    };

    const addDefaultPage = (e: MouseEvent) => {
      const atlRepeaterPart: AtlRepeater = {};

      atlRepeaterPart["defaultPage"] = {
        title: "Next give us details of each item",
        fields: [
          {
            id: "addPersonDefaultPageId1",
            type: "info",
            label: "",
            infoText: "You will be able to add items one at a time.",
            infoType: "noformat",
          },
        ],
      };

      repeater.defaultPage = atlRepeaterPart["defaultPage"]; // Update local model

      refreshRepeaterWith(false, atlRepeaterPart, sectionNumber);

      setDefaultPageExists(true);
    };

    const deleteDefaultPage = (e: MouseEvent) => {
      const atlRepeaterPart: AtlRepeater = {};

      atlRepeaterPart["defaultPage"] = "";

      repeater.defaultPage = undefined; // Update local model

      refreshRepeaterWith(false, atlRepeaterPart, sectionNumber);

      setDefaultPageExists(false);
    };

    const addCyaPage = (e: MouseEvent) => {
      const atlRepeaterPart: AtlRepeater = {};

      atlRepeaterPart["cyaPage"] = {
        title: "Check your answers",
        updateTitle: "Check your answers",
      };

      repeater.cyaPage = atlRepeaterPart["cyaPage"]; // Update local model

      refreshRepeaterWith(false, atlRepeaterPart, sectionNumber);

      setCyaPageExists(true);
    };

    const deleteCyaPage = (e: MouseEvent) => {
      const atlRepeaterPart: AtlRepeater = {};

      atlRepeaterPart["cyaPage"] = "";

      repeater.cyaPage = undefined; // Update local model

      refreshRepeaterWith(false, atlRepeaterPart, sectionNumber);

      setCyaPageExists(false);
    };

    const refreshRepeater = (hideContent: boolean) => {
      const atlRepeaterPart: AtlRepeater = {};
      atlRepeaterPart["defaultPage"] = repeater.defaultPage;

      refreshRepeaterWith(hideContent, atlRepeaterPart, sectionNumber);
    };

    const refreshRepeaterWith = (
      hideContent: boolean,
      atlRepeaterPart: AtlRepeater,
      sectionNumberAfterUpdate: SectionNumber,
    ) => {
      if (titleInput.current !== null) {
        const titleCurrent: SmartStringDiv = titleInput.current;
        atlRepeaterPart["title"] = titleCurrent.value;
      }
      if (captionInput.current !== null) {
        const captionCurrent: SmartStringDiv = captionInput.current;
        atlRepeaterPart["caption"] = captionCurrent.value;
      }

      if (summaryNameInput.current !== null) {
        const summaryNameCurrent: HTMLInputElement = summaryNameInput.current;
        atlRepeaterPart["summaryName"] = summaryNameCurrent.value;
      }

      if (summaryDescriptionInput.current !== null) {
        const summaryDescriptionCurrent: HTMLInputElement = summaryDescriptionInput.current;
        atlRepeaterPart["summaryDescription"] = summaryDescriptionCurrent.value;
      }

      if (repeatsWhileInput.current !== null) {
        const repeatsWhileCurrent: HTMLInputElement = repeatsWhileInput.current;
        atlRepeaterPart["repeatsWhile"] = repeatsWhileCurrent.value;
      }

      if (repeatsUntilInput.current !== null) {
        const repeatsUntilCurrent: HTMLInputElement = repeatsUntilInput.current;
        atlRepeaterPart["repeatsUntil"] = repeatsUntilCurrent.value;
      }

      if (pageIdToDisplayAfterRemoveInput.current !== null) {
        const pageIdToDisplayAfterRemoveCurrent: HTMLInputElement = pageIdToDisplayAfterRemoveInput.current;
        atlRepeaterPart["pageIdToDisplayAfterRemove"] = pageIdToDisplayAfterRemoveCurrent.value;
      }

      if (shortNameInput.current !== null) {
        const shortNameCurrent: HTMLInputElement = shortNameInput.current;
        atlRepeaterPart["shortName"] = shortNameCurrent.value;
      }

      if (descriptionInput.current !== null) {
        const descriptionCurrent: HTMLTextAreaElement = descriptionInput.current;
        atlRepeaterPart["description"] = descriptionCurrent.value;
      }

      if (presentationHintContainer.current?.checkVisibility()) {
        atlRepeaterPart["presentationHint"] = noChecked ? "invisiblePageTitle" : "";
      }

      const atlRepeaterRequest: AtlRepeaterRequest = {
        payload: atlRepeaterPart,
        sectionNumber: sectionNumber,
        sectionNumberAfterUpdate: sectionNumberAfterUpdate,
        sectionPath: sectionPath,
        maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
      };
      const updateRequest: ContentScriptRequest = {
        host: host,
        kind: MessageKind.UpdateAtlRepeater,
        formTemplateId: formTemplateId,
        data: atlRepeaterRequest,
      };

      onMessageHandler<UpdateAtlRepeaterResponse>(updateRequest, (response) => {
        if (response.pageHeading !== undefined) {
          header.insertAdjacentHTML("afterend", response.pageHeading);
          header.remove();

          const new_header: Element | null = document.querySelector("header.hmrc-page-heading");
          if (new_header !== null) {
            header = new_header;

            registerTitleClickHandler(header);
          }

          if (response.descriptions !== undefined) {
            const descriptions: NodeListOf<Element> = document.querySelectorAll("dt.hmrc-summary-list__key");
            response.descriptions.map((desc, index) => {
              let tempParagraph = document.createElement("p");
              tempParagraph.innerHTML = desc;
              descriptions[index].innerHTML = tempParagraph.innerText;
            });
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
      <div id="atl-repeater-controller" class="info" style={{ display: windowDisplayed ? "block" : "none" }}>
        <style>{styles}</style>
        <div style="text-align: center;">Add to list id: {addToListId}</div>
        <SmartStringDiv
          style={{ display: moreOptionsDisplayed || captionOnlyDisplayed ? "block" : "none" }}
          ref={captionInput}
        >
          <label for="edit-caption">Caption</label>
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
        <SmartStringDiv style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={summaryNameInput}>
          <SmartStringInputDeprecated
            id="edit-summaryName"
            class="form-control"
            value={summaryNameValue}
            onKeyUp={summaryNameKeyUp}
          >
            Summary name
          </SmartStringInputDeprecated>
          Summary name
        </SmartStringDiv>
        <SmartStringDiv style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={shortNameInput}>
          <SmartStringInputDeprecated
            id="edit-shortName"
            class="form-control"
            value={shortNameValue}
            onKeyUp={shortNameKeyUp}
          >
            Short name
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <SmartStringDiv style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={descriptionInput}>
          <SmartStringTextArea
            id="edit-description"
            class="form-control"
            value={descriptionValue}
            onKeyUp={descriptionKeyUp}
            rows={3}
            typeId="SmartStringTextArea"
          >
            Description
          </SmartStringTextArea>
        </SmartStringDiv>
        <SmartStringDiv style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={summaryDescriptionInput}>
          <SmartStringInputDeprecated
            id="edit-summaryDescription"
            class="form-control"
            value={summaryDescriptionValue}
            onKeyUp={summaryDescriptionKeyUp}
          >
            Summary description
          </SmartStringInputDeprecated>
        </SmartStringDiv>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <label for="edit-repeatsWhile">Repeats while</label>
          <input
            id="edit-repeatsWhile"
            class="form-control"
            value={repeatsWhileValue}
            ref={repeatsWhileInput}
            onKeyUp={repeatsWhileKeyUp}
          />
        </div>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <label for="edit-repeatsUntil">Repeats until</label>
          <input
            id="edit-repeatsUntil"
            class="form-control"
            value={repeatsUntilValue}
            ref={repeatsUntilInput}
            onKeyUp={repeatsUntilKeyUp}
          />
        </div>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <label for="edit-pageIdToDisplayAfterRemove">PageId to display after remove</label>
          <input
            id="edit-pageIdToDisplayAfterRemove"
            class="form-control"
            value={pageIdToDisplayAfterRemoveValue}
            ref={pageIdToDisplayAfterRemoveInput}
            onKeyUp={pageIdToDisplayAfterRemoveKeyUp}
          />
        </div>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }} ref={presentationHintContainer}>
          Display in summary:
          <input type="radio" id="yes" checked={yesChecked} onClick={yesToggle} />
          <label for="yes">Yes</label>
          <input type="radio" id="no" checked={noChecked} onClick={noToggle} />
          <label for="no">No</label>
        </div>
        <hr style={{ display: moreOptionsDisplayed ? "block" : "none" }}></hr>
        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <div
            style={{
              display: defaultPageExists ? "none" : "flex",
              flexDirection: "row",
              alignItems: "center",
              justifyContent: "space-between",
            }}
          >
            <div>No default page</div>
            <button id="add-defaultPage-button" class="btn btn-success" onClick={addDefaultPage}>
              ↻ Add default page
            </button>
          </div>
          <div
            style={{
              display: defaultPageExists ? "flex" : "none",
              flexDirection: "row",
              alignItems: "center",
              justifyContent: "space-between",
            }}
          >
            <div>✔ Showing default page</div>
            <button id="delete-defaultPage-button" class="btn btn-danger" onClick={deleteDefaultPage}>
              ↺ Delete default page
            </button>
          </div>
        </div>

        <div style={{ display: moreOptionsDisplayed ? "block" : "none" }}>
          <div
            style={{
              display: cyaPageExists ? "none" : "flex",
              flexDirection: "row",
              alignItems: "center",
              justifyContent: "space-between",
            }}
          >
            <div>No summary page for each item</div>
            <button id="add-cyaPage-button" class="btn btn-success" onClick={addCyaPage}>
              ↻ Add cya page
            </button>
          </div>
          <div
            style={{
              display: cyaPageExists ? "flex" : "none",
              flexDirection: "row",
              alignItems: "center",
              justifyContent: "space-between",
            }}
          >
            <div>✔ Displaying summary page for each item</div>
            <button id="delete-cyaPage-button" class="btn btn-danger" onClick={deleteCyaPage}>
              ↺ Delete cya page
            </button>
          </div>
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
          Add to list options
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
