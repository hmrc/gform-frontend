import { Signal } from "@preact/signals";
import { PropsWithChildren } from "preact/compat";
import { useEffect, useRef, useReducer, useState } from "preact/hooks";
import { createRef } from "preact";
import { FunctionComponent } from "preact";
import type {
  ContentScriptRequest,
  FormComponent,
  UpdateHtmlResponse,
  ExclusiveFieldVisibility,
  DateClickable,
  FileState,
  StateParam,
  State,
  FileUploadClickable,
} from "../types";
import { FieldVisibility, SmartString, FieldInteractionType } from "../types";
import { mkFileUploadClickable } from "./fileupload-helper";
import { removePreviousSiblings } from "./pure-functions";
import { initialSectionState, SectionProps } from "./section-panel";
import { initialFileState, fileReducer, FileProps } from "./file-panel";
import { mkStateReducer, SetVisibility } from "./main-panel";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { onMessageHandler } from "../background/index";

const stateReducer = mkStateReducer<FileState>(fileReducer);

export const initialState = (stateParam: StateParam): State<FileState> => {
  return {
    section: initialSectionState(stateParam.section),
    component: initialFileState(stateParam.formComponent),
  };
};

interface FileClickHandler {
  captionClickHandler: (element: HTMLParagraphElement) => void;
  titleClickHandler: (element: HTMLHeadingElement | HTMLLabelElement) => void;
  labelClickHandler: (element: HTMLLabelElement) => void;
  descriptionClickHandler: (element: HTMLParagraphElement) => void;
  helpTextClickHandler: (element: HTMLDivElement) => void;
}

const installEventHandlers = (
  sectionTitleEqFieldLabel: boolean,
  inputFormComponentId: string,
  handlers: FileClickHandler,
  form: HTMLFormElement,
  interactionType: FieldInteractionType,
) => {
  const formParentEl: HTMLElement | null = form.parentElement;

  if (formParentEl !== null) {
    const captionSelector = sectionTitleEqFieldLabel ? ".govuk-caption-l" : ".hmrc-caption";
    const captionEl = formParentEl.querySelector<HTMLParagraphElement>(captionSelector);
    const headingEl = document.querySelector<HTMLHeadingElement>(".govuk-heading-l");
    const descriptionEl: HTMLParagraphElement | null =
      formParentEl.querySelector<HTMLParagraphElement>(".hmrc-page-heading + p");

    if (interactionType !== FieldInteractionType.ComponentOnly) {
      if (captionEl !== null) {
        handlers.captionClickHandler(captionEl);
      }

      if (headingEl !== null) {
        handlers.titleClickHandler(headingEl);
      }

      if (descriptionEl !== null) {
        handlers.descriptionClickHandler(descriptionEl);
      }
    }
  }

  const fileClickable: FileUploadClickable | undefined = mkFileUploadClickable(
    inputFormComponentId,
    sectionTitleEqFieldLabel,
    form,
  );

  if (fileClickable !== undefined) {
    if (sectionTitleEqFieldLabel) {
      handlers.titleClickHandler(fileClickable.labelEl);
    } else {
      handlers.labelClickHandler(fileClickable.labelEl);
    }
    if (fileClickable.helpTextEl !== null) {
      handlers.helpTextClickHandler(fileClickable.helpTextEl);
    }
  }
};

export const FileUploadComponentControllerFactory =
  (
    inputFormComponentId: string,
    formComponent: FormComponent,
    stateParam: StateParam,
    interactionType: FieldInteractionType,
    form: HTMLFormElement,
    fileUploadClickable: FileUploadClickable,
    titleSignal: Signal<SmartString>,
    MainPanel: FunctionComponent<PropsWithChildren<SetVisibility>>,
    SectionPanel: FunctionComponent<SectionProps>,
    FilePanel: FunctionComponent<FileProps>,
  ) =>
  () => {
    // This holds information how is hmtl currently rendered wrapped / not wrapped
    const isHtmlPageHeading = useRef<boolean>(formComponent.label === undefined);

    useEffect(() => {
      installEventHandlers(isHtmlPageHeading.current, inputFormComponentId, handlers, form, interactionType);
    }, []);

    const refreshComponentHtml = (componentHtml: string): void => {
      const fileClickable: FileUploadClickable | undefined = mkFileUploadClickable(
        inputFormComponentId,
        isHtmlPageHeading.current,
        form,
      );

      if (fileClickable !== undefined) {
        fileClickable.parentEl.insertAdjacentHTML("beforebegin", componentHtml);
        fileClickable.parentEl.remove();
      }

      // Remove all gray "Upload file" buttons
      const optionalButtons: NodeListOf<HTMLElement> = document.getElementsByName(
        `${inputFormComponentId}-uploadButton`,
      );
      optionalButtons.forEach((button) => button.remove());
    };

    const serverError = createRef<HTMLDivElement>();
    const [visibility, setVisibility] = useState<ExclusiveFieldVisibility>({ field: FieldVisibility.None });
    const [state, setState] = useReducer(stateReducer, stateParam, initialState);

    const registerCaptionClickHandler = (element: HTMLParagraphElement) => {
      element.addEventListener("click", (event) => {
        setVisibility({ field: FieldVisibility.Caption });
      });
    };

    const registerTitleClickHandler = (element: HTMLHeadingElement | HTMLLabelElement) => {
      element.addEventListener("click", (event) => {
        setVisibility({ field: FieldVisibility.Title });
      });
    };

    const registerHelpTextClickHandler = (element: HTMLDivElement) => {
      element.addEventListener("click", (event) => {
        setVisibility({ field: FieldVisibility.HelpText });
      });
    };

    const registerDescriptionClickHandler = (element: HTMLParagraphElement) => {
      element.addEventListener("click", (event) => {
        setVisibility({ field: FieldVisibility.Description });
      });
    };

    const registerLabelClickHandler = (element: HTMLLabelElement) => {
      element.addEventListener("click", (event) => {
        setVisibility({ field: FieldVisibility.Label });
      });
    };

    const handlers: FileClickHandler = {
      captionClickHandler: registerCaptionClickHandler,
      titleClickHandler: registerTitleClickHandler,
      labelClickHandler: registerLabelClickHandler,
      descriptionClickHandler: registerDescriptionClickHandler,
      helpTextClickHandler: registerHelpTextClickHandler,
    };

    const executeRequest = (updateRequest: ContentScriptRequest) => {
      onMessageHandler<UpdateHtmlResponse>(updateRequest, (response) => {
        if (response.html !== undefined) {
          const sectionTitleEqFieldLabel = response.formLevelHeading || false;

          removePreviousSiblings(form);
          if (response.sectionHtml !== undefined) {
            form.insertAdjacentHTML("beforebegin", response.sectionHtml);
          }

          refreshComponentHtml(response.html);
          serverError.current?.classList.add("hidden");

          installEventHandlers(sectionTitleEqFieldLabel, inputFormComponentId, handlers, form, interactionType);
          isHtmlPageHeading.current = sectionTitleEqFieldLabel;
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
      <VisibilityContext.Provider value={visibility}>
        <StateContext.Provider value={setState}>
          <MainPanel setVisibility={setVisibility} serverError={serverError}>
            <SectionPanel executeRequest={executeRequest} titleSignal={titleSignal} sectionState={state.section} />
            <FilePanel
              inputFormComponentId={inputFormComponentId}
              interactionType={interactionType}
              executeRequest={executeRequest}
              titleSignal={titleSignal}
              fileState={state.component}
            />
          </MainPanel>
        </StateContext.Provider>
      </VisibilityContext.Provider>
    );
  };
