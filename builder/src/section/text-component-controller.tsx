import * as styles from "bundle-text:./content.css";
import { Signal } from "@preact/signals";
import { useRef, useEffect, useReducer, useState } from "preact/hooks";
import { PropsWithChildren } from "preact/compat";
import { FunctionComponent, createRef } from "preact";
import type {
  State,
  Section,
  SectionState,
  TextState,
  ContentScriptRequest,
  FormComponent,
  UpdateHtmlResponse,
  SmartString,
  StateParam,
  DispatchEvent,
} from "../types";
import {
  DispatchTarget,
  SectionUpdateEvent,
  FieldInteractionType,
  ExclusiveFieldVisibility,
  FieldVisibility,
} from "../types";
import { mkTextClickable } from "./text-helper";
import { initialSectionState, SectionProps } from "./section-panel";
import { textReducer, initialTextState, TextProps } from "./text-panel";
import { ErrorReportLink } from "./error-report-component";
import { removePreviousSiblings } from "../pure-functions";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { mkStateReducer, SetVisibility } from "./main-panel";
import { onMessageHandler } from "../background/index";

const stateReducer = mkStateReducer<TextState>(textReducer);

const initialState = (stateParam: StateParam): State<TextState> => {
  return {
    section: initialSectionState(stateParam.section),
    component: initialTextState(stateParam.formComponent),
  };
};

interface TextClickHandler {
  captionClickHandler: (element: HTMLParagraphElement) => void;
  titleClickHandler: (element: HTMLHeadingElement | HTMLLabelElement) => void;
  labelClickHandler: (element: HTMLLabelElement) => void;
  descriptionClickHandler: (element: HTMLParagraphElement) => void;
  helpTextClickHandler: (element: HTMLDivElement) => void;
}

const installEventHandlers = (
  sectionTitleEqFieldLabel: boolean,
  inputFormComponentId: string,
  handlers: TextClickHandler,
  form: HTMLFormElement,
  format: string | undefined,
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

  const textClickable = mkTextClickable(inputFormComponentId, sectionTitleEqFieldLabel, form, format);

  if (textClickable !== undefined) {
    if (textClickable.helpTextEl !== null) {
      handlers.helpTextClickHandler(textClickable.helpTextEl);
    }

    if (textClickable.labelEl !== null) {
      if (sectionTitleEqFieldLabel) {
        handlers.titleClickHandler(textClickable.labelEl);
      } else {
        handlers.labelClickHandler(textClickable.labelEl);
      }
    }
  }
};

export const TextComponentControllerFactory =
  (
    inputFormComponentId: string,
    currentSection: Section,
    formComponent: FormComponent,
    stateParam: StateParam,
    interactionType: FieldInteractionType,
    host: string,
    formTemplateId: string,
    form: HTMLFormElement,
    titleSignal: Signal<SmartString>,
    MainPanel: FunctionComponent<PropsWithChildren<SetVisibility>>,
    SectionPanel: FunctionComponent<SectionProps>,
    TextPanel: FunctionComponent<TextProps>,
  ) =>
  () => {
    // We need format info when installing DOM event handlers,
    // but we don't want to know text state here.
    // Let's remember latest format which was send to server instead.
    const format = useRef<string | undefined>(formComponent.format);

    // This holds information how is hmtl currently rendered wrapped / not wrapped
    const isHtmlPageHeading = useRef<boolean>(formComponent.label === undefined);

    useEffect(() => {
      installEventHandlers(
        isHtmlPageHeading.current,
        inputFormComponentId,
        handlers,
        form,
        formComponent.format,
        interactionType,
      );
    }, []);

    const refreshComponentHtml = (sectionTitleEqFieldLabel: boolean, componentHtml: string): void => {
      const textClickable = mkTextClickable(inputFormComponentId, isHtmlPageHeading.current, form, format.current);
      if (textClickable !== undefined && textClickable.parentEl !== null) {
        textClickable.parentEl.insertAdjacentHTML("beforebegin", componentHtml);
        textClickable.parentEl.remove();
        if (textClickable.characterCountMessage !== null) {
          textClickable.characterCountMessage.remove();
        }
      }
    };

    const serverError = useRef<HTMLDivElement>(null);

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

    const registerTitleLabelClickHandler = (element: HTMLLabelElement) => {
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

    const handlers: TextClickHandler = {
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
          refreshComponentHtml(sectionTitleEqFieldLabel, response.html);
          serverError.current?.classList.add("hidden");
          if (updateRequest.data?.payload?.format) {
            format.current = updateRequest.data.payload.format;
          }
          installEventHandlers(
            sectionTitleEqFieldLabel,
            inputFormComponentId,
            handlers,
            form,
            format.current,
            interactionType,
          );
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
            <TextPanel
              inputFormComponentId={inputFormComponentId}
              interactionType={interactionType}
              executeRequest={executeRequest}
              titleSignal={titleSignal}
              textState={state.component}
            />
          </MainPanel>
        </StateContext.Provider>
      </VisibilityContext.Provider>
    );
  };
