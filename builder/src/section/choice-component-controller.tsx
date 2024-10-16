import { Signal } from "@preact/signals";
import { FunctionComponent } from "preact";
import { PropsWithChildren } from "preact/compat";
import { useState, useEffect, useReducer, useRef } from "preact/hooks";
import type {
  ContentScriptRequest,
  FormComponent,
  FormComponentId,
  UpdateHtmlResponse,
  ChoiceClickable,
  SectionNumber,
  ChoiceState,
  State,
  StateParam,
} from "../types";
import { FieldInteractionType, ExclusiveFieldVisibility, FieldVisibility, SmartString } from "../types";
import { mkChoiceClickable } from "./choice-helper";
import { removePreviousSiblings } from "../pure-functions";
import { initialSectionState, SectionProps } from "./section-panel";
import { choiceReducer, initialChoiceState, ChoiceProps } from "./choice-panel";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { mkStateReducer, SetVisibility } from "./main-panel";
import { onMessageHandler } from "../background/index";

interface ChoiceClickHandler {
  captionClickHandler: (element: HTMLParagraphElement) => void;
  titleClickHandler: (element: HTMLHeadingElement | HTMLLabelElement) => void;
  labelClickHandler: (element: HTMLLegendElement) => void;
  descriptionClickHandler: (element: HTMLParagraphElement) => void;
  helpTextClickHandler: (element: HTMLDivElement) => void;
  choiceClickHandler: (element: HTMLDivElement, index: number) => void;
}

const installEventHandlers = (
  sectionTitleEqFieldLabel: boolean,
  inputFormComponentId: string,
  handlers: ChoiceClickHandler,
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

  const choiceClickable: ChoiceClickable | undefined = mkChoiceClickable(inputFormComponentId);

  if (choiceClickable !== undefined) {
    if (sectionTitleEqFieldLabel) {
      handlers.titleClickHandler(choiceClickable.labelEl);
    } else {
      handlers.labelClickHandler(choiceClickable.labelEl);
    }
    choiceClickable.eachChoiceEl.forEach((value, index) => handlers.choiceClickHandler(value, index));
    if (choiceClickable.helpTextEl !== null) {
      handlers.helpTextClickHandler(choiceClickable.helpTextEl);
    }
  }
};

export const initialState = (stateParam: StateParam): State<ChoiceState> => {
  return {
    section: initialSectionState(stateParam.section),
    component: initialChoiceState(stateParam.formComponent),
  };
};

const stateReducer = mkStateReducer<ChoiceState>(choiceReducer);

export const ChoiceComponentControllerFactory =
  (
    inputFormComponentId: string,
    formComponent: FormComponent,
    stateParam: StateParam,
    interactionType: FieldInteractionType,
    host: string,
    formTemplateId: string,
    sectionNumber: SectionNumber,
    form: HTMLFormElement,
    titleSignal: Signal<SmartString>,
    MainPanel: FunctionComponent<PropsWithChildren<SetVisibility>>,
    SectionPanel: FunctionComponent<SectionProps>,
    ChoicePanel: FunctionComponent<ChoiceProps>,
  ) =>
  () => {
    // This holds information how is hmtl currently rendered wrapped / not wrapped
    const isHtmlPageHeading = useRef<boolean>(formComponent.label === undefined);

    useEffect(() => {
      installEventHandlers(isHtmlPageHeading.current, inputFormComponentId, handlers, form, interactionType);
    }, []);

    const refreshComponentHtml = (componentHtml: string, formComponentId: FormComponentId): void => {
      const choiceClickable: ChoiceClickable | undefined = mkChoiceClickable(inputFormComponentId);
      if (choiceClickable !== undefined) {
        choiceClickable.parentEl.insertAdjacentHTML("beforebegin", componentHtml);
        choiceClickable.parentEl.remove();
      }
    };

    const serverError = useRef<HTMLDivElement>(null);

    const [visibility, setVisibility] = useState<ExclusiveFieldVisibility>({ field: FieldVisibility.None });
    const [state, setState] = useReducer(stateReducer, stateParam, initialState);

    const componentOptionsHandler = (e: MouseEvent) => {
      setVisibility({ field: FieldVisibility.AllComponent });
    };

    const pageOptionsHandler = (e: MouseEvent) => {
      setVisibility({ field: FieldVisibility.AllSection });
    };

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

    const registerChoiceClickHandler = (element: HTMLDivElement, index: number) => {
      element.addEventListener("dblclick", (event) => {
        setVisibility({ field: FieldVisibility.Choice, index: index });
      });
    };

    const registerDescriptionClickHandler = (element: HTMLParagraphElement) => {
      element.addEventListener("click", (event) => {
        setVisibility({ field: FieldVisibility.Description });
      });
    };

    const registerLabelClickHandler = (element: HTMLLegendElement) => {
      element.addEventListener("click", (event) => {
        setVisibility({ field: FieldVisibility.Label });
      });
    };

    const handlers: ChoiceClickHandler = {
      captionClickHandler: registerCaptionClickHandler,
      titleClickHandler: registerTitleClickHandler,
      labelClickHandler: registerLabelClickHandler,
      descriptionClickHandler: registerDescriptionClickHandler,
      helpTextClickHandler: registerHelpTextClickHandler,
      choiceClickHandler: registerChoiceClickHandler,
    };

    const closeHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      setVisibility({ field: FieldVisibility.None });
    };

    const executeRequest = (updateRequest: ContentScriptRequest) => {
      onMessageHandler<UpdateHtmlResponse>(updateRequest, (response) => {
        if (response.html !== undefined) {
          const sectionTitleEqFieldLabel = response.formLevelHeading || false;

          removePreviousSiblings(form);
          if (response.sectionHtml !== undefined) {
            form.insertAdjacentHTML("beforebegin", response.sectionHtml);
          }

          refreshComponentHtml(response.html, inputFormComponentId);
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

    const showPanel = visibility.field !== FieldVisibility.None;
    const showComponentPanel = visibility.field !== FieldVisibility.AllSection;
    const showSectionPanel = visibility.field !== FieldVisibility.AllComponent;

    return (
      <VisibilityContext.Provider value={visibility}>
        <StateContext.Provider value={setState}>
          <MainPanel setVisibility={setVisibility} serverError={serverError}>
            <SectionPanel executeRequest={executeRequest} titleSignal={titleSignal} sectionState={state.section} />
            <ChoicePanel
              formComponentId={formComponent.id}
              inputFormComponentId={inputFormComponentId}
              interactionType={interactionType}
              executeRequest={executeRequest}
              titleSignal={titleSignal}
              choiceState={state.component}
            />
          </MainPanel>
        </StateContext.Provider>
      </VisibilityContext.Provider>
    );
  };
