import * as styles from "bundle-text:./content.css";
import { Signal } from "@preact/signals";
import { useRef, useEffect, useReducer, useState } from "preact/hooks";
import { PropsWithChildren } from "preact/compat";
import { FunctionComponent } from "preact";
import type {
  State,
  InfoState,
  ContentScriptRequest,
  FormComponent,
  UpdateHtmlResponse,
  SmartString,
  StateParam,
  InfoLookupHelper,
} from "../types";
import { FieldInteractionType, ExclusiveFieldVisibility, FieldVisibility } from "../types";
import { findInfoElement } from "./info-component-helper";
import { initialSectionState, SectionProps } from "./section-panel";
import { infoReducer, initialInfoState, InfoProps } from "./info-panel";
import { removePreviousSiblings } from "../pure-functions";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { mkStateReducer, SetVisibility } from "./main-panel";
import { onMessageHandler } from "../background/index";

const stateReducer = mkStateReducer<InfoState>(infoReducer);

const initialState = (stateParam: StateParam): State<InfoState> => {
  return {
    section: initialSectionState(stateParam.section),
    component: initialInfoState(stateParam.formComponent),
  };
};

interface InfoClickHandler {
  captionClickHandler: (element: HTMLParagraphElement) => void;
  titleClickHandler: (element: HTMLHeadingElement | HTMLLabelElement) => void;
  descriptionClickHandler: (element: HTMLParagraphElement) => void;
  infoTextClickHandler: (element: HTMLDivElement | HTMLDetailsElement) => void;
}

const installEventHandlers = (
  inputFormComponentId: string,
  infoType: string | undefined,
  infoLookups: InfoLookupHelper,
  handlers: InfoClickHandler,
  form: HTMLFormElement,
  interactionType: FieldInteractionType,
) => {
  const formParentEl: HTMLElement | null = form.parentElement;

  if (formParentEl !== null) {
    const captionEl = formParentEl.querySelector<HTMLParagraphElement>(".hmrc-caption");
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

  const infoSelectorAndIndex = findInfoElement(inputFormComponentId, infoType, infoLookups);

  if (infoSelectorAndIndex !== undefined) {
    const infoElements: NodeListOf<Element> = document.querySelectorAll(infoSelectorAndIndex.selector);
    const infoComponent = infoElements.item(infoSelectorAndIndex.index);

    if (infoComponent instanceof HTMLDivElement || infoComponent instanceof HTMLDetailsElement) {
      handlers.infoTextClickHandler(infoComponent);
    }
  }
};

export const InfoControllerFactory =
  (
    infoLookupHelper: InfoLookupHelper,
    inputFormComponentId: string,
    formComponent: FormComponent,
    stateParam: StateParam,
    interactionType: FieldInteractionType,
    host: string,
    formTemplateId: string,
    form: HTMLFormElement,
    titleSignal: Signal<SmartString>,
    MainPanel: FunctionComponent<PropsWithChildren<SetVisibility>>,
    SectionPanel: FunctionComponent<SectionProps>,
    InfoPanel: FunctionComponent<InfoProps>,
  ) =>
  () => {
    // We need infoType info when installing DOM event handlers,
    // but we don't want to know text state here.
    // Let's remember latest infoType which was send to server instead.
    const infoType = useRef<string | undefined>(formComponent.infoType);

    useEffect(() => {
      installEventHandlers(inputFormComponentId, infoType.current, infoLookupHelper, handlers, form, interactionType);
    }, []);

    const refreshComponentHtml = (componentHtml: string): void => {
      const infoSelectorAndIndex = findInfoElement(inputFormComponentId, infoType.current, infoLookupHelper);

      if (infoSelectorAndIndex !== undefined) {
        const infoElements: NodeListOf<Element> = document.querySelectorAll(infoSelectorAndIndex.selector);
        const oldInfoComponent = infoElements.item(infoSelectorAndIndex.index);

        let isOpen = false;

        if (oldInfoComponent instanceof HTMLDetailsElement) {
          isOpen = oldInfoComponent.open;
        }

        if (oldInfoComponent instanceof HTMLDivElement || oldInfoComponent instanceof HTMLDetailsElement) {
          oldInfoComponent.insertAdjacentHTML("beforebegin", componentHtml);
          oldInfoComponent.remove();
        }

        if (isOpen) {
          const infoElements: NodeListOf<Element> = document.querySelectorAll(infoSelectorAndIndex.selector);
          const newInfoComponent = infoElements.item(infoSelectorAndIndex.index);
          if (newInfoComponent instanceof HTMLDetailsElement) {
            newInfoComponent.open = true;
          }
        }
      }
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

    const registerDescriptionClickHandler = (element: HTMLParagraphElement) => {
      element.addEventListener("click", (event) => {
        setVisibility({ field: FieldVisibility.Description });
      });
    };

    const registerInfoTextClickHandler = (element: HTMLDivElement | HTMLDetailsElement) => {
      element.addEventListener("click", (event) => {
        setVisibility({ field: FieldVisibility.InfoText });
      });
    };

    const handlers: InfoClickHandler = {
      captionClickHandler: registerCaptionClickHandler,
      titleClickHandler: registerTitleClickHandler,
      descriptionClickHandler: registerDescriptionClickHandler,
      infoTextClickHandler: registerInfoTextClickHandler,
    };

    const serverError = useRef<HTMLDivElement>(null);

    const [visibility, setVisibility] = useState<ExclusiveFieldVisibility>({ field: FieldVisibility.None });
    const [state, setState] = useReducer(stateReducer, stateParam, initialState);

    const executeRequest = (updateRequest: ContentScriptRequest) => {
      onMessageHandler<UpdateHtmlResponse>(updateRequest, (response) => {
        if (response.html !== undefined) {
          removePreviousSiblings(form);
          if (response.sectionHtml !== undefined) {
            form.insertAdjacentHTML("beforebegin", response.sectionHtml);
          }
          refreshComponentHtml(response.html);
          serverError.current?.classList.add("hidden");
          if (updateRequest.data?.payload?.infoType) {
            infoType.current = updateRequest.data.payload.infoType;
          }
          installEventHandlers(
            inputFormComponentId,
            infoType.current,
            infoLookupHelper,
            handlers,
            form,
            interactionType,
          );
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
            <InfoPanel
              inputFormComponentId={inputFormComponentId}
              interactionType={interactionType}
              executeRequest={executeRequest}
              infoState={state.component}
            />
          </MainPanel>
        </StateContext.Provider>
      </VisibilityContext.Provider>
    );
  };
