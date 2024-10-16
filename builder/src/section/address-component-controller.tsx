import { Signal } from "@preact/signals";
import { PropsWithChildren } from "preact/compat";
import { useEffect, useRef, useReducer, useState } from "preact/hooks";
import { FunctionComponent } from "preact";
import type {
  AddressClickable,
  ContentScriptRequest,
  FormComponent,
  UpdateHtmlResponse,
  ExclusiveFieldVisibility,
  AddressState,
  StateParam,
  State,
} from "../types";
import { FieldVisibility, SmartString, FieldInteractionType } from "../types";
import { removePreviousSiblings } from "../pure-functions";
import { initialSectionState, SectionProps } from "./section-panel";
import { initialAddressState, addressReducer, AddressProps } from "./address-panel";
import { mkStateReducer, SetVisibility } from "./main-panel";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { mkAddressClickable } from "./address-helper";
import { onMessageHandler } from "../background/index";

const stateReducer = mkStateReducer<AddressState>(addressReducer);

export const initialState = (stateParam: StateParam): State<AddressState> => {
  return {
    section: initialSectionState(stateParam.section),
    component: initialAddressState(stateParam.formComponent),
  };
};

interface AddressClickHandler {
  captionClickHandler: (element: HTMLParagraphElement) => void;
  titleClickHandler: (element: HTMLHeadingElement | HTMLLabelElement) => void;
  labelClickHandler: (element: HTMLLegendElement) => void;
  descriptionClickHandler: (element: HTMLParagraphElement) => void;
  helpTextClickHandler: (element: HTMLDivElement) => void;
}

const installEventHandlers = (
  sectionTitleEqFieldLabel: boolean,
  inputFormComponentId: string,
  handlers: AddressClickHandler,
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

  const addressClickable: AddressClickable | undefined = mkAddressClickable(inputFormComponentId);

  if (addressClickable !== undefined) {
    if (sectionTitleEqFieldLabel) {
      handlers.titleClickHandler(addressClickable.labelEl);
    } else {
      handlers.labelClickHandler(addressClickable.labelEl);
    }
    if (addressClickable.helpTextEl !== null) {
      handlers.helpTextClickHandler(addressClickable.helpTextEl);
    }
  }
};

export const AddressComponentControllerFactory =
  (
    inputFormComponentId: string,
    formComponent: FormComponent,
    stateParam: StateParam,
    interactionType: FieldInteractionType,
    form: HTMLFormElement,
    addressClickable: AddressClickable,
    titleSignal: Signal<SmartString>,
    MainPanel: FunctionComponent<PropsWithChildren<SetVisibility>>,
    SectionPanel: FunctionComponent<SectionProps>,
    AddressPanel: FunctionComponent<AddressProps>,
  ) =>
  () => {
    // This holds information how is hmtl currently rendered wrapped / not wrapped
    const isHtmlPageHeading = useRef<boolean>(formComponent.label === undefined);

    useEffect(() => {
      installEventHandlers(isHtmlPageHeading.current, inputFormComponentId, handlers, form, interactionType);
    }, []);

    const refreshComponentHtml = (componentHtml: string): void => {
      const addressClickable: AddressClickable | undefined = mkAddressClickable(inputFormComponentId);
      if (addressClickable !== undefined) {
        addressClickable.parentEl.insertAdjacentHTML("beforebegin", componentHtml);
        addressClickable.parentEl.remove();
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

    const registerLabelClickHandler = (element: HTMLLegendElement) => {
      element.addEventListener("click", (event) => {
        setVisibility({ field: FieldVisibility.Label });
      });
    };

    const handlers: AddressClickHandler = {
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
            <AddressPanel
              inputFormComponentId={inputFormComponentId}
              interactionType={interactionType}
              executeRequest={executeRequest}
              titleSignal={titleSignal}
              addressState={state.component}
            />
          </MainPanel>
        </StateContext.Provider>
      </VisibilityContext.Provider>
    );
  };
