import * as styles from "bundle-text:./content.css";
import { PropsWithChildren } from "preact/compat";
import { useContext } from "preact/hooks";
import { Ref, RefObject } from "preact";
import type { FormComponent, DispatchEvent, State } from "../types";
import { FieldInteractionType, ExclusiveFieldVisibility, FieldVisibility, DispatchTarget } from "../types";
import { ErrorReportLink } from "./error-report-component";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { sectionReducer } from "./section-panel";

export function mkStateReducer<T extends { undo?: T }>(
  componentReducer: (component: T, action: DispatchEvent<any>) => T,
) {
  return function (state: State<T>, action: DispatchEvent<any>): State<T> {
    switch (action.type) {
      case DispatchTarget.Section: {
        const updated = sectionReducer(state.section, action);
        return { ...state, section: updated };
      }
      case DispatchTarget.Component: {
        const updated = componentReducer(state.component, action);
        return { ...state, component: updated };
      }
      case DispatchTarget.Undo: {
        const newSectionState = state.section.undo || state.section;
        newSectionState.undo = structuredClone(state.section.undo);

        const newComponentState = state.component.undo || state.component;
        newComponentState.undo = structuredClone(state.component.undo);

        // This state modification can result in two server calls, if both section and component data has changed!!!
        return { section: newSectionState, component: newComponentState };
      }
    }
  };
}

export interface SetVisibility {
  setVisibility: (vis: ExclusiveFieldVisibility) => void;
  serverError: RefObject<HTMLDivElement>;
}

export const MainPanelFactory =
  (
    inputFormComponentId: string,
    formComponent: FormComponent,
    interactionType: FieldInteractionType,
    host: string,
    formTemplateId: string,
  ) =>
  ({ setVisibility, serverError, children }: PropsWithChildren<SetVisibility>) => {
    const visibility = useContext(VisibilityContext);
    const dispatch = useContext(StateContext);

    const closeHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      setVisibility({ field: FieldVisibility.None });
    };

    const componentOptionsHandler = (e: MouseEvent) => {
      setVisibility({ field: FieldVisibility.AllComponent });
    };

    const pageOptionsHandler = (e: MouseEvent) => {
      setVisibility({ field: FieldVisibility.AllSection });
    };

    const undoHandler = (e: MouseEvent) => {
      dispatch({ type: DispatchTarget.Undo, subtype: null, record: {} });
    };

    const showPanel = visibility.field !== FieldVisibility.None;
    const showComponentPanel = visibility.field !== FieldVisibility.AllSection;
    const showSectionPanel = visibility.field !== FieldVisibility.AllComponent;

    return (
      <div id="main-panel" class="info" style={{ display: showPanel ? "block" : "none" }}>
        <style>{styles}</style>
        {children}
        <button id="close-button" class="btn btn-success" onClick={closeHandler}>
          ✓ Close
        </button>
        <button id="undo-button" class="btn btn-secondary" onClick={undoHandler}>
          ↻ Undo
        </button>
        <button
          id="field-options-button"
          class="btn btn-link"
          onClick={componentOptionsHandler}
          style={{ display: showSectionPanel ? "inline" : "none" }}
        >
          Field options
        </button>
        <button
          id="page-options-button"
          class="btn btn-link"
          onClick={pageOptionsHandler}
          style={{
            display: interactionType !== FieldInteractionType.ComponentOnly && showComponentPanel ? "inline" : "none",
          }}
        >
          Page options
        </button>
        <ErrorReportLink host={host} formTemplateId={formTemplateId} formComponentId={formComponent.id} />
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
