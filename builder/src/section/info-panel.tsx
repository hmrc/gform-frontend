import type {
  ServerPageData,
  FormComponentPart,
  ContentScriptRequest,
  FormComponent,
  DispatchEvent,
  FormComponentUpdateRequest,
  SectionNumber,
  InfoState,
  SmartStringEvent,
  SmartString,
} from "../types";
import { DispatchTarget, InfoUpdateEvent, FieldInteractionType, MessageKind, FieldVisibility } from "../types";
import { useContext } from "preact/hooks";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { useUpdateComponent } from "./hooks/useUpdateComponent";
import { updateSmartString, SmartStringInput } from "./smart-string";
import { replaceUndefinedByEmptyString } from "./pure-functions";

export const infoReducer = (state: InfoState, action: DispatchEvent<InfoUpdateEvent>): InfoState => {
  const record = action.record;
  switch (action.subtype) {
    case InfoUpdateEvent.Label:
      {
        const label = updateSmartString(record.content as SmartStringEvent, state.label);
        return { ...state, label };
      }
      break;
    case InfoUpdateEvent.InfoText:
      {
        const infoText = updateSmartString(record.content as SmartStringEvent, state.infoText);
        return { ...state, infoText };
      }
      break;
    case InfoUpdateEvent.InfoType:
      {
        return { ...state, infoType: record.content };
      }
      break;
    default:
      return state;
  }
};

export const initialInfoState = (formComponent: FormComponent): InfoState => {
  const state: InfoState = {
    label: formComponent.label,
    infoText: formComponent.infoText,
    infoType: formComponent.infoType,
  };

  const undo = replaceUndefinedByEmptyString(state);

  return { ...state, undo: undo };
};

export interface InfoProps {
  inputFormComponentId: string;
  interactionType: FieldInteractionType;
  executeRequest: (updateRequest: ContentScriptRequest) => void;
  infoState: InfoState;
}

export const InfoPanelFactory =
  (
    host: string,
    formTemplateId: string,
    formComponent: FormComponent,
    serverPageData: ServerPageData,
    sectionNumber: SectionNumber,
    maybeAccessCode: string | null,
  ) =>
  ({ inputFormComponentId, interactionType, executeRequest, infoState }: InfoProps) => {
    const visibility = useContext(VisibilityContext);
    const textDispatch = useContext(StateContext);

    const dispatch = (subtype: InfoUpdateEvent, content: any, record: Record<string, any> = {}): void => {
      textDispatch({
        type: DispatchTarget.Component,
        subtype: subtype,
        record: { ...record, content: content },
      });
    };

    const refreshInfoComponent = (state: InfoState) => {
      const id = formComponent.id;

      const formComponentPart: FormComponentPart = {
        id,
      };

      formComponentPart["label"] = state.label;
      formComponentPart["infoText"] = state.infoText;
      formComponentPart["infoType"] = state.infoType;

      const data: FormComponentUpdateRequest = {
        payload: formComponentPart,
        atlIterationIndex: serverPageData.atlIterationIndex,
        sectionNumber: sectionNumber,
        maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
      };

      const updateRequest: ContentScriptRequest = {
        host: host,
        kind: MessageKind.UpdateFormComponent,
        formTemplateId: formTemplateId,
        data: data,
      };

      executeRequest(updateRequest);
    };

    useUpdateComponent(infoState, refreshInfoComponent);

    const onInfoTextChange = (e: SmartStringEvent) => {
      dispatch(InfoUpdateEvent.InfoText, e);
    };

    const onInfoTypeChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(InfoUpdateEvent.InfoType, input.value);
    };

    const onLabelChange = (e: SmartStringEvent) => {
      dispatch(InfoUpdateEvent.Label, e);
    };

    const showLabel = visibility.field === FieldVisibility.Label;
    const showInfoText = visibility.field === FieldVisibility.InfoText;
    const showAllFields = visibility.field === FieldVisibility.AllComponent;

    const isPanelVisible = showLabel || showInfoText || showAllFields;

    return (
      <fieldset hidden={!isPanelVisible} class="panel">
        <legend class="panel">Info component - id: {inputFormComponentId}</legend>
        <div hidden={!(showAllFields || showInfoText)}>
          <SmartStringInput id="edit-label" value={infoState.infoText} onKeyUp={onInfoTextChange} multiline={true}>
            Info text
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-shortName" value={infoState.label} onKeyUp={onLabelChange}>
            Label
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <label for="edit-format">Info Type</label>
          <select id="edit-format" class="form-control" value={infoState.infoType} onChange={onInfoTypeChange}>
            <option value="noformat">Text</option>
            <option value="standard">Inset text</option>
            <option value="long">Details</option>
            <option value="important">Warning text</option>
            <option value="banner">Banner</option>
          </select>
        </div>
      </fieldset>
    );
  };
