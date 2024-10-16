import { Signal } from "@preact/signals";
import { useContext } from "preact/hooks";
import type {
  ServerPageData,
  SectionNumber,
  SmartString,
  DateState,
  DispatchEvent,
  FormComponent,
  ContentScriptRequest,
  SmartStringEvent,
} from "../types";
import {
  MessageKind,
  FormComponentPart,
  FieldInteractionType,
  FieldVisibility,
  DateUpdateEvent,
  DispatchTarget,
  FormComponentUpdateRequest,
} from "../types";
import { replaceUndefinedByEmptyString, isOptional } from "../pure-functions";
import { updateSmartString, SmartStringInput } from "./smart-string";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { useUpdateComponent } from "./hooks/useUpdateComponent";

export const dateReducer = (state: DateState, action: DispatchEvent<DateUpdateEvent>): DateState => {
  const record = action.record;
  switch (action.subtype) {
    case DateUpdateEvent.Label:
      const label = updateSmartString(record.content as SmartStringEvent, state.label);
      return { ...state, label };
    case DateUpdateEvent.PageHeading:
      return {
        ...state,
        pageHeading: record.content,
        label: record.label,
      };
    case DateUpdateEvent.ShortName:
      const shortName = updateSmartString(record.content as SmartStringEvent, state.shortName);
      return { ...state, shortName };
    case DateUpdateEvent.HelpText:
      const helpText = updateSmartString(record.content as SmartStringEvent, state.helpText);
      return { ...state, helpText };
    case DateUpdateEvent.Format:
      return { ...state, format: record.content };
    case DateUpdateEvent.ErrorShortName:
      const errorShortName = updateSmartString(record.content as SmartStringEvent, state.errorShortName);
      return { ...state, errorShortName };
    case DateUpdateEvent.ErrorShortNameStart:
      const errorShortNameStart = updateSmartString(record.content as SmartStringEvent, state.errorShortNameStart);
      return { ...state, errorShortNameStart };
    case DateUpdateEvent.ErrorExample:
      const errorExample = updateSmartString(record.content as SmartStringEvent, state.errorExample);
      return { ...state, errorExample };
    case DateUpdateEvent.ErrorMessage:
      const errorMessage = updateSmartString(record.content as SmartStringEvent, state.errorMessage);
      return { ...state, errorMessage };
    case DateUpdateEvent.LabelSize:
      return { ...state, labelSize: record.content };
    default:
      return state;
  }
};

export const initialDateState = (formComponent: FormComponent): DateState => {
  const optional = isOptional(formComponent);
  const state: DateState = {
    label: formComponent.label,
    pageHeading: formComponent.label === undefined,
    helpText: formComponent.helpText,
    shortName: formComponent.shortName,
    format: formComponent.format,
    errorShortName: formComponent.errorShortName,
    errorShortNameStart: formComponent.errorShortNameStart,
    errorExample: formComponent.errorExample,
    errorMessage: formComponent.errorMessage,
    labelSize: formComponent.labelSize,
    optional: optional,
  };

  const undo = replaceUndefinedByEmptyString(state);

  return { ...state, undo: undo };
};

export interface DateProps {
  inputFormComponentId: string;
  interactionType: FieldInteractionType;
  executeRequest: (updateRequest: ContentScriptRequest) => void;
  titleSignal: Signal<SmartString>;
  dateState: DateState;
}

export const DatePanelFactory =
  (
    host: string,
    formTemplateId: string,
    formComponent: FormComponent,
    serverPageData: ServerPageData,
    sectionNumber: SectionNumber,
    maybeAccessCode: string | null,
  ) =>
  ({ inputFormComponentId, interactionType, executeRequest, titleSignal, dateState }: DateProps) => {
    const visibility = useContext(VisibilityContext);
    const stateDispatch = useContext(StateContext);

    const dispatch = (subtype: DateUpdateEvent, content: any, record: Record<string, any> = {}): void => {
      stateDispatch({
        type: DispatchTarget.Component,
        subtype: subtype,
        record: { ...record, content: content },
      });
    };

    const refreshDateComponent = (state: DateState) => {
      const id = formComponent.id;

      const formComponentPart: FormComponentPart = {
        id,
      };

      if (state.pageHeading === true) {
        formComponentPart["label"] = ""; // Empty value will be purged on the server
      } else {
        formComponentPart["label"] = state.label;
      }

      formComponentPart["helpText"] = state.helpText;
      formComponentPart["shortName"] = state.shortName;
      formComponentPart["format"] = state.format;
      formComponentPart["errorShortName"] = state.errorShortName;
      formComponentPart["errorShortNameStart"] = state.errorShortNameStart;
      formComponentPart["errorExample"] = state.errorExample;
      formComponentPart["errorMessage"] = state.errorMessage;
      formComponentPart["labelSize"] = state.labelSize;
      formComponentPart["mandatory"] = state.optional ? false : "";

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

    useUpdateComponent(dateState, refreshDateComponent);

    const onLabelChange = (e: SmartStringEvent) => {
      dispatch(DateUpdateEvent.Label, e);
    };

    const onH1FieldLabelToggle = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      const label = input.checked ? dateState.label : titleSignal.value;
      dispatch(DateUpdateEvent.PageHeading, input.checked, { label: label });
    };

    const onShortNameChange = (e: SmartStringEvent) => {
      dispatch(DateUpdateEvent.ShortName, e);
    };

    const onHelpTextChange = (e: SmartStringEvent) => {
      dispatch(DateUpdateEvent.HelpText, e);
    };

    const onFormatChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(DateUpdateEvent.Format, input.value);
    };

    const onErrorShortNameChange = (e: SmartStringEvent) => {
      dispatch(DateUpdateEvent.ErrorShortName, e);
    };

    const onErrorShortNameStartChange = (e: SmartStringEvent) => {
      dispatch(DateUpdateEvent.ErrorShortNameStart, e);
    };

    const onErrorExampleChange = (e: SmartStringEvent) => {
      dispatch(DateUpdateEvent.ErrorExample, e);
    };

    const onErrorMessageChange = (e: SmartStringEvent) => {
      dispatch(DateUpdateEvent.ErrorMessage, e);
    };

    const onLabelSizeChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(DateUpdateEvent.LabelSize, input.value);
    };

    const optionalToggle = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      dispatch(DateUpdateEvent.Optional, input.checked);
    };

    const showLabel = visibility.field === FieldVisibility.Label;
    const showHelpText = visibility.field === FieldVisibility.HelpText;
    const showAllFields = visibility.field === FieldVisibility.AllComponent;

    const isPanelVisible = showLabel || showHelpText || showAllFields;
    return (
      <fieldset hidden={!isPanelVisible} class="panel">
        <legend class="panel">Date component - id: {inputFormComponentId}</legend>
        <div hidden={!(showAllFields || showLabel)}>
          <SmartStringInput
            id="edit-label"
            value={dateState.label}
            onKeyUp={onLabelChange}
            disabled={dateState.pageHeading}
          >
            Label
          </SmartStringInput>
        </div>
        <div hidden={!(showAllFields && interactionType === FieldInteractionType.TitleLabelWithSync)}>
          <input type="checkbox" id="h1-checkbox" checked={dateState.pageHeading} onClick={onH1FieldLabelToggle} />
          <label for="h1-checkbox">Use field label as H1</label>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-shortName" value={dateState.shortName} onKeyUp={onShortNameChange}>
            Short name
          </SmartStringInput>
        </div>
        <div hidden={!(showAllFields || showHelpText)}>
          <SmartStringInput id="edit-helpText" value={dateState.helpText} onKeyUp={onHelpTextChange}>
            Help Text
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <label for="edit-format">Format</label>
          <input id="edit-format" class="form-control" value={dateState.format} onKeyUp={onFormatChange} />
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorShortName" value={dateState.errorShortName} onKeyUp={onErrorShortNameChange}>
            Error Short Name
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput
            id="edit-errorShortNameStart"
            value={dateState.errorShortNameStart}
            onKeyUp={onErrorShortNameStartChange}
          >
            Error Short Name Start
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorExample" value={dateState.errorExample} onKeyUp={onErrorExampleChange}>
            Error Example
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorMessage" value={dateState.errorMessage} onKeyUp={onErrorMessageChange}>
            Error message
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <label for="edit-labelSize">Label size</label>
          <select id="edit-labelSize" class="form-control" value={dateState.labelSize} onChange={onLabelSizeChange}>
            <option value="">Default</option>
            <option value="xs">xs - Very small</option>
            <option value="s">s - Small</option>
            <option value="m">m - Medium</option>
            <option value="l">l - Large</option>
            <option value="xl">xl - Very large</option>
          </select>
        </div>
        <div hidden={!showAllFields}>
          <input type="checkbox" id="optional" checked={dateState.optional} onClick={optionalToggle} />
          <label for="optional">Optional</label>
        </div>
      </fieldset>
    );
  };
