import { Signal } from "@preact/signals";
import { useContext } from "preact/hooks";
import type {
  ServerPageData,
  SectionNumber,
  SmartString,
  FileState,
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
  FileUpdateEvent,
  DispatchTarget,
  FormComponentUpdateRequest,
} from "../types";
import { replaceUndefinedByEmptyString, isOptional } from "./pure-functions";
import { updateSmartString, SmartStringInput } from "./smart-string";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { useUpdateComponent } from "./hooks/useUpdateComponent";

export const fileReducer = (state: FileState, action: DispatchEvent<FileUpdateEvent>): FileState => {
  const record = action.record;
  switch (action.subtype) {
    case FileUpdateEvent.Label:
      const label = updateSmartString(record.content as SmartStringEvent, state.label);
      return { ...state, label };
    case FileUpdateEvent.PageHeading:
      return {
        ...state,
        pageHeading: record.content,
        label: record.label,
      };
    case FileUpdateEvent.ShortName:
      const shortName = updateSmartString(record.content as SmartStringEvent, state.shortName);
      return { ...state, shortName };
    case FileUpdateEvent.HelpText:
      const helpText = updateSmartString(record.content as SmartStringEvent, state.helpText);
      return { ...state, helpText };
    case FileUpdateEvent.ErrorShortName:
      const errorShortName = updateSmartString(record.content as SmartStringEvent, state.errorShortName);
      return { ...state, errorShortName };
    case FileUpdateEvent.ErrorMessage:
      const errorMessage = updateSmartString(record.content as SmartStringEvent, state.errorMessage);
      return { ...state, errorMessage };
    case FileUpdateEvent.LabelSize:
      return { ...state, labelSize: record.content };
    case FileUpdateEvent.Optional:
      return { ...state, optional: record.content };
    default:
      return state;
  }
};

export const initialFileState = (formComponent: FormComponent): FileState => {
  const optional = isOptional(formComponent);
  const state: FileState = {
    label: formComponent.label,
    pageHeading: formComponent.label === undefined,
    helpText: formComponent.helpText,
    shortName: formComponent.shortName,
    errorShortName: formComponent.errorShortName,
    errorMessage: formComponent.errorMessage,
    labelSize: formComponent.labelSize,
    optional: optional,
  };

  const undo = replaceUndefinedByEmptyString(state);

  return { ...state, undo: undo };
};

export interface FileProps {
  inputFormComponentId: string;
  interactionType: FieldInteractionType;
  executeRequest: (upfileRequest: ContentScriptRequest) => void;
  titleSignal: Signal<SmartString>;
  fileState: FileState;
}

export const FilePanelFactory =
  (
    host: string,
    formTemplateId: string,
    formComponent: FormComponent,
    serverPageData: ServerPageData,
    sectionNumber: SectionNumber,
    maybeAccessCode: string | null,
  ) =>
  ({ inputFormComponentId, interactionType, executeRequest, titleSignal, fileState }: FileProps) => {
    const visibility = useContext(VisibilityContext);
    const stateDispatch = useContext(StateContext);

    const dispatch = (subtype: FileUpdateEvent, content: any, record: Record<string, any> = {}): void => {
      stateDispatch({
        type: DispatchTarget.Component,
        subtype: subtype,
        record: { ...record, content: content },
      });
    };

    const refreshFileComponent = (state: FileState) => {
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
      formComponentPart["errorShortName"] = state.errorShortName;
      formComponentPart["errorMessage"] = state.errorMessage;
      formComponentPart["labelSize"] = state.labelSize;
      formComponentPart["mandatory"] = state.optional ? "false" : "";

      const data: FormComponentUpdateRequest = {
        payload: formComponentPart,
        atlIterationIndex: serverPageData.atlIterationIndex,
        sectionNumber: sectionNumber,
        maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
      };

      const upfileRequest: ContentScriptRequest = {
        host: host,
        kind: MessageKind.UpdateFormComponent,
        formTemplateId: formTemplateId,
        data: data,
      };

      executeRequest(upfileRequest);
    };

    useUpdateComponent(fileState, refreshFileComponent);

    const onLabelChange = (e: SmartStringEvent) => {
      dispatch(FileUpdateEvent.Label, e);
    };

    const onH1FieldLabelToggle = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      const label = input.checked ? fileState.label : titleSignal.value;
      dispatch(FileUpdateEvent.PageHeading, input.checked, { label: label });
    };

    const onShortNameChange = (e: SmartStringEvent) => {
      dispatch(FileUpdateEvent.ShortName, e);
    };

    const onHelpTextChange = (e: SmartStringEvent) => {
      dispatch(FileUpdateEvent.HelpText, e);
    };

    const onErrorShortNameChange = (e: SmartStringEvent) => {
      dispatch(FileUpdateEvent.ErrorShortName, e);
    };

    const onErrorMessageChange = (e: SmartStringEvent) => {
      dispatch(FileUpdateEvent.ErrorMessage, e);
    };

    const onLabelSizeChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(FileUpdateEvent.LabelSize, input.value);
    };

    const optionalToggle = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      dispatch(FileUpdateEvent.Optional, input.checked);
    };

    const showLabel = visibility.field === FieldVisibility.Label;
    const showHelpText = visibility.field === FieldVisibility.HelpText;
    const showAllFields = visibility.field === FieldVisibility.AllComponent;

    const isPanelVisible = showLabel || showHelpText || showAllFields;

    return (
      <fieldset hidden={!isPanelVisible} class="panel">
        <legend class="panel">File component - id: {inputFormComponentId}</legend>
        <div hidden={!(showAllFields || showLabel)}>
          <SmartStringInput
            id="edit-label"
            value={fileState.label}
            onKeyUp={onLabelChange}
            disabled={fileState.pageHeading}
          >
            Label
          </SmartStringInput>
        </div>
        <div hidden={!(showAllFields && interactionType === FieldInteractionType.TitleLabelWithSync)}>
          <input type="checkbox" id="h1-checkbox" checked={fileState.pageHeading} onClick={onH1FieldLabelToggle} />
          <label for="h1-checkbox">Use field label as H1</label>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-shortName" value={fileState.shortName} onKeyUp={onShortNameChange}>
            Short name
          </SmartStringInput>
        </div>
        <div hidden={!(showAllFields || showHelpText)}>
          <SmartStringInput id="edit-helpText" value={fileState.helpText} onKeyUp={onHelpTextChange}>
            Help Text
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorShortName" value={fileState.errorShortName} onKeyUp={onErrorShortNameChange}>
            Error Short Name
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorMessage" value={fileState.errorMessage} onKeyUp={onErrorMessageChange}>
            Error message
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <label for="edit-labelSize">Label size</label>
          <select id="edit-labelSize" class="form-control" value={fileState.labelSize} onChange={onLabelSizeChange}>
            <option value="">Default</option>
            <option value="xs">xs - Very small</option>
            <option value="s">s - Small</option>
            <option value="m">m - Medium</option>
            <option value="l">l - Large</option>
            <option value="xl">xl - Very large</option>
          </select>
        </div>
        <div hidden={!showAllFields}>
          <input type="checkbox" id="optional" checked={fileState.optional} onClick={optionalToggle} />
          <label for="optional">Optional</label>
        </div>
      </fieldset>
    );
  };
