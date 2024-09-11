import { Signal } from "@preact/signals";

import type {
  ServerPageData,
  FormComponentPart,
  ContentScriptRequest,
  FormComponent,
  DispatchEvent,
  FormComponentUpdateRequest,
  SectionNumber,
  SmartStringEvent,
  SmartString,
  FormComponentId,
  ChoiceObject,
  ChoiceState,
} from "../types";
import {
  DispatchTarget,
  ChoiceUpdateEvent,
  FieldInteractionType,
  MessageKind,
  ChoiceComponentType,
  FieldVisibility,
} from "../types";
import { replaceUndefinedByEmptyString, isOptional, replaceWithEnglishValue } from "./pure-functions";
import { updateSmartString, SmartStringInput } from "./smart-string";
import { useContext } from "preact/hooks";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { useUpdateComponent } from "./hooks/useUpdateComponent";

const defaultChoices = ["Yes", "No"];

export const choiceReducer = (state: ChoiceState, action: DispatchEvent<ChoiceUpdateEvent>): ChoiceState => {
  const record = action.record;
  switch (action.subtype) {
    case ChoiceUpdateEvent.Type: {
      const choices =
        state.typeValue === ChoiceComponentType.YesNo
          ? record.content !== ChoiceComponentType.YesNo && state.undo !== undefined && state.undo.choices.length > 0
            ? structuredClone(state.undo.choices) // When transition from Checkboxes/Radios -> YesNo -> Checkboxes/Radios we restore original choices
            : defaultChoices
          : state.choices;
      return { ...state, typeValue: record.content, choices: choices };
    }
    case ChoiceUpdateEvent.Label:
      const label = updateSmartString(record.content as SmartStringEvent, state.label);
      return { ...state, label };
    case ChoiceUpdateEvent.PageHeading:
      return { ...state, pageHeading: record.content, label: record.label };
    case ChoiceUpdateEvent.HelpText:
      const helpText = updateSmartString(record.content as SmartStringEvent, state.helpText);
      return { ...state, helpText };
    case ChoiceUpdateEvent.ShortName:
      const shortName = updateSmartString(record.content as SmartStringEvent, state.shortName);
      return { ...state, shortName };
    case ChoiceUpdateEvent.Choice: {
      const index = record.index;
      const choices = state.choices;
      choices[index] = record.content;
      return { ...state, choices: choices };
    }
    case ChoiceUpdateEvent.Hint: {
      const index = record.index;
      const hints = state.hints;
      hints[index] = record.content;
      return { ...state, hints: hints };
    }
    case ChoiceUpdateEvent.IncludeIf: {
      const index = record.index;
      const includeIfs = state.includeIfs;
      includeIfs[index] = record.content;
      return { ...state, includeIfs: includeIfs };
    }
    case ChoiceUpdateEvent.DividerPosition:
      return { ...state, dividerPosition: record.content === true ? record.index : "" };
    case ChoiceUpdateEvent.DividerText:
      const dividerText = updateSmartString(record.content as SmartStringEvent, state.dividerText);
      return { ...state, dividerText };
    case ChoiceUpdateEvent.NoneChoice: {
      const index = record.index;
      const value = state.values[index - 1]; // Beware, None choice can be an index or a name of one of the values
      const noneChoice = value.length > 0 ? value : record.index;
      return { ...state, noneChoice: record.content === true ? noneChoice : "" };
    }
    case ChoiceUpdateEvent.NoneChoiceError:
      const noneChoiceError = updateSmartString(record.content as SmartStringEvent, state.noneChoiceError);
      return { ...state, noneChoiceError };
    case ChoiceUpdateEvent.ErrorShortName:
      const errorShortName = updateSmartString(record.content as SmartStringEvent, state.errorShortName);
      return { ...state, errorShortName };
    case ChoiceUpdateEvent.ErrorMessage:
      const errorMessage = updateSmartString(record.content as SmartStringEvent, state.errorMessage);
      return { ...state, errorMessage };
    case ChoiceUpdateEvent.Optional:
      return { ...state, optional: record.content };
    default:
      return state;
  }
};

export interface ChoiceProps {
  formComponentId: FormComponentId;
  inputFormComponentId: string;
  interactionType: FieldInteractionType;
  executeRequest: (updateRequest: ContentScriptRequest) => void;
  titleSignal: Signal<SmartString>;
  choiceState: ChoiceState;
}

const getChoices = (formComponent: FormComponent): string[] => {
  if (formComponent.choices instanceof Array) {
    return formComponent.choices
      .map((choice) => (typeof choice === "string" ? choice : choice.en))
      .map((choice) => (choice === "…" ? "" : choice));
  } else {
    return [];
  }
};

const getChoiceHints = (formComponent: FormComponent): string[] => {
  if (formComponent.choices instanceof Array) {
    const standaloneHints: string | string[] | undefined = formComponent.hints;
    return formComponent.choices.map((choice, index) => {
      const standaloneHint = standaloneHints instanceof Array ? standaloneHints[index] : undefined;
      return typeof standaloneHint === "string"
        ? standaloneHint
        : typeof choice === "string"
          ? "" // choice is only text
          : replaceWithEnglishValue(choice.hint);
    });
  } else {
    return [];
  }
};

const getChoiceValues = (formComponent: FormComponent): string[] => {
  if (formComponent.choices instanceof Array) {
    return formComponent.choices.map((choice) => (typeof choice === "string" ? "" : choice.value || ""));
  } else {
    return [];
  }
};

const getChoiceIncludeIfs = (formComponent: FormComponent): string[] => {
  if (formComponent.choices instanceof Array) {
    return formComponent.choices.map((choice) => (typeof choice === "string" ? "" : choice.includeIf || ""));
  } else {
    return [];
  }
};

const findType = (formComponent: FormComponent): ChoiceComponentType => {
  if (formComponent.format === ChoiceComponentType.YesNo) return ChoiceComponentType.YesNo;
  else if (formComponent.multivalue === "true") return ChoiceComponentType.Checkboxes;
  else return ChoiceComponentType.Radios;
};

export const initialChoiceState = (formComponent: FormComponent): ChoiceState => {
  const dividerPosition = typeof formComponent.dividerPosition === "number" ? formComponent.dividerPosition : undefined;
  const optional = isOptional(formComponent);
  const state: ChoiceState = {
    typeValue: findType(formComponent),
    label: formComponent.label,
    pageHeading: formComponent.label === undefined,
    choices: getChoices(formComponent),
    hints: getChoiceHints(formComponent),
    values: getChoiceValues(formComponent),
    includeIfs: getChoiceIncludeIfs(formComponent),
    helpText: formComponent.helpText,
    shortName: formComponent.shortName,
    errorShortName: formComponent.errorShortName,
    errorMessage: formComponent.errorMessage,
    optional: optional,
    dividerPosition: dividerPosition,
    dividerText: formComponent.dividerText,
    noneChoice: formComponent.noneChoice,
    noneChoiceError: formComponent.noneChoiceError,
  };

  const undo = replaceUndefinedByEmptyString(state);

  return { ...state, undo: undo };
};

export const ChoicePanelFactory =
  (
    host: string,
    formTemplateId: string,
    serverPageData: ServerPageData,
    sectionNumber: SectionNumber,
    maybeAccessCode: string | null,
  ) =>
  ({
    formComponentId,
    inputFormComponentId,
    interactionType,
    executeRequest,
    titleSignal,
    choiceState,
  }: ChoiceProps) => {
    const visibility = useContext(VisibilityContext);
    const choiceDispatch = useContext(StateContext);

    const refreshChoiceComponent = (state: ChoiceState) => {
      const id = formComponentId;

      const formComponentPart: FormComponentPart = {
        id,
      };

      if (state.pageHeading === true) {
        formComponentPart["label"] = ""; // Empty value will be purged on the server
      } else {
        formComponentPart["label"] = state.label;
      }

      const getChoiceUpdates = (choiceEn: string | null, index: number): ChoiceObject => {
        const en = { en: choiceEn !== null && choiceEn !== "" ? choiceEn : "…" };

        const choiceHint = choiceState.hints[index];
        const hint = choiceHint === undefined || choiceHint === "" ? {} : { hint: choiceHint };

        const choiceIncludeIf = choiceState.includeIfs[index];
        const includeIf = choiceIncludeIf === undefined || choiceIncludeIf === "" ? {} : { includeIf: choiceIncludeIf };

        return { ...en, ...hint, ...includeIf };
      };

      formComponentPart["choices"] =
        state.choices.length === 0
          ? defaultChoices
          : state.choices.map((choice, index) => getChoiceUpdates(choice, index));

      formComponentPart["helpText"] = state.helpText;
      formComponentPart["shortName"] = state.shortName;

      formComponentPart["errorShortName"] = state.errorShortName;
      formComponentPart["errorMessage"] = state.errorMessage;
      formComponentPart["mandatory"] = state.optional ? false : "";
      formComponentPart["dividerPosition"] = state.dividerPosition;
      formComponentPart["dividerText"] = typeof state.dividerPosition === "number" ? state.dividerText : "";

      switch (choiceState.typeValue) {
        case ChoiceComponentType.YesNo:
          formComponentPart["format"] = ChoiceComponentType.YesNo;
          formComponentPart["choices"] = "";
          formComponentPart["multivalue"] = "";
          formComponentPart["noneChoice"] = "";
          formComponentPart["noneChoiceError"] = "";
          formComponentPart["dividerPosition"] = "";
          formComponentPart["dividerText"] = "";
          break;
        case ChoiceComponentType.Radios:
          formComponentPart["format"] = "";
          formComponentPart["multivalue"] = "";
          formComponentPart["noneChoice"] = "";
          formComponentPart["noneChoiceError"] = "";
          break;
        case ChoiceComponentType.Checkboxes:
          formComponentPart["format"] = "";
          formComponentPart["multivalue"] = "true";
          if (state.noneChoice !== "") {
            formComponentPart["noneChoice"] = state.noneChoice;
            formComponentPart["noneChoiceError"] = state.noneChoiceError;
          } else {
            formComponentPart["noneChoice"] = "";
            formComponentPart["noneChoiceError"] = "";
          }
      }

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

    useUpdateComponent(choiceState, refreshChoiceComponent);

    const dispatch = (subtype: ChoiceUpdateEvent, content: any, record: Record<string, any> = {}): void => {
      choiceDispatch({
        type: DispatchTarget.Component,
        subtype: subtype,
        record: { ...record, content: content },
      });
    };

    const onTypeChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(ChoiceUpdateEvent.Type, input.value);
    };

    const onLabelChange = (e: SmartStringEvent) => {
      dispatch(ChoiceUpdateEvent.Label, e);
    };

    const onH1FieldLabelToggle = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      const label = input.checked ? choiceState.label : titleSignal.value;
      dispatch(ChoiceUpdateEvent.PageHeading, input.checked, { label: label });
    };

    const onShortNameChange = (e: SmartStringEvent) => {
      dispatch(ChoiceUpdateEvent.ShortName, e);
    };

    const onHelpTextChange = (e: SmartStringEvent) => {
      dispatch(ChoiceUpdateEvent.HelpText, e);
    };

    const onErrorShortNameKeyUp = (e: SmartStringEvent) => {
      dispatch(ChoiceUpdateEvent.ErrorShortName, e);
    };

    const onErrorShortNameChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(ChoiceUpdateEvent.ErrorShortName, input.value);
    };

    const onDividerPositionClick = (e: Event, index: number) => {
      const input = e.target as HTMLInputElement;
      dispatch(ChoiceUpdateEvent.DividerPosition, input.checked, { index: index });
    };

    const onDividerTextChange = (e: SmartStringEvent) => {
      dispatch(ChoiceUpdateEvent.DividerText, e);
    };

    const onNoneChoiceClick = (e: Event, index: number) => {
      const input = e.target as HTMLInputElement;
      dispatch(ChoiceUpdateEvent.NoneChoice, input.checked, { index: index });
    };

    const onNoneChoiceErrorKeyUp = (e: SmartStringEvent) => {
      dispatch(ChoiceUpdateEvent.NoneChoiceError, e);
    };
    const onErrorMessageKeyUp = (e: SmartStringEvent) => {
      dispatch(ChoiceUpdateEvent.ErrorMessage, e);
    };

    const optionalToggle = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      dispatch(ChoiceUpdateEvent.Optional, input.checked);
    };

    const generateChoiceDividerPositionInputElements = (index: number) => {
      const checked = choiceState.dividerPosition === index + 1;
      const dividerAfterId = "edit-dividerAfter-" + index;
      const dividerTextId = "edit-dividerText-" + index;
      if (index < currentNumberOfChoices - 1) {
        return (
          <>
            <input
              type="checkbox"
              id={dividerAfterId}
              checked={checked}
              onClick={(e) => onDividerPositionClick(e, index + 1)}
            />
            <label for={dividerAfterId}>Divide after</label>
            <div hidden={!checked}>
              <SmartStringInput id={dividerTextId} value={choiceState.dividerText} onKeyUp={onDividerTextChange}>
                Divider Text
              </SmartStringInput>
            </div>
          </>
        );
      } else {
        return "";
      }
    };

    const generateChoiceNoneChoiceInputElements = (index: number) => {
      if (choiceState.typeValue === ChoiceComponentType.Checkboxes) {
        const checked =
          choiceState.noneChoice === index + 1 ||
          (choiceState.noneChoice === choiceState.values[index] && choiceState.noneChoice !== "");
        const noneChoiceId = "edit-noneChoice-" + index;
        const noneChoiceErrorId = "edit-noneChoiceError-" + index;
        return (
          <>
            <div>
              <input
                type="checkbox"
                id={noneChoiceId}
                checked={checked}
                onClick={(e) => onNoneChoiceClick(e, index + 1)}
              />
              <label for={noneChoiceId}>Set as none choice</label>
            </div>
            <div hidden={!checked}>
              <SmartStringInput
                id={noneChoiceErrorId}
                value={choiceState.noneChoiceError}
                onKeyUp={onNoneChoiceErrorKeyUp}
              >
                None choice error
              </SmartStringInput>
            </div>
          </>
        );
      } else {
        return "";
      }
    };

    const onChoicesChange = (e: Event, index: number) => {
      const input = e.target as HTMLInputElement;
      dispatch(ChoiceUpdateEvent.Choice, input.value, { index: index });
    };

    const onHintChange = (e: Event, index: number) => {
      const input = e.target as HTMLInputElement;
      dispatch(ChoiceUpdateEvent.Hint, input.value, { index: index });
    };

    const onIncludeIfChange = (e: Event, index: number) => {
      const input = e.target as HTMLInputElement;
      dispatch(ChoiceUpdateEvent.IncludeIf, input.value, { index: index });
    };

    const currentNumberOfChoices = choiceState.choices.length;

    const isYesNo = choiceState.typeValue === ChoiceComponentType.YesNo;

    const showLabel = visibility.field === FieldVisibility.Label;
    const showHelpText = visibility.field === FieldVisibility.HelpText;
    const showChoice = visibility.field === FieldVisibility.Choice;
    const showAllFields = visibility.field === FieldVisibility.AllComponent || (isYesNo && showChoice);

    const isPanelVisible = showLabel || showHelpText || showChoice || showAllFields;

    return (
      <fieldset hidden={!isPanelVisible} class="panel">
        <legend class="panel">Choice component - id: {inputFormComponentId}</legend>
        <div hidden={!showAllFields}>
          <label for="edit-type">Type</label>
          <select id="edit-type" class="form-control" value={choiceState.typeValue} onChange={onTypeChange}>
            {Object.keys(ChoiceComponentType)
              .sort()
              .map((key) => (
                <option key={key} value={ChoiceComponentType[key as keyof typeof ChoiceComponentType]}>
                  {key}
                </option>
              ))}
          </select>
        </div>
        <div hidden={!(showAllFields || showLabel)}>
          <SmartStringInput
            id="edit-label"
            value={choiceState.label}
            onKeyUp={onLabelChange}
            disabled={choiceState.pageHeading}
          >
            Label
          </SmartStringInput>
        </div>
        <div hidden={!(showAllFields && interactionType === FieldInteractionType.TitleLabelWithSync)}>
          <input type="checkbox" id="h1-checkbox" checked={choiceState.pageHeading} onClick={onH1FieldLabelToggle} />
          <label for="h1-checkbox">Use field label as H1</label>
        </div>
        {Array.from({ length: currentNumberOfChoices }, (_, index) => (
          <div
            id={"div-edit-choice-" + index}
            hidden={!(showAllFields || (showChoice && index === visibility.index)) || isYesNo}
          >
            <label for={"edit-choice-" + index}>
              Choice {index + 1} text <span class="additional-info">{choiceState.values[index]}</span>
            </label>
            <input
              id={"edit-choice-" + index}
              class="form-control"
              value={choiceState.choices[index] != null ? choiceState.choices[index] : ""}
              onKeyUp={(e) => onChoicesChange(e, index)}
            />
            <label for={"edit-hint-" + index}>Choice {index + 1} hint</label>
            <input
              id={"edit-hint-" + index}
              class="form-control"
              value={choiceState.hints[index] != null ? choiceState.hints[index] : ""}
              onKeyUp={(e) => onHintChange(e, index)}
            />
            <label for={"edit-includeIf-" + index}>Choice {index + 1} include if</label>
            <input
              id={"edit-includeIf-" + index}
              class="form-control"
              value={choiceState.includeIfs[index] != null ? choiceState.includeIfs[index] : ""}
              onKeyUp={(e) => onIncludeIfChange(e, index)}
            />
            {generateChoiceDividerPositionInputElements(index)}
            {generateChoiceNoneChoiceInputElements(index)}
          </div>
        ))}

        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-shortName" value={choiceState.shortName} onKeyUp={onShortNameChange}>
            Short name
          </SmartStringInput>
        </div>
        <div hidden={!(showAllFields || showHelpText)}>
          <SmartStringInput id="edit-helpText" value={choiceState.helpText} onKeyUp={onHelpTextChange}>
            Help text
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorShortName" value={choiceState.errorShortName} onKeyUp={onErrorShortNameKeyUp}>
            Error short name
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorMessage" value={choiceState.errorMessage} onKeyUp={onErrorMessageKeyUp}>
            Error message
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <input type="checkbox" id="optional" checked={choiceState.optional} onClick={optionalToggle} />
          <label for="optional">Optional</label>
        </div>
      </fieldset>
    );
  };
