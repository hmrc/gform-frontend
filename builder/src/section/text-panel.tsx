import { Signal } from "@preact/signals";

import type {
  ServerPageData,
  FormComponentPart,
  ContentScriptRequest,
  FormComponent,
  DispatchEvent,
  FormComponentUpdateRequest,
  SectionNumber,
  TextState,
  SmartStringEvent,
  SmartString,
} from "../types";
import { DispatchTarget, TextUpdateEvent, FieldInteractionType, MessageKind, FieldVisibility } from "../types";
import { replaceUndefinedByEmptyString, isOptional } from "./pure-functions";
import {
  extractFormatPrefix,
  extractFormatValues,
  shouldDisplayFormatValues,
  getFormatValueLabel,
} from "./text-helper";
import { updateSmartString, SmartStringInput } from "./smart-string";
import { useContext } from "preact/hooks";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { useUpdateComponent } from "./hooks/useUpdateComponent";

interface TextFormatState {
  formatParam1: string | undefined;
  formatParam2: string | undefined;
}

const parseNumbericValue = (s: string): string => {
  const intValue = parseInt(s);
  return !isNaN(intValue) && intValue >= 0 ? intValue.toString() : "";
};

const keepFormatParams = (oldState: string, newState: string): boolean => {
  return (
    (oldState === "text" && newState === "shortText") ||
    (oldState === "shortText" && newState === "text") ||
    (oldState === "number" && newState === "positiveNumber") ||
    (oldState === "positiveNumber" && newState === "number")
  );
};

const makeFormatParamConsistent = (
  formatParam1: string | undefined,
  formatParam2: string | undefined,
  defaultParam1: string,
  defaultParam2: string,
): TextFormatState => {
  if ((!formatParam2 && formatParam1 === defaultParam1) || (!formatParam1 && formatParam2 === defaultParam2)) {
    return { formatParam1: "", formatParam2: "" };
  } else if (formatParam2 && !formatParam1) {
    return { formatParam1: defaultParam1, formatParam2: formatParam2 };
  } else if (!formatParam2 && formatParam1) {
    return { formatParam1: formatParam1, formatParam2: defaultParam2 };
  } else {
    return { formatParam1: formatParam1, formatParam2: formatParam2 };
  }
};

export const textReducer = (state: TextState, action: DispatchEvent<TextUpdateEvent>): TextState => {
  const record = action.record;
  switch (action.subtype) {
    case TextUpdateEvent.Label:
      const label = updateSmartString(record.content as SmartStringEvent, state.label);
      return { ...state, label };
    case TextUpdateEvent.PageHeading:
      return {
        ...state,
        pageHeading: record.content,
        label: record.label,
      };
    case TextUpdateEvent.HelpText:
      const helpText = updateSmartString(record.content as SmartStringEvent, state.helpText);
      return { ...state, helpText };
    case TextUpdateEvent.ShortName:
      const shortName = updateSmartString(record.content as SmartStringEvent, state.shortName);
      return { ...state, shortName };
    case TextUpdateEvent.Format: {
      const oldState = state.format;
      const newState = record.content;

      const keepParams = keepFormatParams(oldState, newState);

      if (newState === "referenceNumber") {
        return { ...state, format: newState, formatParam1: "5", formatParam2: "10" };
      } else {
        if (keepParams) {
          return { ...state, format: newState };
        } else {
          return { ...state, format: newState, formatParam1: "", formatParam2: "" };
        }
      }
    }
    case TextUpdateEvent.FormatParam1: {
      const formatParam1 = parseNumbericValue(record.content);
      return { ...state, formatParam1: formatParam1 };
    }
    case TextUpdateEvent.FormatParam1Change: {
      const formatParam1 = parseNumbericValue(record.content);
      switch (state.format) {
        case "text":
        case "shortText":
          {
            const formatParams = makeFormatParamConsistent(formatParam1, state.formatParam2, "0", "1000");
            return { ...state, ...formatParams };
          }
          break;
        case "number":
        case "positiveNumber":
          {
            const formatParams = makeFormatParamConsistent(formatParam1, state.formatParam2, "11", "2");
            return { ...state, ...formatParams };
          }
          break;
        case "referenceNumber": {
          if (!formatParam1) {
            return { ...state, formatParam1: "5" };
          } else {
            return { ...state, formatParam1: formatParam1 };
          }
        }
        default: {
          return { ...state };
        }
      }
    }
    case TextUpdateEvent.FormatParam2: {
      const formatParam2 = parseNumbericValue(record.content);
      return { ...state, formatParam2: formatParam2 };
    }
    case TextUpdateEvent.FormatParam2Change: {
      const formatParam2 = parseNumbericValue(record.content);
      switch (state.format) {
        case "text":
        case "shortText":
          {
            const formatParams = makeFormatParamConsistent(state.formatParam1, formatParam2, "0", "1000");
            return { ...state, ...formatParams };
          }
          break;
        case "number":
        case "positiveNumber":
          {
            const formatParams = makeFormatParamConsistent(state.formatParam1, formatParam2, "11", "2");
            return { ...state, ...formatParams };
          }
          break;
        case "referenceNumber": {
          if (!formatParam2) {
            return { ...state, formatParam2: "10" };
          } else {
            return { ...state, formatParam2: formatParam2 };
          }
        }
        default: {
          return { ...state };
        }
      }
    }
    case TextUpdateEvent.ErrorShortName:
      const errorShortName = updateSmartString(record.content as SmartStringEvent, state.errorShortName);
      return { ...state, errorShortName };
    case TextUpdateEvent.ErrorShortNameStart:
      const errorShortNameStart = updateSmartString(record.content as SmartStringEvent, state.errorShortNameStart);
      return { ...state, errorShortNameStart };
    case TextUpdateEvent.ErrorExample:
      const errorExample = updateSmartString(record.content as SmartStringEvent, state.errorExample);
      return { ...state, errorExample };
    case TextUpdateEvent.ErrorMessage:
      const errorMessage = updateSmartString(record.content as SmartStringEvent, state.errorMessage);
      return { ...state, errorMessage };
    case TextUpdateEvent.DisplayWidth:
      return { ...state, displayWidth: record.content };
    case TextUpdateEvent.LabelSize:
      return { ...state, labelSize: record.content };
    case TextUpdateEvent.Optional:
      return { ...state, optional: record.content };
    case TextUpdateEvent.IsLookup:
      return { ...state, isLookup: record.content };
    case TextUpdateEvent.Lookup:
      return { ...state, lookup: record.content };
    case TextUpdateEvent.IsMultiline:
      return { ...state, isMultiline: record.content };
    default:
      return state;
  }
};

export const initialTextState = (formComponent: FormComponent): TextState => {
  const { min: param1, max: param2 } = extractFormatValues(formComponent.format || "");
  const format = extractFormatPrefix(formComponent.format || "");
  const isLookup = formComponent.format?.startsWith("lookup") || false;
  const isMultiline = formComponent.multiline === true;
  const optional = isOptional(formComponent);
  const state: TextState = {
    label: formComponent.label,
    pageHeading: formComponent.label === undefined,
    helpText: formComponent.helpText,
    shortName: formComponent.shortName,
    format: isLookup ? "text" : format,
    formatParam1: param1,
    formatParam2: param2,
    errorShortName: formComponent.errorShortName,
    errorShortNameStart: formComponent.errorShortNameStart,
    errorExample: formComponent.errorExample,
    errorMessage: formComponent.errorMessage,
    displayWidth: formComponent.displayWidth,
    labelSize: formComponent.labelSize,
    optional: optional,
    isLookup: isLookup,
    lookup: isLookup ? formComponent.format : "lookup(country)",
    isMultiline: isMultiline,
  };

  const undo = replaceUndefinedByEmptyString(state);

  return { ...state, undo: undo };
};

export interface TextProps {
  inputFormComponentId: string;
  interactionType: FieldInteractionType;
  executeRequest: (updateRequest: ContentScriptRequest) => void;
  titleSignal: Signal<SmartString>;
  textState: TextState;
}

export const TextPanelFactory =
  (
    host: string,
    formTemplateId: string,
    formComponent: FormComponent,
    serverPageData: ServerPageData,
    sectionNumber: SectionNumber,
    maybeAccessCode: string | null,
  ) =>
  ({ inputFormComponentId, interactionType, executeRequest, titleSignal, textState }: TextProps) => {
    const visibility = useContext(VisibilityContext);
    const textDispatch = useContext(StateContext);

    const dispatch = (subtype: TextUpdateEvent, content: any, record: Record<string, any> = {}): void => {
      textDispatch({
        type: DispatchTarget.Component,
        subtype: subtype,
        record: { ...record, content: content },
      });
    };

    const refreshTextComponent = (state: TextState) => {
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

      if (state.format !== null) {
        const value =
          state.formatParam1 !== "" && state.formatParam2 !== ""
            ? `${state.format}(${state.formatParam1},${state.formatParam2})`
            : state.format;
        formComponentPart["format"] = value;
      }

      if (state.isLookup) {
        formComponentPart["format"] = state.lookup;
      }

      if (state.isMultiline) {
        formComponentPart["multiline"] = true;
      } else if (state.isLookup) {
        formComponentPart["multiline"] = false;
      } else {
        formComponentPart["multiline"] = "";
      }

      formComponentPart["errorShortName"] = textState.errorShortName;
      formComponentPart["errorShortNameStart"] = textState.errorShortNameStart;
      formComponentPart["errorExample"] = textState.errorExample;
      formComponentPart["errorMessage"] = textState.errorMessage;
      formComponentPart["displayWidth"] = textState.displayWidth;
      formComponentPart["labelSize"] = textState.labelSize;
      formComponentPart["mandatory"] = textState.optional ? false : "";

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

    useUpdateComponent(textState, refreshTextComponent);

    const onLabelChange = (e: SmartStringEvent) => {
      dispatch(TextUpdateEvent.Label, e);
    };

    const onH1FieldLabelToggle = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      const label = input.checked ? textState.label : titleSignal.value;
      dispatch(TextUpdateEvent.PageHeading, input.checked, { label: label });
    };

    const onShortNameChange = (e: SmartStringEvent) => {
      dispatch(TextUpdateEvent.ShortName, e);
    };

    const onHelpTextChange = (e: SmartStringEvent) => {
      dispatch(TextUpdateEvent.HelpText, e);
    };

    const onFormatChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.Format, input.value);
    };

    const isLookupToggle = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.IsLookup, input.checked);
    };

    const onParam1KeyUp = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.FormatParam1, input.value);
    };

    const onParam1Change = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.FormatParam1Change, input.value);
    };

    const onParam2KeyUp = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.FormatParam2, input.value);
    };

    const onParam2Change = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.FormatParam2Change, input.value);
    };

    const onLookupChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.Lookup, input.value);
    };

    const onErrorShortNameChange = (e: SmartStringEvent) => {
      dispatch(TextUpdateEvent.ErrorShortName, e);
    };

    const onErrorShortNameStartChange = (e: SmartStringEvent) => {
      dispatch(TextUpdateEvent.ErrorShortNameStart, e);
    };

    const onErrorExampleChange = (e: SmartStringEvent) => {
      dispatch(TextUpdateEvent.ErrorExample, e);
    };

    const onErrorMessageChange = (e: SmartStringEvent) => {
      dispatch(TextUpdateEvent.ErrorMessage, e);
    };

    const onDisplayWidthChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.DisplayWidth, input.value);
    };

    const onLabelSizeChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.LabelSize, input.value);
    };

    const optionalToggle = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.Optional, input.checked);
    };

    const onMultilineClick = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      dispatch(TextUpdateEvent.IsMultiline, input.checked);
    };

    const showLabel = visibility.field === FieldVisibility.Label;
    const showHelpText = visibility.field === FieldVisibility.HelpText;
    const showAllFields = visibility.field === FieldVisibility.AllComponent;

    const isPanelVisible = showLabel || showHelpText || showAllFields;

    return (
      <fieldset hidden={!isPanelVisible} class="panel">
        <legend class="panel">Text component - id: {inputFormComponentId}</legend>
        <div hidden={!(showAllFields || showLabel)}>
          <SmartStringInput
            id="edit-label"
            value={textState.label}
            onKeyUp={onLabelChange}
            disabled={textState.pageHeading}
          >
            Label
          </SmartStringInput>
        </div>
        <div hidden={!(showAllFields && interactionType === FieldInteractionType.TitleLabelWithSync)}>
          <input type="checkbox" id="h1-checkbox" checked={textState.pageHeading} onClick={onH1FieldLabelToggle} />
          <label for="h1-checkbox">Use field label as H1</label>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-shortName" value={textState.shortName} onKeyUp={onShortNameChange}>
            Short name
          </SmartStringInput>
        </div>
        <div hidden={!(showAllFields || showHelpText)}>
          <SmartStringInput id="edit-helpText" value={textState.helpText} onKeyUp={onHelpTextChange}>
            Help text
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <input type="checkbox" id="lookup" checked={textState.isLookup} onClick={isLookupToggle} />
          <label for="lookup">Lookup</label>
        </div>
        <div hidden={textState.isLookup || !showAllFields}>
          <label for="edit-format">Format</label>
          <select id="edit-format" class="form-control" value={textState.format} onChange={onFormatChange}>
            <option value="text">Text</option>
            <option value="shortText">Shortext</option>
            <option value="telephoneNumber">TelephoneNumber</option>
            <option value="email">Email</option>
            <option value="number">Number</option>
            <option value="positiveNumber">PositiveNumber</option>
            <option value="positiveWholeNumber">PositiveWholeNumber</option>
            <option value="sterling">Sterling</option>
            <option value="positiveSterling">PositiveSterling</option>
            <option value="positiveWholeSterling">PositiveWholeSterling</option>
            <option value="nino">Nino</option>
            <option value="saUtr">SaUtr</option>
            <option value="ctUtr">CtUtr</option>
            <option value="ukVrn">UkVrn</option>
            <option value="payeReference">PayeReference</option>
            <option value="UkEORI">UkEORI</option>
            <option value="childBenefitNumber">ChildBenefitNumber</option>
            <option value="referenceNumber">ReferenceNumber</option>
            <option value="ukBankAccountNumber">UkBankAccountNumber</option>
            <option value="ukSortCode">UkSortCode</option>
          </select>
        </div>
        <div
          style={{
            display: shouldDisplayFormatValues(textState.format || "") && !textState.isLookup ? "flex" : "none",
            flexDirection: "row",
            alignItems: "center",
          }}
        >
          <div hidden={!showAllFields}>
            <label for="edit-formatLeft">{getFormatValueLabel(textState.format || "")?.left}</label>
            <input
              id="edit-formatLeft"
              class="form-control"
              type="number"
              value={textState.formatParam1}
              onKeyUp={onParam1KeyUp}
              onChange={onParam1Change}
            />
          </div>
          <div hidden={!showAllFields}>
            <label for="edit-formatRight">{getFormatValueLabel(textState.format || "")?.right}</label>
            <input
              id="edit-formatRight"
              class="form-control"
              type="number"
              value={textState.formatParam2}
              onKeyUp={onParam2KeyUp}
              onChange={onParam2Change}
            />
          </div>
        </div>
        <div hidden={!(textState.isLookup && showAllFields)}>
          <label for="edit-lookup">Lookup</label>
          <select id="edit-lookup" class="form-control" value={textState.lookup} onChange={onLookupChange}>
            <option value="lookup(country)">Country</option>
            <option value="lookup(currency)">Currency</option>
            <option value="lookup(port)">Port</option>
            <option value="lookup(sicCode)">SicCode</option>
          </select>
        </div>
        <div hidden={textState.isLookup || !showAllFields}>
          <input type="checkbox" id="multiline" checked={textState.isMultiline} onClick={onMultilineClick} />
          <label for="multiline">Multiline</label>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorShortName" value={textState.errorShortName} onKeyUp={onErrorShortNameChange}>
            Error short name
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput
            id="edit-errorShortNameStart"
            value={textState.errorShortNameStart}
            onKeyUp={onErrorShortNameStartChange}
          >
            Error short name start
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorExample" value={textState.errorExample} onKeyUp={onErrorExampleChange}>
            Error example
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorMessage" value={textState.errorMessage} onKeyUp={onErrorMessageChange}>
            Error message
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <label for="edit-displayWidth">DisplayWidth</label>
          <select
            id="edit-displayWidth"
            class="form-control"
            value={textState.displayWidth}
            onChange={onDisplayWidthChange}
          >
            <option value="">Default</option>
            <option value="xs">xs - Very small</option>
            <option value="s">s - Small</option>
            <option value="m">m - Medium</option>
            <option value="l">l - Large</option>
            <option value="xl">xl - Very large</option>
            <option value="xxl">xxl - Very very large</option>
          </select>
        </div>
        <div hidden={!showAllFields}>
          <label for="edit-labelSize">Label size</label>
          <select id="edit-labelSize" class="form-control" value={textState.labelSize} onChange={onLabelSizeChange}>
            <option value="">Default</option>
            <option value="xs">xs - Very small</option>
            <option value="s">s - Small</option>
            <option value="m">m - Medium</option>
            <option value="l">l - Large</option>
            <option value="xl">xl - Very large</option>
          </select>
        </div>
        <div hidden={!showAllFields}>
          <input type="checkbox" id="optional" checked={textState.optional} onClick={optionalToggle} />
          <label for="optional">Optional</label>
        </div>
      </fieldset>
    );
  };
