import { Signal } from "@preact/signals";
import { useContext } from "preact/hooks";
import type {
  ServerPageData,
  SectionNumber,
  SmartString,
  AddressState,
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
  AddressUpdateEvent,
  DispatchTarget,
  FormComponentUpdateRequest,
} from "../types";
import { updateSmartString, SmartStringInput } from "./smart-string";
import { replaceUndefinedByEmptyString, isOptional } from "./pure-functions";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { useUpdateComponent } from "./hooks/useUpdateComponent";

export const addressReducer = (state: AddressState, action: DispatchEvent<AddressUpdateEvent>): AddressState => {
  const record = action.record;
  switch (action.subtype) {
    case AddressUpdateEvent.OverseasAddress:
      return { ...state, isOverseasAddress: record.content };
    case AddressUpdateEvent.Label:
      const label = updateSmartString(record.content as SmartStringEvent, state.label);
      return { ...state, label };
    case AddressUpdateEvent.PageHeading:
      return {
        ...state,
        pageHeading: record.content,
        label: record.label,
      };
    case AddressUpdateEvent.ShortName:
      const shortName = updateSmartString(record.content as SmartStringEvent, state.shortName);
      return { ...state, shortName };
    case AddressUpdateEvent.HelpText:
      const helpText = updateSmartString(record.content as SmartStringEvent, state.helpText);
      return { ...state, helpText };
    case AddressUpdateEvent.ErrorShortName:
      const errorShortName = updateSmartString(record.content as SmartStringEvent, state.errorShortName);
      return { ...state, errorShortName };
    case AddressUpdateEvent.ErrorShortNameStart:
      const errorShortNameStart = updateSmartString(record.content as SmartStringEvent, state.errorShortNameStart);
      return { ...state, errorShortNameStart };
    case AddressUpdateEvent.ErrorExample:
      const errorExample = updateSmartString(record.content as SmartStringEvent, state.errorExample);
      return { ...state, errorExample };
    case AddressUpdateEvent.ErrorMessage:
      const errorMessage = updateSmartString(record.content as SmartStringEvent, state.errorMessage);
      return { ...state, errorMessage };
    case AddressUpdateEvent.LabelSize:
      return { ...state, labelSize: record.content };
    case AddressUpdateEvent.CityMandatory:
      return { ...state, isCityMandatory: record.content };
    case AddressUpdateEvent.CountyDisplayed:
      return { ...state, isCountyDisplayed: record.content };
    case AddressUpdateEvent.Line2Mandatory:
      return { ...state, isLine2Mandatory: record.content };
    case AddressUpdateEvent.PostcodeMandatory:
      return { ...state, isPostcodeMandatory: record.content };
    case AddressUpdateEvent.CountryLookup:
      return { ...state, isCountryLookup: record.content };
    case AddressUpdateEvent.CountryDisplayed:
      return { ...state, isCountryDisplayed: record.content };
    default:
      return state;
  }
};

export const initialAddressState = (formComponent: FormComponent): AddressState => {
  const optional = isOptional(formComponent);
  const isCityMandatory =
    formComponent.type === "overseasAddress"
      ? formComponent.cityMandatory !== "false"
      : formComponent.cityMandatory === "true";
  const state: AddressState = {
    isOverseasAddress: formComponent.type === "overseasAddress",
    label: formComponent.label,
    pageHeading: formComponent.label === undefined,
    helpText: formComponent.helpText,
    shortName: formComponent.shortName,
    errorShortName: formComponent.errorShortName,
    errorShortNameStart: formComponent.errorShortNameStart,
    errorExample: formComponent.errorExample,
    errorMessage: formComponent.errorMessage,
    labelSize: formComponent.labelSize,
    isCityMandatory: isCityMandatory,
    isCountyDisplayed: formComponent.countyDisplayed === "true",
    isLine2Mandatory: formComponent.line2Mandatory === "true",
    isPostcodeMandatory: formComponent.postcodeMandatory === "true",
    isCountryLookup: formComponent.countryLookup !== "false",
    isCountryDisplayed: formComponent.countryDisplayed !== "false",
  };

  const undo = replaceUndefinedByEmptyString(state);

  return { ...state, undo: undo };
};

export interface AddressProps {
  inputFormComponentId: string;
  interactionType: FieldInteractionType;
  executeRequest: (updateRequest: ContentScriptRequest) => void;
  titleSignal: Signal<SmartString>;
  addressState: AddressState;
}

export const AddressPanelFactory =
  (
    host: string,
    formTemplateId: string,
    formComponent: FormComponent,
    serverPageData: ServerPageData,
    sectionNumber: SectionNumber,
    maybeAccessCode: string | null,
  ) =>
  ({ inputFormComponentId, interactionType, executeRequest, titleSignal, addressState }: AddressProps) => {
    const visibility = useContext(VisibilityContext);
    const stateDispatch = useContext(StateContext);

    const dispatch = (subtype: AddressUpdateEvent, content: any, record: Record<string, any> = {}): void => {
      stateDispatch({
        type: DispatchTarget.Component,
        subtype: subtype,
        record: { ...record, content: content },
      });
    };

    const refreshAddressComponent = (state: AddressState) => {
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
      formComponentPart["errorShortNameStart"] = state.errorShortNameStart;
      formComponentPart["errorExample"] = state.errorExample;
      formComponentPart["errorMessage"] = state.errorMessage;
      formComponentPart["labelSize"] = state.labelSize;

      formComponentPart["type"] = state.isOverseasAddress ? "overseasAddress" : "address";
      const isCityMandatory = state.isCityMandatory
        ? state.isOverseasAddress
          ? ""
          : "true"
        : state.isOverseasAddress
          ? "false"
          : "";

      formComponentPart["cityMandatory"] = isCityMandatory;
      formComponentPart["countyDisplayed"] = !state.isOverseasAddress && state.isCountyDisplayed ? "true" : "";
      formComponentPart["line2Mandatory"] = state.isOverseasAddress && state.isLine2Mandatory ? "true" : "";
      formComponentPart["postcodeMandatory"] = state.isOverseasAddress && state.isPostcodeMandatory ? "true" : "";
      formComponentPart["countryLookup"] = state.isOverseasAddress && state.isCountryLookup ? "" : "false";
      formComponentPart["countryDisplayed"] = state.isOverseasAddress && state.isCountryDisplayed ? "" : "false";

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

    useUpdateComponent(addressState, refreshAddressComponent);

    const onLabelChange = (e: SmartStringEvent) => {
      dispatch(AddressUpdateEvent.Label, e);
    };

    const onOverseasAddressClick = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(AddressUpdateEvent.OverseasAddress, input.checked);
    };

    const onCityMandatoryClick = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(AddressUpdateEvent.CityMandatory, input.checked);
    };

    const onCountyDisplayedClick = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(AddressUpdateEvent.CountyDisplayed, input.checked);
    };

    const onLine2MandatoryClick = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(AddressUpdateEvent.Line2Mandatory, input.checked);
    };

    const onPostcodeMandatoryClick = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(AddressUpdateEvent.PostcodeMandatory, input.checked);
    };

    const onCountryLookupClick = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(AddressUpdateEvent.CountryLookup, input.checked);
    };

    const onCountryDisplayedClick = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(AddressUpdateEvent.CountryDisplayed, input.checked);
    };

    const onH1FieldLabelToggle = (e: MouseEvent) => {
      const input = e.target as HTMLInputElement;
      const label = input.checked ? addressState.label : titleSignal.value;
      dispatch(AddressUpdateEvent.PageHeading, input.checked, { label: label });
    };

    const onShortNameChange = (e: SmartStringEvent) => {
      dispatch(AddressUpdateEvent.ShortName, e);
    };

    const onHelpTextChange = (e: SmartStringEvent) => {
      dispatch(AddressUpdateEvent.HelpText, e);
    };

    const onErrorShortNameChange = (e: SmartStringEvent) => {
      dispatch(AddressUpdateEvent.ErrorShortName, e);
    };

    const onErrorShortNameStartChange = (e: SmartStringEvent) => {
      dispatch(AddressUpdateEvent.ErrorShortNameStart, e);
    };

    const onErrorExampleChange = (e: SmartStringEvent) => {
      dispatch(AddressUpdateEvent.ErrorExample, e);
    };

    const onErrorMessageChange = (e: SmartStringEvent) => {
      dispatch(AddressUpdateEvent.ErrorMessage, e);
    };

    const onLabelSizeChange = (e: Event) => {
      const input = e.target as HTMLInputElement;
      dispatch(AddressUpdateEvent.LabelSize, input.value);
    };

    const isOverseasAddress = addressState.isOverseasAddress;
    const showLabel = visibility.field === FieldVisibility.Label;
    const showHelpText = visibility.field === FieldVisibility.HelpText;
    const showAllFields = visibility.field === FieldVisibility.AllComponent;

    const isPanelVisible = showLabel || showHelpText || showAllFields;
    return (
      <fieldset hidden={!isPanelVisible} class="panel">
        <legend class="panel">Address component - id: {inputFormComponentId}</legend>
        <div hidden={!showAllFields}>
          <input
            id="edit-overseasAddress"
            type="checkbox"
            checked={addressState.isOverseasAddress}
            onClick={onOverseasAddressClick}
          />
          <label for="edit-overseasAddress">Overseas Address</label>
        </div>
        <div hidden={!(showAllFields || showLabel)}>
          <SmartStringInput
            id="edit-label"
            value={addressState.label}
            onKeyUp={onLabelChange}
            disabled={addressState.pageHeading}
          >
            Label
          </SmartStringInput>
        </div>
        <div hidden={!(showAllFields && interactionType === FieldInteractionType.TitleLabelWithSync)}>
          <input type="checkbox" id="h1-checkbox" checked={addressState.pageHeading} onClick={onH1FieldLabelToggle} />
          <label for="h1-checkbox">Use field label as H1</label>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-shortName" value={addressState.shortName} onKeyUp={onShortNameChange}>
            Short name
          </SmartStringInput>
        </div>
        <div hidden={!(showAllFields || showHelpText)}>
          <SmartStringInput id="edit-helpText" value={addressState.helpText} onKeyUp={onHelpTextChange}>
            Help Text
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput
            id="edit-errorShortName"
            value={addressState.errorShortName}
            onKeyUp={onErrorShortNameChange}
          >
            Error Short Name
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput
            id="edit-errorShortNameStart"
            value={addressState.errorShortNameStart}
            onKeyUp={onErrorShortNameStartChange}
          >
            Error Short Name Start
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorExample" value={addressState.errorExample} onKeyUp={onErrorExampleChange}>
            Error Example
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <SmartStringInput id="edit-errorMessage" value={addressState.errorMessage} onKeyUp={onErrorMessageChange}>
            Error message
          </SmartStringInput>
        </div>
        <div hidden={!showAllFields}>
          <label for="edit-labelSize">Label size</label>
          <select id="edit-labelSize" class="form-control" value={addressState.labelSize} onChange={onLabelSizeChange}>
            <option value="">Default</option>
            <option value="xs">xs - Very small</option>
            <option value="s">s - Small</option>
            <option value="m">m - Medium</option>
            <option value="l">l - Large</option>
            <option value="xl">xl - Very large</option>
          </select>
        </div>
        <div hidden={!showAllFields}>
          <input
            id="edit-cityMandatory"
            type="checkbox"
            checked={addressState.isCityMandatory}
            onClick={onCityMandatoryClick}
          />
          <label for="edit-cityMandatory">City Mandatory</label>
        </div>
        <div hidden={!(showAllFields && !isOverseasAddress)}>
          <input
            id="edit-countyDisplayed"
            type="checkbox"
            checked={addressState.isCountyDisplayed}
            onClick={onCountyDisplayedClick}
          />
          <label for="edit-countyDisplayed">County Displayed</label>
        </div>
        <div hidden={!(showAllFields && isOverseasAddress)}>
          <input
            id="edit-line2Mandatory"
            type="checkbox"
            checked={addressState.isLine2Mandatory}
            onClick={onLine2MandatoryClick}
          />
          <label for="edit-line2Mandatory">Line2 Mandatory</label>
        </div>
        <div hidden={!(showAllFields && isOverseasAddress)}>
          <input
            id="edit-postcodeMandatory"
            type="checkbox"
            checked={addressState.isPostcodeMandatory}
            onClick={onPostcodeMandatoryClick}
          />
          <label for="edit-postcodeMandatory">PostCode Mandatory</label>
        </div>
        <div hidden={!(showAllFields && isOverseasAddress)}>
          <input
            id="edit-countryLookup"
            type="checkbox"
            checked={addressState.isCountryLookup}
            disabled={!addressState.isCountryDisplayed}
            onClick={onCountryLookupClick}
          />
          <label for="edit-countryLookup">Country Lookup</label>
        </div>
        <div hidden={!(showAllFields && isOverseasAddress)}>
          <input
            id="edit-countryDisplayed"
            type="checkbox"
            checked={addressState.isCountryDisplayed}
            onClick={onCountryDisplayedClick}
          />
          <label for="edit-countryDisplayed">Country Displayed</label>
        </div>
      </fieldset>
    );
  };
