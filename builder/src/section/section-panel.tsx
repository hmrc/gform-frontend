import { Signal } from "@preact/signals";
import type {
  ContentScriptRequest,
  ServerPageData,
  Section,
  SectionPart,
  SectionUpdateRequest,
  SectionNumber,
  SectionDetails,
  SectionState,
  SmartString,
  DispatchEvent,
  SmartStringEvent,
  SmartStringCondition,
} from "../types";
import { SmartStringFieldType, DispatchTarget, SectionUpdateEvent, MessageKind, FieldVisibility } from "../types";
import { useContext } from "preact/hooks";
import { updateSmartString, SmartStringInput } from "./smart-string";
import { VisibilityContext } from "./visibility-context";
import { StateContext } from "./state-context";
import { replaceUndefinedByEmptyString } from "../pure-functions";
import { useUpdateComponent } from "./hooks/useUpdateComponent";

export const sectionReducer = (state: SectionState, action: DispatchEvent<SectionUpdateEvent>): SectionState => {
  const record = action.record;
  switch (action.subtype) {
    case SectionUpdateEvent.Caption:
      const caption = updateSmartString(record as SmartStringEvent, state.caption);
      return { ...state, caption };
    case SectionUpdateEvent.Title:
      const title = updateSmartString(record as SmartStringEvent, state.title);
      return { ...state, title };
    case SectionUpdateEvent.Description:
      const description = updateSmartString(record as SmartStringEvent, state.description);
      return { ...state, description };
    case SectionUpdateEvent.ShortName:
      const shortName = updateSmartString(record as SmartStringEvent, state.shortName);
      return { ...state, shortName };
    case SectionUpdateEvent.ContinueLabel:
      const continueLabel = updateSmartString(record as SmartStringEvent, state.continueLabel);
      return { ...state, continueLabel };
    case SectionUpdateEvent.YesChecked:
      return { ...state, yesChecked: true, noChecked: false };
    case SectionUpdateEvent.NoChecked:
      return { ...state, yesChecked: false, noChecked: true };
    default:
      return state;
  }
};

export const initialSectionState = (currentSection: Section): SectionState => {
  const invisiblePageTitle = currentSection.presentationHint === "invisiblePageTitle";
  const state = {
    caption: currentSection.caption,
    title: currentSection.title,
    description: currentSection.description,
    shortName: currentSection.shortName,
    continueLabel: currentSection.continueLabel,
    yesChecked: !invisiblePageTitle,
    noChecked: invisiblePageTitle,
  };

  // For example when description is undefined and user adds description
  // via section panel, undo needs to remove description and for that
  // description needs to be empty string, for the server to see it.
  const undo = replaceUndefinedByEmptyString(state);

  return { ...state, undo: undo };
};

export interface SectionProps {
  executeRequest: (updateRequest: ContentScriptRequest) => void;
  titleSignal: Signal<SmartString>;
  sectionState: SectionState;
}

export const SectionControllerFactory =
  (
    inputFormComponentId: string,
    host: string,
    formTemplateId: string,
    sectionNumber: SectionNumber,
    serverPageData: ServerPageData,
    maybeAccessCode: string | null,
  ) =>
  ({ executeRequest, titleSignal, sectionState }: SectionProps) => {
    titleSignal.value = sectionState.title || "";

    const visibility = useContext(VisibilityContext);

    const sectionDispatch = useContext(StateContext);

    const dispatch = (subtype: SectionUpdateEvent, content: any): void => {
      const event: DispatchEvent<SectionUpdateEvent> = {
        type: DispatchTarget.Section,
        subtype: subtype,
        record: content,
      };
      sectionDispatch(event);
    };

    const refreshSection = (sectionState: SectionState) => {
      const sectionPart: SectionPart = {};

      sectionPart["title"] = sectionState.title;

      sectionPart["caption"] = sectionState.caption;
      sectionPart["description"] = sectionState.description;
      sectionPart["shortName"] = sectionState.shortName;
      sectionPart["continueLabel"] = sectionState.continueLabel;
      sectionPart["presentationHint"] = sectionState.noChecked ? "invisiblePageTitle" : "";

      const sectionDetails: SectionDetails = {
        section: sectionPart,
        sectionPath: serverPageData.sectionPath,
      };

      const data: SectionUpdateRequest = {
        section: sectionDetails,
        sectionNumber: sectionNumber,
        formComponentId: inputFormComponentId,
        maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
      };

      const updateTitle: ContentScriptRequest = {
        host: host,
        kind: MessageKind.UpdateSectionTitle,
        formTemplateId: formTemplateId,
        data: data,
      };

      executeRequest(updateTitle);
    };

    useUpdateComponent(sectionState, refreshSection);

    const showCaption = visibility.field === FieldVisibility.Caption;
    const showTitle = visibility.field === FieldVisibility.Title;
    const showDescription = visibility.field === FieldVisibility.Description;

    const showAllSectionFields = visibility.field === FieldVisibility.AllSection;

    const isPanelVisible = showAllSectionFields || showCaption || showTitle || showDescription;

    const onCaptionChange = (e: SmartStringEvent) => {
      dispatch(SectionUpdateEvent.Caption, e);
    };

    const onTitleChange = (e: SmartStringEvent) => {
      dispatch(SectionUpdateEvent.Title, e);
    };

    const yesToggle = (e: MouseEvent) => {
      dispatch(SectionUpdateEvent.YesChecked, null);
    };

    const noToggle = (e: MouseEvent) => {
      dispatch(SectionUpdateEvent.NoChecked, null);
    };

    const onDescriptionChange = (e: SmartStringEvent) => {
      dispatch(SectionUpdateEvent.Description, e);
    };

    const onSectionShortNameChange = (e: SmartStringEvent) => {
      dispatch(SectionUpdateEvent.ShortName, e);
    };

    const onContinueLabelChange = (e: SmartStringEvent) => {
      dispatch(SectionUpdateEvent.ContinueLabel, e);
    };

    return (
      <fieldset hidden={!isPanelVisible} class="panel">
        <legend class="panel">Page</legend>
        <div hidden={!(showAllSectionFields || showCaption)}>
          <SmartStringInput id="edit-caption" value={sectionState.caption} onKeyUp={onCaptionChange}>
            Caption
          </SmartStringInput>
        </div>
        <div hidden={!(showAllSectionFields || showTitle)}>
          <SmartStringInput id="edit-title" value={sectionState.title} onKeyUp={onTitleChange}>
            Title
          </SmartStringInput>
        </div>
        <div hidden={!showAllSectionFields}>
          Display in summary:
          <input type="radio" id="yes" checked={sectionState.yesChecked} onClick={yesToggle} />
          <label for="yes">Yes</label>
          <input type="radio" id="no" checked={sectionState.noChecked} onClick={noToggle} />
          <label for="no">No</label>
        </div>
        <div hidden={!(showAllSectionFields || showDescription)}>
          <SmartStringInput id="edit-description" value={sectionState.description} onKeyUp={onDescriptionChange}>
            Description
          </SmartStringInput>
        </div>
        <div hidden={!showAllSectionFields}>
          <SmartStringInput
            id="edit-section-shortName"
            value={sectionState.shortName}
            onKeyUp={onSectionShortNameChange}
          >
            Short name
          </SmartStringInput>
        </div>
        <div hidden={!showAllSectionFields}>
          <SmartStringInput id="edit-continueLabel" value={sectionState.continueLabel} onKeyUp={onContinueLabelChange}>
            Continue label
          </SmartStringInput>
        </div>
      </fieldset>
    );
  };
