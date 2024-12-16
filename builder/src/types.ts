import { Signal } from "@preact/signals";

export type FormTemplateId = string;
export type FormComponentId = string;

interface NormalPage {
  type: "normalPage";
  sectionNumber: number;
}

interface AddToListPage {
  type: "addToListPage";
  sectionNumber: number;
  iterationNumber: number;
  pageNumber: number;
}

interface AddToListDefaultPage {
  type: "addToListDefaultPage";
  sectionNumber: number;
}

interface AddToListCyaPage {
  type: "addToListCyaPage";
  sectionNumber: number;
  iterationNumber: number;
}

interface AddToListRepeaterPage {
  type: "addToListRepeaterPage";
  sectionNumber: number;
  iterationNumber: number;
}

interface RepeatedPage {
  type: "repeatedPage";
  sectionNumber: number;
  pageNumber: number;
}

interface TaskListSectionNumber {
  type: "taskListSectionNumber";
  taskSectionNumber: number;
  taskNumber: number;
  sectionNumber: SectionNumber;
}

interface AcknowledgementSectionNumber {
  type: "acknowledgement";
}

export type SectionNumber =
  | NormalPage
  | AddToListDefaultPage
  | AddToListPage
  | AddToListCyaPage
  | AddToListRepeaterPage
  | RepeatedPage
  | TaskListSectionNumber
  | AcknowledgementSectionNumber;

export const SectionNumber = {
  NormalPage: (n: number): NormalPage => ({ type: "normalPage", sectionNumber: n }),
  AddToListDefaultPage: (n: number): AddToListDefaultPage => ({
    type: "addToListDefaultPage",
    sectionNumber: n,
  }),
  AddToListPage: (n: number, iteration: number, page: number): AddToListPage => ({
    type: "addToListPage",
    sectionNumber: n,
    iterationNumber: iteration,
    pageNumber: page,
  }),
  AddToListCyaPage: (n: number, iteration: number): AddToListCyaPage => ({
    type: "addToListCyaPage",
    sectionNumber: n,
    iterationNumber: iteration,
  }),
  AddToListRepeaterPage: (n: number, iteration: number): AddToListRepeaterPage => ({
    type: "addToListRepeaterPage",
    sectionNumber: n,
    iterationNumber: iteration,
  }),
  RepeatedPage: (n: number, page: number): RepeatedPage => ({
    type: "repeatedPage",
    sectionNumber: n,
    pageNumber: page,
  }),
  TaskListSectionNumber: (
    taskSectionNumber: number,
    taskNumber: number,
    sectionNumber: SectionNumber,
  ): TaskListSectionNumber => ({ type: "taskListSectionNumber", taskSectionNumber, taskNumber, sectionNumber }),
  AcknowledgementSectionNumber: (): AcknowledgementSectionNumber => ({
    type: "acknowledgement",
  }),

  asString: (sn: SectionNumber): string => {
    if (sn.type === "normalPage") {
      return "n" + sn.sectionNumber;
    } else if (sn.type === "addToListDefaultPage") {
      return "ad" + sn.sectionNumber;
    } else if (sn.type === "addToListPage") {
      return "ap" + sn.sectionNumber + "." + sn.iterationNumber + "." + sn.pageNumber;
    } else if (sn.type === "addToListCyaPage") {
      return "ac" + sn.sectionNumber + "." + sn.iterationNumber;
    } else if (sn.type === "addToListRepeaterPage") {
      return "ar" + sn.sectionNumber + "." + sn.iterationNumber;
    } else if (sn.type === "repeatedPage") {
      return "r" + sn.sectionNumber + "." + sn.pageNumber;
    } else if (sn.type === "taskListSectionNumber") {
      return sn.taskSectionNumber + "," + sn.taskNumber + "," + SectionNumber.asString(sn.sectionNumber);
    } else {
      return "";
    }
  },

  isRegularListSectionNumber: (sn: SectionNumber): boolean => {
    const t = sn.type;
    return (
      t === "normalPage" ||
      t === "addToListDefaultPage" ||
      t === "addToListPage" ||
      t === "addToListCyaPage" ||
      t === "addToListRepeaterPage" ||
      t === "repeatedPage"
    );
  },
  isTaskListSectionNumber: (sn: SectionNumber): boolean => !SectionNumber.isRegularListSectionNumber(sn),
  //isAcknowledgementSectionNumber: (sn: SectionNumber): boolean => "a" in sn && sn.a === "acknowledgement",
};

export enum MessageKind {
  FetchSection,
  UpdateSectionTitle,
  QuerySectionTitle,
  UpdateFormComponent,
  FetchFormTemplate,
  UpdateNote,
  UpdateTask,
  UpdateSubmitSection,
  UpdateFormTemplate,
  SummarySection,
  UpdateSummarySection,
  UpdateSummarySectionNote,
  UpdateSummarySectionFormComponent,
  AcknowledgementSection,
  UpdateAcknowledgementNote,
  UpdateAcknowledgement,
  UpdateAcknowledgementFormComponent,
  UpdateAtlDefaultPageNote,
  UpdateAtlDefaultPageFormComponent,
  UpdateAtlDefaultPage,
  UpdateAtlCyaPageNote,
  UpdateAtlCyaPage,
  UpdateAtlRepeater,
  UpdateAtlRepeaterNote,
  UpdateAtlRepeaterAddAnotherQuestion,
  UpdateAtlRepeaterFormComponent,
}

export interface SummarySectionOriginalRequest {
  coordinates: Coordinates | null;
  maybeAccessCode?: string;
}

export interface SummarySectionRequest {
  payload: SummarySection;
  coordinates: Coordinates | null;
  maybeAccessCode?: string;
}

export interface AtlDefaultPageRequest {
  payload: DefaultPagePart;
  sectionNumber: SectionNumber;
  sectionPath: string;
  maybeAccessCode?: string;
}

export interface AtlCyaPageRequest {
  payload: CyaPagePart;
  sectionNumber: SectionNumber;
  sectionPath: string;
  maybeAccessCode?: string;
}

export interface AtlRepeaterAddAnotherQuestionRequest {
  payload: AddAnotherQuestionPart;
  sectionNumber: SectionNumber;
  sectionPath: string;
  maybeAccessCode?: string;
}

export interface AtlRepeaterRequest {
  payload: AtlRepeater;
  sectionNumber: SectionNumber;
  sectionNumberAfterUpdate: SectionNumber; // When adding/removing defaultPage/cyaPage sectionNumber is changing
  sectionPath: string;
  maybeAccessCode?: string;
}

export interface AtlRepeaterNoteRequest {
  payload: AtlRepeater;
  sectionPath: string;
}

export interface AtlDefaultPageNoteRequest {
  payload: DefaultPagePart;
  sectionPath: string;
}

export interface AtlCyaPageNoteRequest {
  payload: CyaPagePart;
  sectionPath: string;
}

export interface SummarySection {
  title?: SmartString;
  header?: string;
  footer?: string;
  displayWidth?: string;
  keyDisplayWidth?: string;
  continueLabel?: SmartString;
  note?: NoteInfo[] | string;
  fields?: FormComponent[];
  doneNote?: string[];
  error?: string;
}

export interface AcknowledgementRequest {
  payload: AcknowledgementSection;
  maybeAccessCode?: string;
}

export interface AcknowledgementSection {
  title?: SmartString;
  showReference?: boolean;
  note?: NoteInfo[] | string;
  fields?: FormComponent[];
  doneNote?: string[];
  error?: string;
}

export interface ContentScriptRequest {
  host: string;
  formTemplateId: FormTemplateId;
  kind: MessageKind;
  data?: any;
}

export interface FetchSectionRequest {
  sectionNumber: SectionNumber;
  maybeAccessCode?: string;
}

export interface SectionUpdateRequest {
  section: SectionDetails;
  sectionNumber: SectionNumber;
  formComponentId: FormComponentId; // Needed when Title and Label happens to be the same
  maybeAccessCode?: string;
}

export interface NoteUpdateRequest {
  section: SectionDetails;
}

export interface NoteInfo {
  noteText: string;
  position: { x: number; y: number };
  size: { width: number; height: number };
  color?: string;
  zIndex: number;
}

export interface NoteInfoState extends NoteInfo {
  isDeleted: boolean;
}

export interface Section {
  title?: SmartString;
  caption?: SmartString;
  description?: SmartString;
  shortName?: SmartString;
  continueLabel?: SmartString;
  presentationHint?: string;
  note?: NoteInfo[] | string;
  fields: FormComponent[];
  doneNote?: string[];
}

export interface AddAnotherQuestion {
  id: FormComponentId;
  label: SmartString;
  errorMessage?: SmartString;
}

export type AddAnotherQuestionPart = Partial<AddAnotherQuestion>;

export interface DefaultPage {
  caption?: SmartString;
  title: SmartString;
  fields: FormComponent[];
  continueLabel?: SmartString;
  note?: NoteInfo[] | string;
  doneNote?: string[];
}

export type DefaultPagePart = Partial<DefaultPage>;

export interface CyaPage {
  caption?: SmartString;
  title?: SmartString;
  updateTitle: SmartString;
  noPIITitle?: SmartString;
  noPIIUpdateTitle?: SmartString;
  header?: string;
  footer?: string;
  presentationHint?: string;
  removeItemIf?: string;
  continueLabel?: SmartString;
  note?: NoteInfo[] | string;
  doneNote?: string[];
  displayWidth?: string;
  keyDisplayWidth?: string;
}

export type CyaPagePart = Partial<CyaPage>;

export interface AtlRepeater {
  title?: SmartString;
  caption?: SmartString;
  description?: SmartString;
  shortName?: SmartString;
  summaryName?: SmartString;
  summaryDescription?: SmartString;
  continueLabel?: SmartString;
  presentationHint?: string;
  repeatsWhile?: string;
  repeatsUntil?: string;
  pageIdToDisplayAfterRemove?: string;
  defaultPage?: DefaultPage | string; // string to set it empty when deleting
  cyaPage?: CyaPage | string; // string to set it empty when deleting
  addAnotherQuestion?: AddAnotherQuestion;
  note?: NoteInfo[] | string;
  fields?: FormComponent[];
  doneNote?: string[];
}

export type SectionPart = Partial<Section>;

export interface FormComponentUpdateRequest {
  payload: FormComponentPart;
  sectionNumber: SectionNumber;
  maybeAccessCode?: string;
  atlIterationIndex?: number;
  requestData?: any;
}

export type FormTemplate = {
  _id: FormTemplateId;
  formName: String;
  displayWidth?: string;
  sections: [Section] | [TaskSection];
  authConfig: any;
  declarationSection: any;
  acknowledgementSection: any;
  submitSection: SubmitSection;
  note?: NoteInfo[] | string;
  doneNote?: string[];
};

export type FormTemplatePart = Partial<FormTemplate>;

export type ChoiceObject = {
  en: string;
  cy?: string;
  hint?: string | object;
  includeIf?: string;
  value?: string;
};

export type FormComponent = {
  id: FormComponentId;
  type: string;
  label?: SmartString;
  shortName?: SmartString;
  helpText?: SmartString;
  format?: string;
  errorShortName?: SmartString;
  errorShortNameStart?: SmartString;
  errorExample?: SmartString;
  errorMessage?: SmartString;
  displayWidth?: string;
  labelSize?: string;
  mandatory?: boolean | "";
  infoText?: SmartString;
  infoType?: string;
  multivalue?: boolean | "";
  multiline?: boolean | "";
  dividerPosition?: number | string;
  dividerText?: SmartString;
  noneChoice?: number | string;
  noneChoiceError?: SmartString;
  choices?: string[] | ChoiceObject[] | string;
  hints?: string[] | string;
  cityMandatory?: boolean | "";
  countyDisplayed?: boolean | "";
  line2Mandatory?: boolean | "";
  postcodeMandatory?: boolean | "";
  countryLookup?: boolean | "";
  countryDisplayed?: boolean | "";
  submitMode?: string;
};

export type FormComponentPart = Partial<FormComponent>;

export interface FormComponentFromInput extends Function {
  (n: HTMLInputElement): FormComponent;
}

export type UpdateSubmitSection = {
  error?: string;
  label?: string;
  taskLabel?: string;
};

export type UpdateHtmlResponse = {
  error?: string;
  html?: string;
  sectionHtml?: string;
  formLevelHeading?: boolean;
};

export interface HiddenChoicesLookup {
  [key: string]: number[];
}

export type ServerPageData = {
  section: Section;
  atlIterationIndex?: number;
  atlRepeater?: boolean;
  atlDefaultPage?: boolean;
  atlCyaPage?: boolean;
  sectionPath: string;
  hiddenComponentIds?: string[];
  hiddenChoicesLookup?: HiddenChoicesLookup;
};

export type ServerFormTemplateData = {
  formTemplate: FormTemplatePart;
  version: number;
  error?: string;
};

export type APIResponse = {
  ok: boolean;
  error?: string;
};

export type TextClickable = {
  parentEl: HTMLDivElement;
  labelEl: HTMLLabelElement;
  helpTextEl: HTMLDivElement | null;
  characterCountMessage: HTMLDivElement | null; // Used by text area for "You can enter up to 1000 characters" message
};

export type FileUploadClickable = TextClickable;

export type ChoiceClickable = {
  parentEl: HTMLDivElement;
  labelEl: HTMLLegendElement;
  eachChoiceEl: NodeListOf<HTMLInputElement>;
  helpTextEl: HTMLDivElement | null;
};

export type DateClickable = {
  parentEl: HTMLDivElement;
  labelEl: HTMLLegendElement;
  helpTextEl: HTMLDivElement | null;
};

export type AddressClickable = {
  parentEl: HTMLFieldSetElement;
  labelEl: HTMLLegendElement;
  helpTextEl: HTMLDivElement | null;
};

export enum ChoiceComponentType {
  YesNo = "yesno",
  Checkboxes = "checkboxes",
  Radios = "radios",
}

export type SectionDetails = {
  section: SectionPart | TaskSection | Task;
  sectionPath: string;
};

export interface FormTemplateUpdateRequest {
  formTemplate: FormComponentPart;
}

export interface TaskSectionUpdateRequest {
  batch: TemplateBatchUpdate;
  sectionNumber: SectionNumber;
  maybeAccessCode?: string;
}

export interface TaskSubmitSectionUpdateRequest {
  batch: TemplateBatchUpdate;
  maybeAccessCode?: string;
}

export interface TaskSection {
  title: SmartString;
  caption?: SmartString;
  displayWidth?: string;
  tasks: Task[];
}

export type TaskSectionPart = Partial<TaskSection>;

export interface Task {
  title: SmartString;
  sections: Section[];
  caption?: string;
  summarySection?: TaskSummarySection | string;
  declarationSection?: TaskDeclarationSection | string;
}

export interface TaskSummarySection {
  title: string;
  caption?: string;
  header: string;
  footer: string;
}

export interface TaskDeclarationSection {
  title: string;
  fields: FormComponent[];
}

export type TaskSummarySectionPart = Partial<TaskSummarySection>;

export type TaskDeclarationSectionPart = Partial<TaskDeclarationSection>;

export interface SubmitSection {
  label: SmartString;
  taskLabel: SmartString;
}

export type SubmitSectionPart = Partial<SubmitSection>;

export type SummarySectionTitleClickable = {
  title: HTMLElement;
};

export type SummarySectionHeaderClickable = {
  header: HTMLDivElement;
};

export type SummarySectionFooterClickable = {
  footer: HTMLDivElement;
};

export interface UpdateSummarySectionTitleResponse {
  error?: string;
  title?: string;
  header?: string;
  footer?: string;
}

export interface UpdateAtlDefaultPageResponse {
  error?: string;
  pageHeading?: string;
  continueLabel?: string;
}

// When error is set, no other fieds are present.
export interface UpdateAtlCyaPageResponse {
  error?: string;
  pageHeading: string;
  continueLabel: string;
  header: string;
  footer: string;
  noPIITitle: string;
  summaryTable: string;
}

export interface UpdateAtlCyaPageHeaderResponse {
  error?: string;
  header?: string;
}

export interface UpdateAtlRepeaterResponse {
  error?: string;
  pageHeading?: string;
  descriptions?: [string];
}

export interface UpdateAtlRepeaterAddAnotherQuestionResponse {
  error?: string;
  label?: string;
}

export interface UpdateAcknowledgementResponse {
  error?: string;
  panelHtml?: string;
}

export type AcknowledgementTitleClickable = {
  title: HTMLElement;
};

export interface InfoRenderParam {
  host: string;
  formTemplateId: FormTemplateId;
  sectionNumber: SectionNumber;
  kind: MessageKind;
  atlIterationIndex?: number;
  sectionNumberChange?: Signal<SectionNumber | undefined>;
  requestData?: any;
}

export enum NoteUpdateKind {
  FormTemplate,
  Section,
  SummarySection,
  AcknowledgementSection,
  AddToListRepeater,
  AddToListDefaultPage,
  AddToListCyaPage,
}

export type TaskSectionNumber = number;
export type TaskNumber = number;

export interface Coordinates {
  taskSectionNumber: TaskSectionNumber;
  taskNumber: TaskNumber;
}

export interface QueueItem {
  request: ContentScriptRequest;
  sendResponse: (response?: any) => void;
}

export interface UpdateRenderQueueItem {
  kind: "updateRender";
  updateUrl: string;
  renderUrl: string;
  updatePayload: any;
  sendResponse: (response?: any) => void;
}

export interface UpdateQueueItem {
  kind: "update";
  updateUrl: string;
  updatePayload: any;
  sendResponse: (response?: any) => void;
}

export interface TemplateBatchUpdate {
  updates: UpdateByPath[];
}

export interface UpdateByPath {
  payload: any;
  path: string;
  focus: "taskSection" | "task" | "taskSummarySection" | "taskDeclarationSection" | "submitSection"; // Hint for the backend to indicate what is referred by path
}

export type UpdateTaskList = {
  error?: string;
  taskSectionTitle?: string;
  taskTitles?: string[];
  taskCaptions?: string[];
};

export type SmartStringCondition = {
  includeIf?: string;
  en: string;
};

export type SmartStringObj = {
  en: string;
};

export type SmartString = string | SmartStringObj | SmartStringCondition[];

export interface ExclusiveFieldVisibility {
  field: FieldVisibility;
  index?: number; // use for choices to display only single choice fields
}

export enum FieldVisibility {
  Caption, // Section only field
  Title, // Section only field
  Description, // Section only field
  Label,
  HelpText,
  InfoText,
  Choice,
  AllSection, // All Section fields
  AllComponent,
  None,
}

export enum ChoiceComponentPanelVisibility {
  Label,
  HelpText,
  Choice,
  All,
  None,
}

export enum FieldInteractionType {
  // This denotes panel which toggles between section fields and
  // form component fields and which allows to use title as a field label

  // For example single component page
  TitleLabelWithSync,

  // This denotes panel which toggles between section fields and
  // form component fields, but label can't be used as page title

  // For example first component on two components page
  TitleLabelNoSync,

  // This denotes panel which controls only form component fields

  // For example second component on two components page
  ComponentOnly,
}

export interface SectionState {
  caption?: SmartString;
  title?: SmartString;
  description?: SmartString;
  shortName?: SmartString;
  continueLabel?: SmartString;
  yesChecked: boolean;
  noChecked: boolean;
  undo?: SectionState;
}

export enum SectionUpdateEvent {
  Caption,
  Title,
  Description,
  ShortName,
  ContinueLabel,
  YesChecked,
  NoChecked,
}

export interface TextState {
  label?: SmartString;
  pageHeading: boolean;
  helpText?: SmartString;
  shortName?: SmartString;
  format: string;
  formatParam1?: string;
  formatParam2?: string;
  errorShortName?: SmartString;
  errorShortNameStart?: SmartString;
  errorExample?: SmartString;
  errorMessage?: SmartString;
  displayWidth?: string;
  labelSize?: string;
  optional: boolean;
  isLookup: boolean;
  lookup?: string;
  isMultiline: boolean;
  undo?: TextState;
}

export interface State<T> {
  section: SectionState;
  component: T;
}

export interface ChoiceState {
  typeValue: ChoiceComponentType;
  label?: SmartString;
  pageHeading: boolean;
  choices: string[];
  hints: string[];
  values: string[];
  includeIfs: string[];
  helpText?: SmartString;
  shortName?: SmartString;
  errorShortName?: SmartString;
  errorMessage?: SmartString;
  optional: boolean;
  dividerPosition?: number | string;
  dividerText?: SmartString;
  noneChoice?: number | string;
  noneChoiceError?: SmartString;
  undo?: ChoiceState;
}

export interface DateState {
  label?: SmartString;
  pageHeading: boolean;
  helpText?: SmartString;
  shortName?: SmartString;
  format?: string;
  errorShortName?: SmartString;
  errorShortNameStart?: SmartString;
  errorExample?: SmartString;
  errorMessage?: SmartString;
  labelSize?: string;
  optional: boolean;
  undo?: DateState;
}

export interface AddressState {
  isOverseasAddress: boolean;
  label?: SmartString;
  pageHeading: boolean;
  helpText?: SmartString;
  shortName?: SmartString;
  errorShortName?: SmartString;
  errorShortNameStart?: SmartString;
  errorExample?: SmartString;
  errorMessage?: SmartString;
  labelSize?: string;
  isCityMandatory: boolean;
  isCountyDisplayed: boolean;
  isLine2Mandatory: boolean;
  isPostcodeMandatory: boolean;
  isCountryLookup: boolean;
  isCountryDisplayed: boolean;
  undo?: AddressState;
}

export interface FileState {
  label?: SmartString;
  pageHeading: boolean;
  helpText?: SmartString;
  shortName?: SmartString;
  errorShortName?: SmartString;
  errorMessage?: SmartString;
  labelSize?: string;
  optional: boolean;
  undo?: FileState;
}

export interface InfoState {
  label?: SmartString;
  infoType?: string;
  infoText?: SmartString;
  undo?: InfoState;
}

export interface NoteState {
  notes: NoteInfoState[];
  doneNotes: string[];
  undo?: NoteState;
}

export enum TextUpdateEvent {
  Label,
  PageHeading,
  HelpText,
  ShortName,
  Format,
  FormatParam1,
  FormatParam1Change,
  FormatParam2,
  FormatParam2Change,
  ErrorShortName,
  ErrorShortNameStart,
  ErrorExample,
  ErrorMessage,
  DisplayWidth,
  LabelSize,
  Optional,
  IsLookup,
  Lookup,
  IsMultiline,
}

export enum ChoiceUpdateEvent {
  Type,
  Label,
  PageHeading,
  HelpText,
  ShortName,
  ErrorShortName,
  ErrorMessage,
  Optional,
  Choice,
  Hint,
  IncludeIf,
  DividerPosition,
  DividerText,
  NoneChoice,
  NoneChoiceError,
}

export enum DateUpdateEvent {
  Label,
  PageHeading,
  HelpText,
  ShortName,
  Format,
  ErrorShortName,
  ErrorShortNameStart,
  ErrorExample,
  ErrorMessage,
  LabelSize,
  Optional,
}

export enum AddressUpdateEvent {
  OverseasAddress,
  Label,
  PageHeading,
  HelpText,
  ShortName,
  Format,
  ErrorShortName,
  ErrorShortNameStart,
  ErrorExample,
  ErrorMessage,
  LabelSize,
  CityMandatory,
  CountyDisplayed,
  Line2Mandatory,
  PostcodeMandatory,
  CountryLookup,
  CountryDisplayed,
}

export enum FileUpdateEvent {
  Label,
  PageHeading,
  HelpText,
  ShortName,
  ErrorShortName,
  ErrorMessage,
  LabelSize,
  Optional,
}

export enum InfoUpdateEvent {
  Label,
  InfoText,
  InfoType,
}

export enum NoteUpdateType {
  NoteText,
  Position,
  Size,
  Color,
  ZIndex,
  Delete,
  Add,
}

export interface NoteUpdateEvent {
  index: number;
  record: Record<string, any>;
  eventType: NoteUpdateType;
}

export interface StateParam {
  formComponent: FormComponent;
  section: Section;
}

export enum DispatchTarget {
  Section,
  Component,
  Undo,
}

export interface DispatchEvent<T> {
  type: DispatchTarget;
  subtype: T;
  record: Record<string, any>;
}

export enum SmartStringFieldType {
  Text,
  IncludeIf,
}

export interface SmartStringEvent {
  index: number;
  fieldType: SmartStringFieldType;
  event: Event;
}

export interface InfoLookupHelper {
  noformatIndexLookup: Record<FormComponentId, number>;
  standardIndexLookup: Record<FormComponentId, number>;
  longIndexLookup: Record<FormComponentId, number>;
  bannerIndexLookup: Record<FormComponentId, number>;
  importantIndexLookup: Record<FormComponentId, number>;
}

export interface InfoSelectorAndIndex {
  selector: string;
  index: number;
}
