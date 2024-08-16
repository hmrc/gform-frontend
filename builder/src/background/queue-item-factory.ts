import {
  TaskSectionUpdateRequest,
  TaskSubmitSectionUpdateRequest,
  NoteUpdateRequest,
  FormTemplateUpdateRequest,
  SectionUpdateRequest,
  AcknowledgementRequest,
  AddAnotherQuestionPart,
  AtlCyaPageRequest,
  AtlDefaultPageRequest,
  AtlRepeater,
  AtlRepeaterAddAnotherQuestionRequest,
  AtlRepeaterNoteRequest,
  AtlRepeaterRequest,
  Coordinates,
  FormComponentId,
  FormComponentPart,
  FormComponentUpdateRequest,
  QueueItem,
  SectionNumber,
  SummarySection,
  SectionDetails,
  SummarySectionRequest,
  UpdateQueueItem,
  UpdateRenderQueueItem,
} from "../types";
import { fullFormComponentId } from "../pure-functions";

export const atlDefaultPageQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const atlDefaultPageRequest: AtlDefaultPageRequest = request.data;
  const atlRepeater: AtlRepeater = atlDefaultPageRequest.payload;
  const sectionNumber = SectionNumber.asString(atlDefaultPageRequest.sectionNumber);
  const sectionPath = encodeURIComponent(atlDefaultPageRequest.sectionPath);
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-atl-default-page/${formTemplateId}?sectionPath=${sectionPath}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-atl-default-page/${formTemplateId}/${sectionNumber}`,
  );

  if (atlDefaultPageRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", atlDefaultPageRequest.maybeAccessCode);
  }

  const renderUrl = renderUrlURL.toString();

  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: atlRepeater,
    sendResponse: item.sendResponse,
  };
};

export const atlCyaPageQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const atlCyaPageRequest: AtlCyaPageRequest = request.data;
  const atlRepeater: AtlRepeater = atlCyaPageRequest.payload;
  const sectionNumber = SectionNumber.asString(atlCyaPageRequest.sectionNumber);
  const sectionPath = encodeURIComponent(atlCyaPageRequest.sectionPath);
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-atl-cya-page/${formTemplateId}?sectionPath=${sectionPath}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-atl-cya-page/${formTemplateId}/${sectionNumber}`,
  );

  if (atlCyaPageRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", atlCyaPageRequest.maybeAccessCode);
  }

  const renderUrl = renderUrlURL.toString();
  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: atlRepeater,
    sendResponse: item.sendResponse,
  };
};

export const atlRepeaterQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const atlRepeaterRequest: AtlRepeaterRequest = request.data;
  const atlRepeater: AtlRepeater = atlRepeaterRequest.payload;
  const sectionNumber = SectionNumber.asString(atlRepeaterRequest.sectionNumber);
  const sectionNumberAfterUpdate = SectionNumber.asString(atlRepeaterRequest.sectionNumberAfterUpdate);
  const sectionPath = encodeURIComponent(atlRepeaterRequest.sectionPath);
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-atl-repeater/${formTemplateId}?sectionPath=${sectionPath}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-atl-repeater/${formTemplateId}/${sectionNumberAfterUpdate}`,
  );

  if (atlRepeaterRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", atlRepeaterRequest.maybeAccessCode);
  }

  const renderUrl = renderUrlURL.toString();

  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: atlRepeater,
    sendResponse: item.sendResponse,
  };
};

export const atlRepeaterAddAnotherQuestionQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const atlRepeaterRequest: AtlRepeaterAddAnotherQuestionRequest = request.data;
  const atlRepeater: AddAnotherQuestionPart = atlRepeaterRequest.payload;
  const sectionNumber = SectionNumber.asString(atlRepeaterRequest.sectionNumber);
  const sectionPath = encodeURIComponent(atlRepeaterRequest.sectionPath);
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-atl-repeater/add-another-question/${formTemplateId}?sectionPath=${sectionPath}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-atl-repeater/add-another-question/${formTemplateId}/${sectionNumber}`,
  );
  if (atlRepeaterRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", atlRepeaterRequest.maybeAccessCode);
  }
  const renderUrl = renderUrlURL.toString();
  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: atlRepeater,
    sendResponse: item.sendResponse,
  };
};

export const atlRepeaterFormComponentQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const updateRequest: FormComponentUpdateRequest = request.data;
  const payload: FormComponentPart = updateRequest.payload;
  const sectionNumber = SectionNumber.asString(updateRequest.sectionNumber);
  const formComponentId = payload.id;
  const sectionPath = encodeURIComponent(updateRequest.requestData as string);
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-atl-repeater/form-component/${formTemplateId}/${formComponentId}?sectionPath=${sectionPath}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-component-html-atl-repeater/${formTemplateId}/${sectionNumber}/${formComponentId}`,
  );
  if (updateRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", updateRequest.maybeAccessCode);
  }
  const renderUrl = renderUrlURL.toString();
  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: payload,
    sendResponse: item.sendResponse,
  };
};

export const acknowledgementSectionQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const updateRequest: AcknowledgementRequest = request.data;
  const payload = updateRequest.payload;

  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-acknowledgement/${formTemplateId}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-acknowledgement-panel-html/${formTemplateId}`,
  );

  if (updateRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", updateRequest.maybeAccessCode);
  }

  const renderUrl = renderUrlURL.toString();

  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: payload,
    sendResponse: item.sendResponse,
  };
};

export const updateFormComponentQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const updateRequest: FormComponentUpdateRequest = request.data;

  const sectionNumber = updateRequest.sectionNumber;
  const payload = updateRequest.payload;
  const formComponentId = updateRequest.payload.id || "missing-form-component-id";

  const sectionNumberStr = SectionNumber.asString(sectionNumber);

  const inputFormComponentId = fullFormComponentId(formComponentId, updateRequest.atlIterationIndex);
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-form-component/${formTemplateId}/${formComponentId}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-section-and-component-html/${formTemplateId}/${sectionNumberStr}/${inputFormComponentId}`,
  );

  if (updateRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", updateRequest.maybeAccessCode);
  }

  const renderUrl = renderUrlURL.toString();

  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: payload,
    sendResponse: item.sendResponse,
  };
};

export const updateSectionTitleQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const updateRequest: SectionUpdateRequest = request.data;
  const sectionNumber = updateRequest.sectionNumber;
  const payload: SectionDetails = updateRequest.section;

  const sectionNumberStr = SectionNumber.asString(sectionNumber);

  const formComponentId: FormComponentId = updateRequest.formComponentId;

  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-header/${formTemplateId}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-section-and-component-html/${formTemplateId}/${sectionNumberStr}/${formComponentId}`,
  );

  if (updateRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", updateRequest.maybeAccessCode);
  }

  const renderUrl = renderUrlURL.toString();

  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: payload,
    sendResponse: item.sendResponse,
  };
};

export const acknowledgementSectionNoteQueueItem = (item: QueueItem): UpdateQueueItem => {
  const request = item.request;
  const updateRequest: AcknowledgementRequest = request.data;
  const payload = updateRequest.payload;
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-acknowledgement/${formTemplateId}`;

  return {
    kind: "update",
    updateUrl,
    updatePayload: payload,
    sendResponse: item.sendResponse,
  };
};

export const acknowledgementSectionFormComponentQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const updateRequest: FormComponentUpdateRequest = request.data;
  const payload = updateRequest.payload;

  const formComponentId = payload.id;
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-acknowledgement-form-component/${formTemplateId}/${formComponentId}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-component-html-acknowledgement-section/${formTemplateId}/${formComponentId}`,
  );

  if (updateRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", updateRequest.maybeAccessCode);
  }

  const renderUrl = renderUrlURL.toString();
  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: payload,
    sendResponse: item.sendResponse,
  };
};

export const processSummarySectionQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const summarySectionRequest: SummarySectionRequest = request.data;
  const summarySection: SummarySection = summarySectionRequest.payload;
  const coordinates: Coordinates | null = summarySectionRequest.coordinates;
  const formTemplateId = request.formTemplateId;

  const updateUrlURL = new URL(
    `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-summary-section/${formTemplateId}`,
  );
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-summary-section/${formTemplateId}`,
  );

  if (coordinates !== null) {
    const c = coordinates.taskSectionNumber + "," + coordinates.taskNumber;
    updateUrlURL.searchParams.set("c", c);
    renderUrlURL.searchParams.set("c", c);
  }

  if (summarySectionRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", summarySectionRequest.maybeAccessCode);
  }

  const updateUrl = updateUrlURL.toString();
  const renderUrl = renderUrlURL.toString();

  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: summarySection,
    sendResponse: item.sendResponse,
  };
};

export const summarySectionNoteQueueItem = (item: QueueItem): UpdateQueueItem => {
  const request = item.request;
  const summarySectionRequest: SummarySectionRequest = request.data;
  const summarySection: SummarySection = summarySectionRequest.payload;
  const coordinates: Coordinates | null = summarySectionRequest.coordinates;
  const params: string = coordinates !== null ? `?c=${coordinates.taskSectionNumber},${coordinates.taskNumber}` : "";
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-summary-section/${formTemplateId}${params}`;

  return {
    kind: "update",
    updateUrl,
    updatePayload: summarySection,
    sendResponse: item.sendResponse,
  };
};

export const atlRepeaterNoteQueueItem = (item: QueueItem): UpdateQueueItem => {
  const request = item.request;
  const atlRepeaterRequest: AtlRepeaterNoteRequest = request.data;
  const atlRepeater: AtlRepeater = atlRepeaterRequest.payload;
  const sectionPath: string = atlRepeaterRequest.sectionPath;
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-atl-repeater/${formTemplateId}?sectionPath=${sectionPath}`;

  return {
    kind: "update",
    updateUrl,
    updatePayload: atlRepeater,
    sendResponse: item.sendResponse,
  };
};

export const atlDefaultPageNoteQueueItem = (item: QueueItem): UpdateQueueItem => {
  const request = item.request;
  const atlRepeaterRequest: AtlRepeaterNoteRequest = request.data;
  const atlRepeater: AtlRepeater = atlRepeaterRequest.payload;
  const sectionPath: string = atlRepeaterRequest.sectionPath;
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-atl-default-page/${formTemplateId}?sectionPath=${sectionPath}`;

  return {
    kind: "update",
    updateUrl,
    updatePayload: atlRepeater,
    sendResponse: item.sendResponse,
  };
};

export const atlCyaPageNoteQueueItem = (item: QueueItem): UpdateQueueItem => {
  const request = item.request;
  const atlRepeaterRequest: AtlRepeaterNoteRequest = request.data;
  const atlRepeater: AtlRepeater = atlRepeaterRequest.payload;
  const sectionPath: string = atlRepeaterRequest.sectionPath;
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-atl-cya-page/${formTemplateId}?sectionPath=${sectionPath}`;

  return {
    kind: "update",
    updateUrl,
    updatePayload: atlRepeater,
    sendResponse: item.sendResponse,
  };
};

export const atlDefaultPageFormComponentQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const updateRequest: FormComponentUpdateRequest = request.data;
  const payload: FormComponentPart = updateRequest.payload;
  const sectionNumber = SectionNumber.asString(updateRequest.sectionNumber);
  const formComponentId = payload.id;
  const sectionPath = encodeURIComponent(updateRequest.requestData as string);
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-atl-default-page/form-component/${formTemplateId}/${formComponentId}?sectionPath=${sectionPath}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-component-html-atl-default-page/${formTemplateId}/${sectionNumber}/${formComponentId}`,
  );

  if (updateRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", updateRequest.maybeAccessCode);
  }

  const renderUrl = renderUrlURL.toString();

  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: payload,
    sendResponse: item.sendResponse,
  };
};

export const summarySectionFormComponentQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;
  const updateRequest: FormComponentUpdateRequest = request.data;
  const payload = updateRequest.payload;
  const sectionNumber: SectionNumber = updateRequest.sectionNumber;

  const formComponentId = payload.id;
  const formTemplateId = request.formTemplateId;

  const updateUrlURL = new URL(
    `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-summary-section-form-component/${formTemplateId}/${formComponentId}`,
  );
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-component-html-summary-section/${formTemplateId}/${formComponentId}`,
  );

  if (SectionNumber.isTaskListSectionNumber(sectionNumber)) {
    const c = SectionNumber.asString(sectionNumber);
    updateUrlURL.searchParams.set("c", c);
    renderUrlURL.searchParams.set("c", c);
  }

  if (updateRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", updateRequest.maybeAccessCode);
  }

  const updateUrl = updateUrlURL.toString();
  const renderUrl = renderUrlURL.toString();

  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: payload,
    sendResponse: item.sendResponse,
  };
};

export const formTemplateQueueItem = (item: QueueItem): UpdateQueueItem => {
  const request = item.request;
  const updateRequest: FormTemplateUpdateRequest = request.data;
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-form-template/${formTemplateId}`;

  return {
    kind: "update",
    updateUrl,
    updatePayload: updateRequest,
    sendResponse: item.sendResponse,
  };
};

export const sectionUpdateNoteQueueItem = (item: QueueItem): UpdateQueueItem => {
  const request = item.request;
  const updateRequest: NoteUpdateRequest = request.data;
  const section: SectionDetails = updateRequest.section;
  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-header/${formTemplateId}`;

  return {
    kind: "update",
    updateUrl,
    updatePayload: section,
    sendResponse: item.sendResponse,
  };
};

export const updateTaskQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;

  const updateRequest: TaskSectionUpdateRequest = request.data;
  const sectionNumber = SectionNumber.asString(updateRequest.sectionNumber);
  const payload = updateRequest.batch;

  const formTemplateId = request.formTemplateId;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-batch/${formTemplateId}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-task-list/${formTemplateId}/${sectionNumber}`,
  );

  if (updateRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", updateRequest.maybeAccessCode);
  }

  const renderUrl = renderUrlURL.toString();

  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: payload,
    sendResponse: item.sendResponse,
  };
};

export const updateSubmitSectionQueueItem = (item: QueueItem): UpdateRenderQueueItem => {
  const request = item.request;

  const updateRequest: TaskSubmitSectionUpdateRequest = request.data;
  const formTemplateId = request.formTemplateId;
  const payload = updateRequest.batch;

  const updateUrl = `${request.host}/submissions/test-only/proxy-to-gform/gform/builder/update-batch/${formTemplateId}`;
  const renderUrlURL = new URL(
    `${request.host}/submissions/test-only/builder/generate-submit-section/${formTemplateId}`,
  );

  if (updateRequest.maybeAccessCode !== undefined) {
    renderUrlURL.searchParams.set("a", updateRequest.maybeAccessCode);
  }

  const renderUrl = renderUrlURL.toString();

  return {
    kind: "updateRender",
    updateUrl,
    renderUrl,
    updatePayload: payload,
    sendResponse: item.sendResponse,
  };
};
