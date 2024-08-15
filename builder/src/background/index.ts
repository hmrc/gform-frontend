import {
  Coordinates,
  FetchSectionRequest,
  FormComponentId,
  NoteUpdateRequest,
  ContentScriptRequest,
  FormComponentUpdateRequest,
  SectionUpdateRequest,
  FormTemplateUpdateRequest,
  AcknowledgementRequest,
  SummarySection,
  SummarySectionRequest,
  SummarySectionOriginalRequest,
  QueueItem,
  UpdateQueueItem,
  UpdateRenderQueueItem,
} from "../types";
import { fullFormComponentId } from "../pure-functions";

import { SectionNumber } from "../types";
import { MessageKind } from "../types";

import {
  acknowledgementSectionFormComponentQueueItem,
  acknowledgementSectionQueueItem,
  acknowledgementSectionNoteQueueItem,
  atlCyaPageQueueItem,
  atlDefaultPageQueueItem,
  atlDefaultPageNoteQueueItem,
  atlCyaPageNoteQueueItem,
  atlDefaultPageFormComponentQueueItem,
  atlRepeaterNoteQueueItem,
  atlRepeaterQueueItem,
  atlRepeaterFormComponentQueueItem,
  atlRepeaterAddAnotherQuestionQueueItem,
  formTemplateQueueItem,
  processSummarySectionQueueItem,
  sectionUpdateNoteQueueItem,
  summarySectionNoteQueueItem,
  summarySectionFormComponentQueueItem,
  updateFormComponentQueueItem,
  updateSectionTitleQueueItem,
  updateTaskQueueItem,
  updateSubmitSectionQueueItem,
} from "./queue-item-factory";
import { updateRenderItem, updateRenderQueue } from "./update-and-render-handler";

export async function onMessageHandler<T extends { error?: string }>(
  request: ContentScriptRequest,
  sendResponse: (response: T) => void,
) {
  const runUpdate = async (request: any, f: (queueItem: QueueItem) => UpdateRenderQueueItem | UpdateQueueItem) => {
    const queueItem: QueueItem = {
      request: request,
      sendResponse: sendResponse,
    };

    const item = f(queueItem);

    // Queue incoming events to process them in incoming order to avoid race conditions
    updateRenderQueue.push(item);
    if (updateRenderQueue.length === 1) {
      await updateRenderItem();
    }
  };

  (async function () {
    switch (request.kind) {
      case MessageKind.FetchSection:
        {
          const fetchSectionRequest: FetchSectionRequest = request.data;
          const sectionNumberStr = SectionNumber.asString(fetchSectionRequest.sectionNumber);
          const url = new URL(
            `${request.host}/submissions/test-only/builder/original-section/${request.formTemplateId}/${sectionNumberStr}`,
          );
          if (fetchSectionRequest.maybeAccessCode !== undefined) {
            url.searchParams.set("a", fetchSectionRequest.maybeAccessCode);
          }
          const response = await fetch(url.toString(), {
            method: "GET",
            headers: {
              Accept: "application/json",
            },
          });

          const json = await response.json();

          if (response.ok) {
            sendResponse(json);
          } else {
            const error = `Error when fetching section json: ${json.error}`;
            console.error(error);
            sendResponse({ error: error } as T);
          }
        }
        break;
      case MessageKind.FetchFormTemplate:
        {
          const accessCode = request.data;
          const url = new URL(
            `${request.host}/submissions/test-only/builder/original-form-template/${request.formTemplateId}`,
          );
          if (accessCode !== null) {
            url.searchParams.set("a", accessCode);
          }

          const response = await fetch(url.toString(), {
            method: "GET",
            headers: {
              Accept: "application/json",
            },
          });

          const json = await response.json();

          if (response.ok) {
            sendResponse(json);
          } else {
            const error = `Error when fetching json template: ${json.error}`;
            console.error(error);
            sendResponse({ error } as T);
          }
        }
        break;
      case MessageKind.SummarySection:
        {
          const url = new URL(
            `${request.host}/submissions/test-only/builder/original-summary-section/${request.formTemplateId}`,
          );

          const r: SummarySectionOriginalRequest = request.data;
          const coordinates: Coordinates | null = r.coordinates;
          if (coordinates !== null) {
            url.searchParams.set("c", coordinates.taskSectionNumber + "," + coordinates.taskNumber);
          }

          if (r.maybeAccessCode !== undefined) {
            url.searchParams.set("a", r.maybeAccessCode);
          }

          const response = await fetch(url.toString(), {
            method: "GET",
            headers: {
              Accept: "application/json",
            },
          });

          const json = await response.json();

          if (response.ok) {
            sendResponse(json);
          } else {
            const error = `Error when fetching summary section: ${json.error}`;
            console.error(error);
            sendResponse({ error } as T);
          }
        }
        break;

      case MessageKind.AcknowledgementSection:
        {
          const maybeAccessCode: string | null = request.data;

          const url = new URL(
            `${request.host}/submissions/test-only/builder/original-acknowledgement/${request.formTemplateId}`,
          );

          if (maybeAccessCode !== null) {
            url.searchParams.set("a", maybeAccessCode);
          }

          const response = await fetch(url.toString(), {
            method: "GET",
            headers: {
              Accept: "application/json",
            },
          });

          const json = await response.json();

          if (response.ok) {
            sendResponse(json);
          } else {
            const error = `Error when fetching acknowledgement component: ${json.error}`;
            console.error(error);
            sendResponse({ error } as T);
          }
        }
        break;
      case MessageKind.UpdateAtlRepeaterFormComponent:
        runUpdate(request, atlRepeaterFormComponentQueueItem);
        break;
      case MessageKind.UpdateAtlRepeaterAddAnotherQuestion:
        runUpdate(request, atlRepeaterAddAnotherQuestionQueueItem);
        break;
      case MessageKind.UpdateAtlDefaultPageFormComponent:
        runUpdate(request, atlDefaultPageFormComponentQueueItem);
        break;
      case MessageKind.UpdateAtlDefaultPageNote:
        runUpdate(request, atlDefaultPageNoteQueueItem);
        break;
      case MessageKind.UpdateAtlCyaPageNote:
        runUpdate(request, atlCyaPageNoteQueueItem);
        break;
      case MessageKind.UpdateAtlRepeaterNote:
        runUpdate(request, atlRepeaterNoteQueueItem);
        break;
      case MessageKind.UpdateAtlDefaultPage:
        runUpdate(request, atlDefaultPageQueueItem);
        break;
      case MessageKind.UpdateAtlCyaPage:
        runUpdate(request, atlCyaPageQueueItem);
        break;
      case MessageKind.UpdateAtlRepeater:
        runUpdate(request, atlRepeaterQueueItem);
        break;
      case MessageKind.UpdateSummarySectionNote:
        runUpdate(request, summarySectionNoteQueueItem);
        break;
      case MessageKind.UpdateSummarySection:
        runUpdate(request, processSummarySectionQueueItem);
        break;
      case MessageKind.UpdateSummarySectionFormComponent:
        runUpdate(request, summarySectionFormComponentQueueItem);
        break;
      case MessageKind.UpdateAcknowledgement:
        runUpdate(request, acknowledgementSectionQueueItem);
        break;
      case MessageKind.UpdateAcknowledgementFormComponent:
        runUpdate(request, acknowledgementSectionFormComponentQueueItem);
        break;
      case MessageKind.UpdateAcknowledgementNote:
        runUpdate(request, acknowledgementSectionNoteQueueItem);
        break;
      case MessageKind.UpdateFormComponent:
        runUpdate(request, updateFormComponentQueueItem);
        break;
      case MessageKind.UpdateSectionTitle:
        runUpdate(request, updateSectionTitleQueueItem);
        break;
      case MessageKind.UpdateNote:
        runUpdate(request, sectionUpdateNoteQueueItem);
        break;
      case MessageKind.UpdateTask:
        runUpdate(request, updateTaskQueueItem);
        break;
      case MessageKind.UpdateSubmitSection:
        runUpdate(request, updateSubmitSectionQueueItem);
        break;
      case MessageKind.UpdateFormTemplate:
        runUpdate(request, formTemplateQueueItem);
        break;
    }
  })();
  return true; // Will respond asynchronously.
}
