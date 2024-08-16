import * as styles from "bundle-text:../content.css";
import * as noteStyles from "bundle-text:./note.css";
import { useRef, useState, useReducer, useEffect } from "preact/hooks";
import { Colors } from "./color-picker";

import type {
  APIResponse,
  AtlCyaPageNoteRequest,
  AtlDefaultPageNoteRequest,
  AtlRepeater,
  AtlRepeaterNoteRequest,
  CyaPagePart,
  DefaultPagePart,
  NoteUpdateRequest,
  NoteInfo,
  ContentScriptRequest,
  Coordinates,
  SectionDetails,
  SectionPart,
  ServerPageData,
  FormTemplatePart,
  FormTemplateUpdateRequest,
  SummarySection,
  SummarySectionRequest,
  AcknowledgementSection,
  AcknowledgementRequest,
  NoteState,
} from "../../types";

import { Note } from "./note";
import { MessageKind, NoteUpdateKind, NoteUpdateType, NoteUpdateEvent } from "../../types";

import { noteReducer, NoteProps } from "../note-panel";
import { onMessageHandler } from "../../background/index";

const stateReducer = noteReducer;

export interface InitialNoteData {
  notes: NoteInfo[];
  doneNotes: string[];
}

const initialState = (initialNoteData: InitialNoteData): NoteState => {
  let noteInfoStates = initialNoteData.notes.map((noteInfo) => ({
    ...noteInfo,
    isDeleted: false,
  }));
  return {
    notes: noteInfoStates,
    doneNotes: initialNoteData.doneNotes,
    undo: undefined,
  };
};

export const NoteComponentControllerFactory =
  (host: string, formTemplateId: string, noteData0: NoteInfo[] | string, doneNote: string[]) =>
  ({ noteKind, noteKindData }: NoteProps) => {
    const noteData = convertNoteField(noteData0);

    const initialNotes: InitialNoteData = {
      notes: convertNoteField(noteData),
      doneNotes: doneNote,
    };

    const [state, setState] = useReducer<NoteState, NoteUpdateEvent, InitialNoteData>(
      stateReducer,
      initialNotes,
      initialState,
    );

    const serverError = useRef<HTMLDivElement | null>(null);

    const [showHint, setShowHint] = useState<boolean>(false);
    const [showDoneHint, setShowDoneHint] = useState<boolean>(false);

    const dispatch = (eventType: NoteUpdateType, index: number, content: any): void => {
      setState({
        index: index,
        eventType: eventType,
        record: { content: content },
      });
    };

    const updateSection = (notes0: NoteInfo[], doneNote0: string[], serverPageData: ServerPageData) => {
      const sectionPart: SectionPart = {};

      sectionPart["note"] = notes0;
      sectionPart["doneNote"] = doneNote0;

      const sectionDetails: SectionDetails = {
        section: sectionPart,
        sectionPath: serverPageData.sectionPath,
      };

      const data: NoteUpdateRequest = {
        section: sectionDetails,
      };

      executeRequest(MessageKind.UpdateNote, data);
    };

    const updateFormTemplate = (notes0: NoteInfo[], doneNote0: string[]) => {
      const formTemplatePart: FormTemplatePart = {
        _id: formTemplateId,
      };

      formTemplatePart["note"] = notes0;
      formTemplatePart["doneNote"] = doneNote0;

      const data: FormTemplateUpdateRequest = {
        formTemplate: formTemplatePart,
      };

      executeRequest(MessageKind.UpdateFormTemplate, data);
    };

    const updateSummarySection = (notes0: NoteInfo[], doneNote0: string[], coordinates: Coordinates | null) => {
      const summarySectionPart: SummarySection = {};

      summarySectionPart["note"] = notes0;
      summarySectionPart["doneNote"] = doneNote0;

      const summarySectionRequest: SummarySectionRequest = {
        payload: summarySectionPart,
        coordinates,
      };

      executeRequest(MessageKind.UpdateSummarySectionNote, summarySectionRequest);
    };

    const updateAcknowledgementSection = (note0: NoteInfo[], doneNote0: string[]) => {
      const acknowledgementSectionPart: AcknowledgementSection = {};

      acknowledgementSectionPart["note"] = note0;
      acknowledgementSectionPart["doneNote"] = doneNote0;

      const acknowledgementSectionRequest: AcknowledgementRequest = {
        payload: acknowledgementSectionPart,
      };

      executeRequest(MessageKind.UpdateAcknowledgementNote, acknowledgementSectionRequest);
    };

    const updateAddToListRepeater = (note0: NoteInfo[], doneNote0: string[], sectionPath: string) => {
      const atlRepeaterPart: AtlRepeater = {};

      atlRepeaterPart["note"] = note0;
      atlRepeaterPart["doneNote"] = doneNote0;

      const atlRepeaterRequest: AtlRepeaterNoteRequest = {
        payload: atlRepeaterPart,
        sectionPath: sectionPath,
      };

      executeRequest(MessageKind.UpdateAtlRepeaterNote, atlRepeaterRequest);
    };

    const updateAddToListDefaultPage = (note0: NoteInfo[], doneNote0: string[], sectionPath: string) => {
      const atlDefaultPagePart: DefaultPagePart = {};

      atlDefaultPagePart["note"] = note0;
      atlDefaultPagePart["doneNote"] = doneNote0;

      const atlDefaultPageRequest: AtlDefaultPageNoteRequest = {
        payload: atlDefaultPagePart,
        sectionPath: sectionPath,
      };

      executeRequest(MessageKind.UpdateAtlDefaultPageNote, atlDefaultPageRequest);
    };

    const updateAddToListCyaPage = (note0: NoteInfo[], doneNote0: string[], sectionPath: string) => {
      const atlCyaPagePart: CyaPagePart = {};

      atlCyaPagePart["note"] = note0;
      atlCyaPagePart["doneNote"] = doneNote0;

      const atlCyaPageRequest: AtlCyaPageNoteRequest = {
        payload: atlCyaPagePart,
        sectionPath: sectionPath,
      };

      executeRequest(MessageKind.UpdateAtlCyaPageNote, atlCyaPageRequest);
    };

    const executeRequest = (messageKind: MessageKind, data: any) => {
      const updateNoteRequest: ContentScriptRequest = {
        host: host,
        kind: messageKind,
        formTemplateId: formTemplateId,
        data: data,
      };

      onMessageHandler<APIResponse>(updateNoteRequest, (response) => {
        if (response.ok) {
        } else {
          if (serverError.current !== null) {
            if (response.error !== undefined) {
              serverError.current.innerText = "Form builder error: " + response.error;
            } else {
              serverError.current.innerText = "Form builder error: unknown server error";
            }
          }
        }
      });
    };

    useEffect(() => {
      handleSave();
    }, [state]);

    const handleSave = () => {
      if (state.notes.length === 0 && state.doneNotes.length === 0) {
        return;
      }
      const notesToSave = state.notes.filter((note, _) => !note.isDeleted).map(({ isDeleted, ...rest }) => rest);
      const doneNotes0 = state.doneNotes;
      switch (noteKind) {
        case NoteUpdateKind.Section:
          const serverPageData = noteKindData as ServerPageData;
          updateSection(notesToSave, doneNotes0, serverPageData);
          break;
        case NoteUpdateKind.FormTemplate:
          updateFormTemplate(notesToSave, doneNotes0);
          break;
        case NoteUpdateKind.SummarySection:
          const coordinates = noteKindData as Coordinates | null;
          updateSummarySection(notesToSave, doneNotes0, coordinates);
          break;
        case NoteUpdateKind.AcknowledgementSection:
          updateAcknowledgementSection(notesToSave, doneNotes0);
          break;
        case NoteUpdateKind.AddToListRepeater:
          {
            const sectionPath = noteKindData as string;
            updateAddToListRepeater(notesToSave, doneNotes0, sectionPath);
          }
          break;
        case NoteUpdateKind.AddToListDefaultPage:
          {
            const sectionPath = noteKindData as string;
            updateAddToListDefaultPage(notesToSave, doneNotes0, sectionPath);
          }
          break;
        case NoteUpdateKind.AddToListCyaPage:
          {
            const sectionPath = noteKindData as string;
            updateAddToListCyaPage(notesToSave, doneNotes0, sectionPath);
          }
          break;
      }
    };

    const handleMouseEnter = () => {
      setShowHint(true);
    };

    const handleMouseLeave = () => {
      setShowHint(false);
    };

    const handleMouseDoneEnter = () => {
      setShowDoneHint(true);
    };

    const handleMouseDoneLeave = () => {
      setShowDoneHint(false);
    };

    const handleAddNoteClick = () => {
      dispatch(NoteUpdateType.Add, 0, null);
    };

    const showDoneNotes = () => {
      const newWindow = window.open();
      if (newWindow) {
        const content = `
                <table class="table-class">
                    <thead>
                        <tr>
                            <th>Done notes</th>
                        </tr>
                    </thead>
                    <tbody>
                        ${state.doneNotes.map((note) => `<tr><td>${note.replace(/\n/g, "<br>")}</td></tr>`).join("")}
                    </tbody>
                </table>
            `;
        newWindow.document.body.innerHTML = `
                <html>
                  <head>
                    <style>
                      .body-class {
                          font-family: Arial, sans-serif;
                          margin: 20px;
                          padding: 20px;
                          background-color: #f9f9f9;
                          color: #333;
                      }

                      .header-class {
                          color: #007bff;
                          margin-bottom: 20px;
                      }

                      .table-class {
                          width: 100%;
                          border-collapse: collapse;
                          border: 1px solid #ddd;
                          font-size: 38px;
                          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
                      }

                      .table-class th, .table-class td {
                          padding: 10px;
                          border: 1px solid #ddd;
                          background-color: #fff;
                      }

                      .table-class th {
                          background-color: #f5f5f5;
                          text-align: left;
                      }
                    </style>
                  </head>
                  <body class="body-class">
                    ${content}
                  </body>
                </html>
            `;
      } else {
        console.error("Failed to open new window/tab");
      }
    };

    return (
      <>
        {
          <div>
            <style>
              {styles}
              {noteStyles}
            </style>

            <div>
              <div class="done-number" onClick={showDoneNotes}>
                <span onMouseEnter={handleMouseDoneEnter} onMouseLeave={handleMouseDoneLeave}>
                  {state.doneNotes.length}
                </span>
                <div class="tooltip" style={{ opacity: showDoneHint ? 1 : 0 }}>
                  Click to view done notes
                </div>
              </div>
              <div class="plus-symbol" onClick={handleAddNoteClick}>
                <span onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>
                  +
                </span>
                <div class="tooltip" style={{ opacity: showHint ? 1 : 0 }}>
                  Click to add a note
                </div>
              </div>
            </div>
            {state.notes.map((_, index) => (
              <Note noteIndex={index} note={state.notes[index]} serverError={serverError} dispatch={dispatch} />
            ))}
          </div>
        }

        <div class="server-error" ref={serverError}></div>
      </>
    );
  };

const createNoteInfo = (noteText: string) => {
  const defaultPosition = { x: 610, y: 15 };
  const defaultSize = { width: 300, height: 250 };
  const defaultColor = Colors.Yellow;
  return {
    noteText,
    position: defaultPosition,
    size: defaultSize,
    color: defaultColor,
    zIndex: 1000,
  };
};

// TODO: remove `NoteInfo[] | string` once there are no existing string notes
const convertNoteField = (note: NoteInfo[] | string) => {
  if (typeof note === "string") {
    return [createNoteInfo(note)];
  }
  return note;
};
