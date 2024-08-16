import type { NoteState } from "../types";
import { NoteUpdateType, NoteUpdateEvent, NoteInfoState } from "../types";

import { NoteUpdateKind } from "../types";

import { Colors } from "./note-component/color-picker";

export const noteReducer = (state: NoteState, action: NoteUpdateEvent): NoteState => {
  const index = action.index;
  const record = action.record;

  switch (action.eventType) {
    case NoteUpdateType.NoteText: {
      const updatedNotes = [...state.notes];
      updatedNotes[index] = { ...updatedNotes[index], noteText: record.content };
      return { ...state, notes: updatedNotes, undo: { ...state } };
    }

    case NoteUpdateType.Position: {
      const updatedNotes = [...state.notes];
      updatedNotes[index] = { ...updatedNotes[index], position: record.content };

      return { ...state, notes: updatedNotes, undo: { ...state } };
    }

    case NoteUpdateType.Size: {
      const updatedNotes = [...state.notes];
      updatedNotes[index] = { ...updatedNotes[index], size: record.content };

      return { ...state, notes: updatedNotes, undo: { ...state } };
    }
    case NoteUpdateType.Color: {
      const updatedNotes = [...state.notes];
      updatedNotes[index] = { ...updatedNotes[index], color: record.content };

      return { ...state, notes: updatedNotes, undo: { ...state } };
    }
    case NoteUpdateType.ZIndex: {
      const currentZIndex = state.notes[index].zIndex;
      const nextZIndex =
        Math.max(...state.notes.map((note) => (note.zIndex && note.zIndex > 100 ? note.zIndex : 1000000))) + 1;
      if (currentZIndex + 1 >= nextZIndex) {
        // do not increase zIndex if max already
        return state;
      }
      const updatedNotes = [...state.notes];
      updatedNotes[index] = { ...updatedNotes[index], zIndex: nextZIndex };

      return { ...state, notes: updatedNotes, undo: { ...state } };
    }
    case NoteUpdateType.Delete: {
      const updatedNotes = [...state.notes];
      updatedNotes[index] = { ...updatedNotes[index], isDeleted: true };
      return {
        ...state,
        notes: updatedNotes,
        undo: { ...state },
        doneNotes: [...state.doneNotes, updatedNotes[index].noteText],
      };
    }

    case NoteUpdateType.Add: {
      const nextZIndex =
        Math.max(...state.notes.map((note) => (note.zIndex && note.zIndex > 100 ? note.zIndex : 1000000))) + 1;
      const newNote: NoteInfoState = {
        noteText: "",
        position: { x: 610, y: 15 },
        size: { width: 300, height: 250 },
        color: Colors.Yellow,
        zIndex: nextZIndex,
        isDeleted: false,
      };
      const updatedNotes = [...state.notes, newNote];
      return { ...state, notes: updatedNotes, undo: { ...state } };
    }
    default:
      return state;
  }
};

export interface NoteProps {
  noteKind: NoteUpdateKind;
  noteKindData: any;
}
