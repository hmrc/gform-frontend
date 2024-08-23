import { useState, useRef, useEffect } from "preact/hooks";
import { Ref } from "preact";

import type { NoteInfo } from "../../types";
import { NoteUpdateType } from "../../types";
import { ColorPicker } from "./color-picker";

interface NoteProps {
  note: NoteInfo;
  serverError: Ref<HTMLDivElement>;
  noteIndex: number;
  dispatch: (eventType: NoteUpdateType, index: number, content: any) => void;
}

export const Note = ({ note, noteIndex, dispatch }: NoteProps) => {
  const textAreaInputRef = useRef<HTMLTextAreaElement | null>(null);
  const [textAreaValue, setTexAreaValue] = useState(note.noteText);
  const [inVisibleState, setInvisibleState] = useState(false);
  const [positionState, setPositionState] = useState(note.position);
  const [sizeState, setSizeState] = useState(note.size);
  const [isDraggingState, setIsDraggingState] = useState(false);
  const [isResizingState, setIsResizingState] = useState(false);
  const [textAreaEditable, setTextAreaEditable] = useState(false);
  const [backgroundColor, setBackgroundColor] = useState<string>("");

  const textAreaOverlayRef = useRef<HTMLDivElement>(null);
  const isDragging = useRef(false);
  const isResizing = useRef(false);
  const inTextAreaRef = useRef<boolean>(false);
  const dragStart = useRef({ x: 0, y: 0 });

  const handleColorChange = (color: string) => {
    setBackgroundColor(color);
    dispatch(NoteUpdateType.Color, noteIndex, color);
  };

  // attempt to drag wnen mouse is down
  const handleMouseDown = (e: MouseEvent) => {
    // Prevent dragging when resizing
    if (isResizing.current) return;

    isDragging.current = true;
    setIsDraggingState(true);
    dragStart.current = { x: e.clientX - positionState.x, y: e.clientY - positionState.y };

    if (textAreaOverlayRef.current) {
      textAreaOverlayRef.current.classList.remove("grab");
      textAreaOverlayRef.current.classList.add("grabbing");
    }

    const handleMouseMove = (moveEvent: MouseEvent) => {
      if (isDragging.current) {
        const position = {
          x: moveEvent.clientX - dragStart.current.x,
          y: moveEvent.clientY - dragStart.current.y,
        };
        setPositionState(position);
      }
    };

    const handleMouseUp = () => {
      isDragging.current = false;
      setIsDraggingState(false);
      document.body.classList.remove("no-select");
      if (textAreaOverlayRef.current) {
        textAreaOverlayRef.current.classList.remove("grabbing");
        textAreaOverlayRef.current.classList.add("grab");
      }

      window.removeEventListener("mousemove", handleMouseMove);
      window.removeEventListener("mouseup", handleMouseUp);
    };

    window.addEventListener("mousemove", handleMouseMove);
    window.addEventListener("mouseup", handleMouseUp);
  };

  useEffect(() => {
    // save position and size when dragging or resizing is done
    if (!isDraggingState && !isResizingState) {
      dispatch(NoteUpdateType.Position, noteIndex, positionState);
      dispatch(NoteUpdateType.Size, noteIndex, sizeState);
    }
    // make z-index of the note highest when dragging or resizing
    if (isDraggingState || isResizingState) {
      dispatch(NoteUpdateType.ZIndex, noteIndex, null);
    }
  }, [positionState, sizeState, isDraggingState, isResizingState]);

  useEffect(() => {
    if (textAreaInputRef.current && textAreaEditable) {
      textAreaInputRef.current.focus();
    }
  }, [textAreaEditable]);

  const handleResize = (e: MouseEvent) => {
    isResizing.current = true;
    setIsResizingState(true);
    const startX = e.clientX;
    const startY = e.clientY;
    const startWidth = sizeState.width;
    const startHeight = sizeState.height;

    const doDrag = (dragEvent: MouseEvent) => {
      const newWidth = startWidth + dragEvent.clientX - startX;
      const newHeight = startHeight + dragEvent.clientY - startY;

      // Enforce minimum width and height
      const constrainedWidth = Math.max(newWidth, 200);
      const constrainedHeight = Math.max(newHeight, 150);

      setSizeState({
        width: constrainedWidth,
        height: constrainedHeight,
      });
    };

    const stopDrag = () => {
      isResizing.current = false;
      setIsResizingState(false);
      window.removeEventListener("mousemove", doDrag);
      window.removeEventListener("mouseup", stopDrag);
    };

    window.addEventListener("mousemove", doDrag);
    window.addEventListener("mouseup", stopDrag);
  };

  const onTextAreaEnter = () => {
    inTextAreaRef.current = true;
    setTextAreaEditable(true);
  };

  const onTextAreaOverlayClick = () => {
    if (textAreaOverlayRef.current) {
      textAreaOverlayRef.current.focus();
    }
  };

  const onTextAreaLeave = () => {
    inTextAreaRef.current = false;
    setTextAreaEditable(false);
  };

  const handleKeyDown = (e: KeyboardEvent) => {
    if (e.key === "Delete" || e.key === "Backspace") {
      setInvisibleState(true);
      dispatch(NoteUpdateType.Delete, noteIndex, null);
    }
  };

  const handleTextAreaKeyUp = () => {
    setTexAreaValue(textAreaInputRef.current?.value || "");
    dispatch(NoteUpdateType.NoteText, noteIndex, textAreaInputRef.current?.value);
  };

  return (
    <>
      {inVisibleState ? null : (
        <div class="draggable">
          <div
            class="post-it"
            style={{
              left: `${positionState.x}px`,
              top: `${positionState.y}px`,
              width: `${sizeState.width}px`,
              height: `${sizeState.height}px`,
              position: "absolute",
              backgroundColor: `${note.color ? note.color : ""}`,
              zIndex: note.zIndex,
            }}
            onMouseDown={handleMouseDown}
          >
            <div class="container" style={{ display: "inline-block" }}>
              <textarea
                id={"note" + noteIndex}
                className="form-control textarea"
                style={{
                  resize: "none",
                  backgroundColor: note.color,
                }}
                tabIndex={0}
                value={textAreaValue}
                ref={textAreaInputRef}
                onKeyUp={handleTextAreaKeyUp}
                onMouseEnter={onTextAreaEnter}
                onBlur={onTextAreaLeave}
              />
              {!textAreaEditable && (
                <div
                  id={"noteOverlay" + noteIndex}
                  style={{
                    position: "absolute",
                    top: 0,
                    left: 0,
                    width: "100%",
                    height: "100%",
                    zIndex: 1,
                  }}
                  ref={textAreaOverlayRef}
                  tabIndex={0}
                  className="custom-focus grab"
                  onKeyDown={handleKeyDown}
                  onClick={onTextAreaOverlayClick}
                  onDblClick={onTextAreaEnter}
                />
              )}
              <div class="note-controls bottom-container">
                <ColorPicker onChange={handleColorChange} />
              </div>
            </div>

            <div class="resizer" onMouseDown={handleResize}></div>
          </div>
        </div>
      )}
    </>
  );
};
