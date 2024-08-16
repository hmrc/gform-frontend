import { PropsWithChildren } from "preact/compat";
import type { SmartStringEvent, SmartString } from "../types";
import { SmartStringFieldType } from "../types";

export const updateSmartString = (
  smartStringEvent: SmartStringEvent,
  smartString: SmartString | undefined,
): SmartString => {
  const input = smartStringEvent.event.target as HTMLInputElement;
  const content = input.value;
  if (smartString === undefined || typeof smartString === "string") {
    return content;
  } else {
    if (smartString instanceof Array) {
      const index = smartStringEvent.index;
      const fieldType = smartStringEvent.fieldType;
      const variant = smartString[smartStringEvent.index];
      switch (smartStringEvent.fieldType) {
        case SmartStringFieldType.Text:
          variant.en = content;
          break;
        case SmartStringFieldType.IncludeIf:
          variant.includeIf = content;
          break;
      }
      return smartString;
    } else {
      return { en: content };
    }
  }
};

const textEventNoIndex = (e: Event): SmartStringEvent => ({
  index: -1,
  fieldType: SmartStringFieldType.Text,
  event: e, // Only event is needed, see updateSmartString function above
});

const textEvent = (index: number, e: Event): SmartStringEvent => ({
  index: index,
  fieldType: SmartStringFieldType.Text,
  event: e,
});

const includeIfEvent = (index: number, e: Event): SmartStringEvent => ({
  index: index,
  fieldType: SmartStringFieldType.IncludeIf,
  event: e,
});

export interface SmartStringProps {
  id: string;
  value: SmartString | undefined;
  onKeyUp: (e: SmartStringEvent) => void;
  disabled?: boolean;
  multiline?: boolean;
}

const enValue = (value: any): string => {
  if (value === undefined || typeof value === "string") {
    return value;
  } else {
    return value.en;
  }
};

export const SmartStringInput = ({
  id,
  value,
  onKeyUp,
  disabled = false,
  multiline = false,
  children,
}: PropsWithChildren<SmartStringProps>) => {
  if (multiline) {
    if (value instanceof Array) {
      return (
        <>
          <fieldset class="smarttext-panel">
            <legend class="smarttext-panel">{children}</legend>
            {value.map((item, index) => (
              <>
                <label for={id + "-" + index}>
                  {index + 1}. Variant {value.length === index + 1 ? "(default)" : ""}
                </label>
                <textarea
                  id={id + "-" + index}
                  class="form-control"
                  disabled={disabled}
                  rows={4}
                  onKeyUp={(e) => onKeyUp(textEvent(index, e))}
                >
                  {enValue(item)}
                </textarea>
                {item.includeIf !== undefined && (
                  <>
                    <label for={id + "-includeIf-" + index}>{index + 1}. IncludeIf</label>
                    <input
                      id={id + "-includeIf-" + index}
                      class="form-control"
                      value={item.includeIf}
                      disabled={disabled}
                      onKeyUp={(e) => onKeyUp(includeIfEvent(index, e))}
                    />
                  </>
                )}
              </>
            ))}
          </fieldset>
        </>
      );
    } else {
      return (
        <>
          <label for={id}>{children}</label>
          <textarea
            id={id}
            class="form-control"
            disabled={disabled}
            rows={4}
            onKeyUp={(e) => onKeyUp(textEventNoIndex(e))}
          >
            {enValue(value)}
          </textarea>
        </>
      );
    }
  } else {
    if (value instanceof Array) {
      return (
        <>
          <fieldset class="smarttext-panel">
            <legend class="smarttext-panel">{children}</legend>
            {value.map((item, index) => (
              <>
                <label for={id + "-" + index}>
                  {index + 1}. Variant {value.length === index + 1 ? "(default)" : ""}
                </label>
                <input
                  id={id + "-" + index}
                  class="form-control"
                  value={enValue(item)}
                  disabled={disabled}
                  onKeyUp={(e) => onKeyUp(textEvent(index, e))}
                />
                {item.includeIf !== undefined && (
                  <>
                    <label for={id + "-includeIf-" + index}>{index + 1}. IncludeIf</label>
                    <input
                      id={id + "-includeIf-" + index}
                      class="form-control"
                      value={item.includeIf}
                      disabled={disabled}
                      onKeyUp={(e) => onKeyUp(includeIfEvent(index, e))}
                    />
                  </>
                )}
              </>
            ))}
          </fieldset>
        </>
      );
    } else {
      return (
        <>
          <label for={id}>{children}</label>
          <input
            id={id}
            class="form-control"
            value={enValue(value)}
            disabled={disabled}
            onKeyUp={(e) => onKeyUp(textEventNoIndex(e))}
          />
        </>
      );
    }
  }
};
