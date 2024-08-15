import type { TextClickable } from "../types";

export const mkTextClickable = (
  inputFormComponentId: string,
  sectionTitleEqFieldLabel: boolean,
  form: HTMLFormElement,
  format: string | undefined,
): TextClickable | undefined => {
  const textInput: Element | null = document.getElementById(inputFormComponentId); // This is not working for lookup components.

  if (textInput instanceof HTMLInputElement || textInput instanceof HTMLTextAreaElement) {
    let searchRoot: Element = textInput;

    if (textInput.inputMode === "decimal") {
      // Sterling html is special
      const properSearchRoot = textInput.parentElement;
      if (properSearchRoot !== null) {
        searchRoot = properSearchRoot;
      }
    }

    if (sectionTitleEqFieldLabel) {
      // Previous element of searchRoot can be either h1, div or p
      // <h1> -> label (is inside h1)
      // <div> -> help text
      // <p> -> error message
      const labelEl: HTMLLabelElement | null = form.querySelector("label");

      let previousElement = format?.startsWith("lookup")
        ? searchRoot.previousElementSibling?.parentElement?.parentElement?.previousElementSibling
        : searchRoot.previousElementSibling;

      if (previousElement instanceof HTMLParagraphElement) {
        // Let's skip error message
        previousElement = previousElement.previousElementSibling;
      }

      if (previousElement instanceof HTMLDivElement) {
        const helpTextEl = previousElement;

        if (labelEl instanceof HTMLLabelElement) {
          const parentEl = labelEl.parentElement?.parentElement;
          if (parentEl instanceof HTMLDivElement) {
            const characterCountMessage = getCountInfo(inputFormComponentId);
            return {
              parentEl,
              labelEl,
              helpTextEl,
              characterCountMessage,
            };
          }
        }
      } else {
        if (labelEl instanceof HTMLLabelElement) {
          const parentEl = labelEl.parentElement?.parentElement;
          if (parentEl instanceof HTMLDivElement) {
            const characterCountMessage = getCountInfo(inputFormComponentId);
            return {
              parentEl,
              labelEl,
              helpTextEl: null,
              characterCountMessage,
            };
          }
        }
      }
    } else {
      // Previous element of searchRoot can be either label, div or p
      // <label> -> label
      // <div> -> help text
      // <p> -> error message
      let previousElement =
        format && format.toLowerCase().includes("sterling")
          ? searchRoot.previousElementSibling?.parentElement?.previousElementSibling
          : format?.startsWith("lookup")
            ? searchRoot.previousElementSibling?.parentElement?.parentElement?.previousElementSibling
            : searchRoot.previousElementSibling;

      if (previousElement instanceof HTMLParagraphElement) {
        // Let's skip error message
        previousElement = previousElement.previousElementSibling;
      }

      if (previousElement instanceof HTMLDivElement) {
        const labelEl = previousElement.previousElementSibling;

        const helpTextEl = previousElement;

        if (labelEl instanceof HTMLLabelElement) {
          const parentEl = labelEl.parentElement;
          if (parentEl instanceof HTMLDivElement) {
            const characterCountMessage = getCountInfo(inputFormComponentId);
            return {
              parentEl,
              labelEl,
              helpTextEl,
              characterCountMessage,
            };
          }
        }
      } else if (previousElement instanceof HTMLLabelElement) {
        const labelEl = previousElement;

        const parentEl = labelEl.parentElement;
        if (parentEl instanceof HTMLDivElement) {
          const characterCountMessage = getCountInfo(inputFormComponentId);
          return {
            parentEl,
            labelEl,
            helpTextEl: null,
            characterCountMessage,
          };
        }
      }
    }
  }
};

const getCountInfo = (inputFormComponentId: string): HTMLDivElement | null => {
  const characterCountInfo = document.getElementById(`${inputFormComponentId}-info`);
  return characterCountInfo instanceof HTMLDivElement ? characterCountInfo : null;
};

export const extractFormatValues = (format: string) => {
  let min = "";
  let max = "";
  // Extracts minimum and maximum values from the format string in the formats: string, text(1,2), shortText(10, 1000)
  const regex = /\(\s*(\d+)\s*,\s*(\d+)\s*\)/;
  const match = regex.exec(format);

  if (match) {
    min = match[1];
    max = match[2];
  }

  return { min, max };
};

export const extractFormatPrefix = (format: string): string => {
  const index = format?.indexOf("(");
  return index === -1 ? format : format.substring(0, index);
};

export const shouldDisplayFormatValues = (format: string): boolean =>
  ["text", "shortText", "referenceNumber", "number", "positiveNumber"].includes(format);

export const getFormatValueLabel = (format: string) => {
  switch (format) {
    case "text":
    case "shortText":
    case "referenceNumber":
      return { left: "Minimum characters", right: "Maximum characters" };
    case "number":
    case "positiveNumber":
      return { left: "Maximum whole digits", right: " Maximum decimal digits" };
  }
};
