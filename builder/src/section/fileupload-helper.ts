import type { FileUploadClickable } from "../types";

export const mkFileUploadClickable = (
  inputFormComponentId: string,
  sectionTitleEqFieldLabel: boolean,
  form: HTMLFormElement,
): FileUploadClickable | undefined => {
  const fileUploadInput: HTMLElement | null = document.getElementById(`${inputFormComponentId}`);

  if (fileUploadInput instanceof HTMLInputElement) {
    if (sectionTitleEqFieldLabel) {
      const labelEl: HTMLLabelElement | null = form.querySelector("label");
      let previousElement = fileUploadInput.previousElementSibling;

      if (previousElement instanceof HTMLParagraphElement) {
        // Let's skip error message
        previousElement = previousElement.previousElementSibling;
      }

      const helpTextEl = previousElement instanceof HTMLDivElement ? previousElement : null;

      if (labelEl instanceof HTMLLabelElement) {
        const parentEl = labelEl.parentElement?.parentElement;
        if (parentEl instanceof HTMLDivElement) {
          return {
            parentEl,
            labelEl,
            helpTextEl,
            characterCountMessage: null,
          };
        }
      }
    } else {
      if (fileUploadInput instanceof HTMLInputElement) {
        // Previous element can be either label, div or p
        // <label> -> label
        // <div> -> help text
        // <p> -> error message

        let previousElement = fileUploadInput.previousElementSibling;

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
              return {
                parentEl,
                labelEl,
                helpTextEl,
                characterCountMessage: null,
              };
            }
          }
        } else if (previousElement instanceof HTMLLabelElement) {
          const labelEl = previousElement;

          const parentEl = labelEl.parentElement;
          if (parentEl instanceof HTMLDivElement) {
            return {
              parentEl,
              labelEl,
              helpTextEl: null,
              characterCountMessage: null,
            };
          }
        }
      }
    }
  }
};
