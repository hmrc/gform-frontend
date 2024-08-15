import { mkLookup } from "./pure-functions";
import type { DateClickable, Section, FormComponent } from "../types";

export const mkDateClickable = (formComponentId: string): DateClickable | undefined => {
  const dateComponent = document.getElementById(formComponentId);

  if (dateComponent !== null) {
    // Previous element can be either legend, div or p
    // <legend> -> label
    // <div> -> help text
    // <p> -> error message
    let previousElement = dateComponent.previousElementSibling;

    if (previousElement instanceof HTMLParagraphElement) {
      // Let's skip error message
      previousElement = previousElement.previousElementSibling;
    }

    if (previousElement instanceof HTMLDivElement) {
      const labelEl = previousElement.previousElementSibling;

      const helpTextEl = previousElement;

      if (labelEl instanceof HTMLLegendElement) {
        const parentEl = labelEl?.parentElement?.parentElement;
        if (parentEl instanceof HTMLDivElement) {
          return {
            parentEl,
            labelEl,
            helpTextEl,
          };
        }
      }
    } else if (previousElement instanceof HTMLLegendElement) {
      const labelEl = previousElement;
      if (labelEl instanceof HTMLLegendElement) {
        const parentEl = labelEl?.parentElement?.parentElement;
        if (parentEl instanceof HTMLDivElement) {
          return {
            parentEl,
            labelEl,
            helpTextEl: null,
          };
        }
      }
    }
  }
};
