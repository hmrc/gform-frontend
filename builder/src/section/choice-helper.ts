import type { ChoiceClickable } from "../types";

export const mkChoiceClickable = (formComponentId: string): ChoiceClickable | undefined => {
  const eachChoiceEl: NodeListOf<HTMLInputElement> = document.querySelectorAll<HTMLInputElement>(
    `[name='${formComponentId}']`,
  );

  const firstElement = eachChoiceEl[0];

  if (
    firstElement !== null &&
    firstElement.parentElement !== null &&
    firstElement.parentElement.parentElement !== null
  ) {
    const isMultivalue = firstElement.type === "checkbox";

    // Previous element can be either legend, div or p
    // <legend> -> label
    // <div> -> help text
    // <p> -> error message
    let previousElement: Element | null = firstElement.parentElement.parentElement.previousElementSibling;

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
            eachChoiceEl,
            helpTextEl,
          };
        }
      }
    } else if (previousElement instanceof HTMLLegendElement) {
      const labelEl = previousElement;
      if (labelEl instanceof HTMLLegendElement) {
        const parentEl = labelEl?.parentElement?.parentElement; // <div> <- <fieldset> <- <legend> (labelEl)
        if (parentEl instanceof HTMLDivElement) {
          // This case can be reproduced with this section:
          //"sections": [
          //   {
          //     "title": "Example",
          //     "fields": [
          //       {
          //         "id": "example",
          //         "type": "choice",
          //         "label": "Example",
          //         "choices": ["Yes", "No"]
          //       }
          //     ]
          //   }
          // ]
          return {
            parentEl,
            labelEl,
            eachChoiceEl,
            helpTextEl: null,
          };
        }
      }
    }
  }
};

export const translateIndex = (smallerIndex: number, missingIndexes: number[]): number => {
  let largerIndex = 0;
  let missingCount = 0;

  while (missingCount <= smallerIndex) {
    if (!missingIndexes.includes(largerIndex)) {
      missingCount++;
    }
    if (missingCount <= smallerIndex) {
      largerIndex++;
    }
  }

  return largerIndex;
};
