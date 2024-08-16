import { mkLookup } from "./pure-functions";
import type { AddressClickable, Section, FormComponent } from "../types";

export const mkAddressClickable = (inputFormComponentId: string): AddressClickable | undefined => {
  const parentEl: HTMLElement | null = document.getElementById(`${inputFormComponentId}-fieldset`);

  if (parentEl instanceof HTMLFieldSetElement) {
    const labelEl: HTMLLegendElement | null = parentEl.querySelector("legend");
    const helpTextEl: HTMLElement | null = document.getElementById(`${inputFormComponentId}-hint`);

    if (labelEl instanceof HTMLLegendElement && (helpTextEl === null || helpTextEl instanceof HTMLDivElement)) {
      return {
        parentEl,
        labelEl,
        helpTextEl,
      };
    }
  }
};
