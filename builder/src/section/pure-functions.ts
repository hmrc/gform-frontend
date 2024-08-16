import type { FormComponent, FormComponentId, ServerPageData } from "../types";
import { FieldInteractionType } from "../types";

export const mkLookup = (acc: any, cur: FormComponent, index: number) => {
  acc[cur.id] = index;
  return acc;
};

export const registerSectionChannelHandler = (
  channel: MessageChannel,
  onSectionUpdateHandler: (msg: any) => void,
): void => {
  if (channel.port2.onmessage === null) {
    channel.port2.onmessage = onSectionUpdateHandler;
  }
};

export const fullFormComponentId = (formComponentId: FormComponentId, atlIterationIndex?: number): FormComponentId => {
  const preffix = atlIterationIndex !== undefined ? atlIterationIndex + "_" : "";

  return `${preffix}${formComponentId}`;
};

export const isOptional = (formComponent: FormComponent): boolean =>
  formComponent.mandatory === "false" || formComponent.mandatory === "no";

export const findFirstField = (serverPageData: ServerPageData): FormComponent | null => {
  const applicableFields = findApplicableFields(serverPageData.section.fields);
  const firstField = serverPageData.section.fields[0];

  return applicableFields.length === 1 && applicableFields[0] === firstField ? firstField : null;
};

export const findApplicableFields = (fields: FormComponent[]): FormComponent[] => {
  return fields.filter((field) => !["info", "table", "miniSummaryList", "postcodeLookup"].includes(field.type));
};

export const findInfoInteractionType = (fields: FormComponent[]): FieldInteractionType => {
  return fields[0].type === "info" ? FieldInteractionType.TitleLabelNoSync : FieldInteractionType.ComponentOnly;
};

export const replaceUndefinedByEmptyString = (obj: any): any => {
  const newObject: any = {};
  Object.keys(obj).forEach((key) => {
    newObject[key] = obj[key] === undefined ? "" : obj[key];
  });
  return structuredClone(newObject);
};

export const isTitleEqLabel = (serverPageData: ServerPageData): boolean => {
  const firstFieldLabel = findFirstField(serverPageData)?.label;
  return (
    firstFieldLabel !== undefined &&
    serverPageData.section.title !== undefined &&
    firstFieldLabel === serverPageData.section.title
  );
};

export const replaceWithEnglishValue = (obj: any): any => {
  if (!obj || typeof obj !== "object") return obj;

  if ("en" in obj) {
    return Object.keys(obj).some((attr) => attr !== "en" && attr !== "cy") ? obj : obj.en;
  }
  const result: any = Array.isArray(obj) ? [] : {};
  for (let key in obj) {
    if (key !== "choices") {
      result[key] = replaceWithEnglishValue(obj[key]);
    } else {
      result[key] = obj[key];
    }
  }
  return result;
};

export const removeWelshValues = (obj: any): any => {
  if (!obj || typeof obj !== "object") return obj;

  if ("cy" in obj) {
    delete obj.cy;
  }
  const result: any = Array.isArray(obj) ? [] : {};
  for (let key in obj) {
    result[key] = removeWelshValues(obj[key]);
  }
  return result;
};

export const attachShadowDom = (element: Element) => {
  let shadowRoot: ShadowRoot | null = element.shadowRoot;
  if (shadowRoot === null) {
    shadowRoot = element.attachShadow({ mode: "open" });
    const slot: HTMLSlotElement = document.createElement("slot");
    const content: HTMLDivElement = document.createElement("div");
    shadowRoot.appendChild(slot);
    shadowRoot.appendChild(content);
    return content;
  }
};

export const removePreviousSiblings = (form: HTMLFormElement) => {
  const prev: Element | null = form.previousElementSibling;
  if (prev !== null) {
    prev.remove();
    removePreviousSiblings(form);
  }
};
