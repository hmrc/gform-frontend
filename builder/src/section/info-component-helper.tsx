import { render } from "preact";
import type {
  InfoLookupHelper,
  FormComponent,
  FormComponentId,
  FormTemplateId,
  InfoRenderParam,
  InfoSelectorAndIndex,
} from "../types";
import { MessageKind, SectionNumber } from "../types";
import { mkLookup, attachShadowDom } from "../pure-functions";
import { InfoComponentControllerFactory } from "./info-component-controller";

const initiateInfoComponent = (
  element: HTMLDivElement | HTMLDetailsElement,
  formComponent: FormComponent,
  params: InfoRenderParam,
  maybeAccessCode: string | null,
) => {
  const attachmentDiv = document.createElement("div");
  attachmentDiv.setAttribute("id", `${formComponent.id}-shadow-root`);
  element.insertAdjacentElement("afterend", attachmentDiv);

  const content: HTMLDivElement | undefined = attachShadowDom(attachmentDiv);
  let abortController = new AbortController();
  if (content !== undefined) {
    const InfoComponentController = InfoComponentControllerFactory(
      formComponent,
      params.host,
      params.formTemplateId,
      params.sectionNumber,
      params.atlIterationIndex,
      element,
      abortController,
      maybeAccessCode,
      params.sectionNumberChange,
    );
    render(<InfoComponentController kind={params.kind} requestData={params.requestData} />, content);
  }
};

export const infoComponentLookup = (fields: FormComponent[]): InfoLookupHelper => {
  const infoComponents: FormComponent[] = fields.filter((fc) => fc.type === "info");
  const noformatInfo: FormComponent[] = infoComponents.filter((fc) => fc.infoType === "noformat");
  const standardInfo: FormComponent[] = infoComponents.filter(
    (fc) => fc.infoType === "standard" || fc.infoType === undefined,
  );
  const longInfo: FormComponent[] = infoComponents.filter((fc) => fc.infoType === "long");
  const bannerInfo: FormComponent[] = infoComponents.filter((fc) => fc.infoType === "banner");
  const importantInfo: FormComponent[] = infoComponents.filter((fc) => fc.infoType === "important");

  const noformatIndexLookup = noformatInfo.reduce(mkLookup, {});
  const standardIndexLookup = standardInfo.reduce(mkLookup, {});
  const longIndexLookup = longInfo.reduce(mkLookup, {});
  const bannerIndexLookup = bannerInfo.reduce(mkLookup, {});
  const importantIndexLookup = importantInfo.reduce(mkLookup, {});

  return {
    noformatIndexLookup,
    standardIndexLookup,
    longIndexLookup,
    bannerIndexLookup,
    importantIndexLookup,
  };
};

export const findInfoElement = (
  formComponentId: string,
  infoType: string | undefined,
  infoLookups: InfoLookupHelper,
): InfoSelectorAndIndex | undefined => {
  switch (infoType) {
    case "noformat":
      {
        return {
          selector: ".govuk-\\!-font-size-19",
          index: infoLookups.noformatIndexLookup[formComponentId],
        };
      }
      break;
    case undefined:
    case "standard":
      {
        return {
          selector: ".govuk-inset-text",
          index: infoLookups.standardIndexLookup[formComponentId],
        };
      }
      break;
    case "long":
      {
        return {
          selector: ".govuk-details",
          index: infoLookups.longIndexLookup[formComponentId],
        };
      }
      break;
    case "banner":
      {
        return {
          selector: ".govuk-panel",
          index: infoLookups.bannerIndexLookup[formComponentId],
        };
      }
      break;
    case "important":
      {
        return {
          selector: ".govuk-warning-text",
          index: infoLookups.importantIndexLookup[formComponentId],
        };
      }
      break;
  }
};

// Deprecated, dont use!!!
export const initiateInfoComponents = (
  fields: FormComponent[],
  params: InfoRenderParam,
  maybeAccessCode: string | null,
) => {
  const infoComponents: FormComponent[] = fields.filter((fc) => fc.type === "info");
  const noformatInfo: FormComponent[] = infoComponents.filter((fc) => fc.infoType === "noformat");
  const standardInfo: FormComponent[] = infoComponents.filter(
    (fc) => fc.infoType === "standard" || fc.infoType === undefined,
  );
  const longInfo: FormComponent[] = infoComponents.filter((fc) => fc.infoType === "long");
  const bannerInfo: FormComponent[] = infoComponents.filter((fc) => fc.infoType === "banner");
  const importantInfo: FormComponent[] = infoComponents.filter((fc) => fc.infoType === "important");

  const noformatIndexLookup = noformatInfo.reduce(mkLookup, {});
  const standardIndexLookup = standardInfo.reduce(mkLookup, {});
  const longIndexLookup = longInfo.reduce(mkLookup, {});

  const bannerIndexLookup = bannerInfo.reduce(mkLookup, {});
  const importantIndexLookup = importantInfo.reduce(mkLookup, {});

  fields.forEach((formComponent) => {
    const id: FormComponentId = formComponent.id;
    switch (formComponent.type) {
      case "info":
        {
          switch (formComponent.infoType) {
            case "noformat":
              {
                const infoElements: NodeListOf<Element> = document.querySelectorAll(".govuk-\\!-font-size-19");

                const componentIndex = noformatIndexLookup[formComponent.id];

                const infoComponent = infoElements.item(componentIndex);

                if (infoComponent instanceof HTMLDivElement) {
                  initiateInfoComponent(infoComponent, formComponent, params, maybeAccessCode);
                }
              }
              break;
            case undefined:
            case "standard":
              {
                const infoElements: NodeListOf<Element> = document.querySelectorAll(".govuk-inset-text");

                const componentIndex = standardIndexLookup[formComponent.id];

                const infoComponent = infoElements.item(componentIndex);

                if (infoComponent instanceof HTMLDivElement) {
                  initiateInfoComponent(infoComponent, formComponent, params, maybeAccessCode);
                }
              }
              break;
            case "long":
              {
                const infoElements: NodeListOf<Element> = document.querySelectorAll(".govuk-details");

                const componentIndex = longIndexLookup[formComponent.id];

                const infoComponent = infoElements.item(componentIndex);

                if (infoComponent instanceof HTMLDetailsElement) {
                  initiateInfoComponent(infoComponent, formComponent, params, maybeAccessCode);
                }
              }
              break;
            case "banner":
              {
                const infoElements: NodeListOf<Element> = document.querySelectorAll(".govuk-panel");

                const componentIndex = bannerIndexLookup[formComponent.id];

                const infoComponent = infoElements.item(componentIndex);

                if (infoComponent instanceof HTMLDivElement) {
                  initiateInfoComponent(infoComponent, formComponent, params, maybeAccessCode);
                }
              }
              break;
            case "important":
              {
                const infoElements: NodeListOf<Element> = document.querySelectorAll(".govuk-warning-text");
                const componentIndex = importantIndexLookup[formComponent.id];

                const infoComponent = infoElements.item(componentIndex);

                if (infoComponent instanceof HTMLDivElement) {
                  initiateInfoComponent(infoComponent, formComponent, params, maybeAccessCode);
                }
              }
              break;
          }
        }
        break;
    }
  });
};
