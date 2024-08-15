import type { AtlRepeater, FetchSectionRequest, ContentScriptRequest, FormTemplateId, ServerPageData } from "./types";
import { MessageKind, SectionNumber } from "./types";
import { atlDefaultPageBootstrap } from "./add-to-list/default-page";
import { atlCyaPageBootstrap } from "./add-to-list/cya-page";
import { atlRepeaterBootstrap } from "./add-to-list/repeater";
import { sectionBootstrap } from "./section/section-bootstrap";
import { replaceWithEnglishValue, removeWelshValues } from "./pure-functions";
import { onMessageHandler } from "./background/index";

const url: string = window.document.documentURI;
const urlMatch: RegExpMatchArray | null = url.match(/(.*)\/submissions\/form\/([^/]*)\/(.*)\?(.*)/);
const host = urlMatch![1];
const formTemplateId: FormTemplateId = urlMatch![2];

const constructPageIdFromString = (s: string): SectionNumber => {
  const pattern = /^\d+,\d+,\d+$/;
  const parsedNumber = parseInt(s);

  if (pattern.test(s)) {
    return SectionNumber.TaskListSectionNumber(s);
  } else if (!isNaN(parsedNumber)) {
    return SectionNumber.RegularSectionNumber(parsedNumber);
  } else {
    throw new Error(`Cannot convert ${s} into SectionNumber`);
  }
};

const queryParams = urlMatch![4];
const urlParams = new URLSearchParams("?" + queryParams);
const nParam: string | null = urlParams.get("n");
const maybeAccessCode: string | null = urlParams.get("a");

if (nParam === null) {
  const msg = `"Could not find section number in url ${url}"`;
  console.error(msg);
  throw new Error(msg);
}

const sectionNumber: SectionNumber = constructPageIdFromString(nParam);

const fetchSectionRequest: FetchSectionRequest = {
  sectionNumber: sectionNumber,
  maybeAccessCode: maybeAccessCode === null ? undefined : maybeAccessCode,
};

const fetchSection: ContentScriptRequest = {
  host: host,
  kind: MessageKind.FetchSection,
  formTemplateId: formTemplateId,
  data: fetchSectionRequest,
};

const form: HTMLElement | null = document.getElementById("gf-form");

if (form instanceof HTMLFormElement) {
  const formParentEl: HTMLElement | null = form.parentElement;
  if (formParentEl instanceof HTMLDivElement) {
    onMessageHandler(fetchSection, (initialServerPageData0) => {
      const initialServerPageData: ServerPageData = replaceWithEnglishValue(
        structuredClone(initialServerPageData0),
      ) as ServerPageData;
      if (initialServerPageData.atlRepeater) {
        const sectionPath = initialServerPageData.sectionPath;
        const repeater: AtlRepeater = initialServerPageData.section as AtlRepeater;
        const atlIterationIndex = initialServerPageData.atlIterationIndex;
        const addToListId = repeater.addAnotherQuestion?.id;
        if (addToListId !== undefined && atlIterationIndex !== undefined) {
          atlRepeaterBootstrap(
            repeater,
            atlIterationIndex,
            addToListId,
            formParentEl,
            host,
            formTemplateId,
            sectionNumber,
            sectionPath,
            maybeAccessCode,
          );
        }
      } else if (initialServerPageData.atlDefaultPage) {
        const sectionPath = initialServerPageData.sectionPath;
        const repeater: AtlRepeater = initialServerPageData.section as AtlRepeater;
        atlDefaultPageBootstrap(
          repeater,
          formParentEl,
          host,
          formTemplateId,
          sectionNumber,
          sectionPath,
          maybeAccessCode,
        );
      } else if (initialServerPageData.atlCyaPage) {
        const sectionPath = initialServerPageData.sectionPath;
        const repeater: AtlRepeater = initialServerPageData.section as AtlRepeater;
        atlCyaPageBootstrap(repeater, formParentEl, host, formTemplateId, sectionNumber, sectionPath, maybeAccessCode);
      } else {
        const initialServerPageDataNoWelsch: ServerPageData = removeWelshValues(
          structuredClone(initialServerPageData0),
        ) as ServerPageData;
        sectionBootstrap(
          initialServerPageDataNoWelsch,
          formParentEl,
          host,
          formTemplateId,
          sectionNumber,
          form,
          maybeAccessCode,
        );
      }
    });
  }
}
