import type { AtlRepeater, FetchSectionRequest, ContentScriptRequest, FormTemplateId, ServerPageData } from "../types";
import { MessageKind, SectionNumber } from "../types";
import { atlDefaultPageBootstrap } from "../add-to-list/default-page";
import { atlCyaPageBootstrap } from "../add-to-list/cya-page";
import { atlRepeaterBootstrap } from "../add-to-list/repeater";
import { sectionBootstrap } from "./section-bootstrap";
import { replaceWithEnglishValue, removeWelshValues } from "../pure-functions";
import { onMessageHandler } from "../background/index";

export const activateSectionBuilder = (urlMatch: RegExpMatchArray, url: string) => {
  const host = urlMatch![1];
  const formTemplateId: FormTemplateId = urlMatch![2];

  const normalPagePattern = /^n(\d+)$/;
  const atlPagePattern = /^ap(\d+).(\d+).(\d+)$/;
  const atlDefaultPagePattern = /^ad(\d+)$/;
  const atlCyaPagePattern = /^ac(\d+).(\d+)$/;
  const atlRepeaterPagePattern = /^ar(\d+).(\d+)$/;

  const toSectionNumber = (s: string): SectionNumber => {
    let match;
    if ((match = normalPagePattern.exec(s)) !== null) {
      return SectionNumber.NormalPage(parseInt(match[1]));
    } else if ((match = atlPagePattern.exec(s)) !== null) {
      return SectionNumber.AddToListPage(parseInt(match[1]), parseInt(match[2]), parseInt(match[3]));
    } else if ((match = atlDefaultPagePattern.exec(s)) !== null) {
      return SectionNumber.AddToListDefaultPage(parseInt(match[1]));
    } else if ((match = atlCyaPagePattern.exec(s)) !== null) {
      return SectionNumber.AddToListCyaPage(parseInt(match[1]), parseInt(match[2]));
    } else if ((match = atlRepeaterPagePattern.exec(s)) !== null) {
      return SectionNumber.AddToListRepeaterPage(parseInt(match[1]), parseInt(match[2]));
    } else {
      throw new Error(`Cannot convert ${s} into SectionNumber`);
    }
  };

  const constructPageIdFromString = (s: string): SectionNumber => {
    const taskListPattern = /^(\d+),(\d+),([a-z.0-9]+)$/;

    let match;
    if ((match = taskListPattern.exec(s)) !== null) {
      const sn = toSectionNumber(match[3]);
      return SectionNumber.TaskListSectionNumber(parseInt(match[1]), parseInt(match[2]), sn);
    } else {
      return toSectionNumber(s);
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
          atlCyaPageBootstrap(
            repeater,
            formParentEl,
            host,
            formTemplateId,
            sectionNumber,
            sectionPath,
            maybeAccessCode,
          );
        } else {
          const initialServerPageDataNoWelsh: ServerPageData = removeWelshValues(
            structuredClone(initialServerPageData0),
          ) as ServerPageData;
          sectionBootstrap(
            initialServerPageDataNoWelsh,
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
};
