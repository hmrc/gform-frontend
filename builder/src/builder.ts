import { activateSectionBuilder } from "./section/index";
import { activateSummaryBuilder } from "./summary/index";
import { activateTaskLandingPageBuilder } from "./task-landing-page/index";
import { activateAcknowledgementBuilder } from "./acknowledgement/index";

const url: string = window.document.documentURI;
const urlMatchContent: RegExpMatchArray | null = url.match(/(.*)\/submissions\/form\/([^/]*)\/(.*)\?(.*)/);
const urlMatchSummary: RegExpMatchArray | null = url.match(/(.*)\/submissions\/summary\/([^/]*)\/(.*)\?(.*)/);
const urlMatchTaskLandingPage: RegExpMatchArray | null = url.match(
  /(.*)\/submissions\/tasklist\/tasks\/([^/]+)\/([^/]+)(?:\/(.*))?/,
);

const urlMatchAcknowledgement: RegExpMatchArray | null = url.match(/(.*)\/submissions\/acknowledgement\/([^/]*)?(.*)/);
if (urlMatchContent !== null) {
  activateSectionBuilder(urlMatchContent, url);
} else if (urlMatchSummary !== null) {
  activateSummaryBuilder(urlMatchSummary, url);
} else if (urlMatchTaskLandingPage !== null) {
  activateTaskLandingPageBuilder(urlMatchTaskLandingPage, url);
} else if (urlMatchAcknowledgement !== null) {
  activateAcknowledgementBuilder(urlMatchAcknowledgement, url);
}
