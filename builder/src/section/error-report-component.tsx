import { FunctionComponent } from "preact";

interface ErrorReportLinkProps {
  host: string;
  formTemplateId: string;
  formComponentId: string;
}

export const ErrorReportLink: FunctionComponent<ErrorReportLinkProps> = ({ host, formTemplateId, formComponentId }) => (
  <a
    className="btn error-report btn-link"
    href={`${host}/submissions/test-only/errors/${formTemplateId}?jsonReport=false&baseComponentId=${formComponentId}&isUsageReport=false`}
  >
    Error messages report
  </a>
);
