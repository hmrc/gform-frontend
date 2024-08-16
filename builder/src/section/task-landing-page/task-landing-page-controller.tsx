import * as styles from "bundle-text:../content.css";
import { useEffect, useRef, useState } from "preact/hooks";
import { ContentScriptRequest, FormTemplatePart, FormTemplateUpdateRequest, APIResponse } from "../../types";
import { MessageKind } from "../../types";
import { onMessageHandler } from "../../background/index";

export const TaskLandingPageControllerFactory =
  (
    formTemplateId: string,
    host: string,
    mainContent: HTMLElement,
    headingEl: HTMLHeadingElement,
    displayWidth: string | undefined,
  ) =>
  () => {
    const refreshLandingPageLayout = (displayWidth: string): void => {
      const layoutEl = mainContent
        .querySelector(".govuk-grid-row .govuk-grid-column-full .govuk-grid-row")
        ?.children.item(0);
      const className = layoutEl?.className;
      if (className) {
        switch (displayWidth) {
          case "":
          case "m":
            layoutEl?.classList.replace(className, "govuk-grid-column-two-thirds");
            break;
          case "l":
            layoutEl?.classList.replace(className, "govuk-grid-column-three-quarters");
            break;
          case "xl":
            layoutEl?.classList.replace(className, "govuk-grid-column-full");
            break;
        }
      }
    };

    const content = useRef<HTMLDivElement>(null);
    const serverError = useRef<HTMLDivElement>(null);

    const displayWidthInput = useRef<HTMLSelectElement>(null);
    const displayWidthContainer = useRef<HTMLDivElement>(null);

    const [displayWidthValue, setDisplayWidthValue] = useState(displayWidth);
    const [buttonLabel, setButtonLabel] = useState("");

    const registerHeadingClickHandler = (element: HTMLHeadingElement) => {
      element.addEventListener("click", (event) => {
        displayWidthContainer.current?.classList.remove("hidden");
        content.current?.classList.remove("hidden");
        setButtonLabel("Update display width");
      });
    };

    useEffect(() => {
      registerHeadingClickHandler(headingEl);
    }, []);

    const cancelClickHandler = (e: MouseEvent) => {
      serverError.current?.classList.add("hidden");
      content.current?.classList.add("hidden");
    };

    const updateHandler = (e: MouseEvent) => {
      refreshTaskSection(true);
    };

    const refreshTaskSection = (hideContent: boolean) => {
      if (displayWidthInput.current !== null) {
        const displayWidthCurrent: HTMLSelectElement = displayWidthInput.current;

        const formTemplatePart: FormTemplatePart = {
          _id: formTemplateId,
        };

        if (displayWidthCurrent.checkVisibility()) {
          const value = displayWidthCurrent.value;
          formTemplatePart["displayWidth"] = value;
        }

        const data: FormTemplateUpdateRequest = {
          formTemplate: formTemplatePart,
        };

        const updateRequest: ContentScriptRequest = {
          host: host,
          kind: MessageKind.UpdateFormTemplate,
          formTemplateId: formTemplateId,
          data: data,
        };

        onMessageHandler<APIResponse>(updateRequest, (response) => {
          if (response.ok) {
            if (displayWidthCurrent.checkVisibility()) {
              refreshLandingPageLayout(displayWidthCurrent.value);
            }
            serverError.current?.classList.add("hidden");
            if (hideContent) {
              content.current?.classList.add("hidden");
            }
          } else {
            if (serverError.current !== null) {
              if (response.error !== undefined) {
                serverError.current.innerText = response.error;
              } else {
                serverError.current.innerText = JSON.stringify(response);
              }
            }
            serverError.current?.classList.remove("hidden");
          }
        });
      }
    };

    const onDisplayWidthChange = (e: Event) => {
      refreshTaskSection(false);
    };

    const onDisplayWidthBlur = (e: Event) => {
      if (displayWidthInput.current !== null) {
        setDisplayWidthValue(displayWidthInput.current.value);
      }
    };

    return (
      <div id="edit-task-landing-page" class="info hidden" ref={content}>
        <style>{styles}</style>
        <div class="hidden" ref={displayWidthContainer}>
          <label for="edit-displayWidth">Display width</label>
          <select
            id="edit-displayWidth"
            class="form-control"
            value={displayWidthValue}
            ref={displayWidthInput}
            onChange={onDisplayWidthChange}
            onBlur={onDisplayWidthBlur}
          >
            <option value="">Default</option>
            <option value="m">m - Medium</option>
            <option value="l">l - Large</option>
            <option value="xl">xl - Very large</option>
          </select>
        </div>
        <button id="update-button" class="btn btn-success" onClick={updateHandler}>
          {buttonLabel}
        </button>
        <button id="cancel-button" class="btn btn-secondary" onClick={cancelClickHandler}>
          Cancel
        </button>
        <div class="hidden server-error" ref={serverError}></div>
      </div>
    );
  };
