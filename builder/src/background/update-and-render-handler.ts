import { UpdateQueueItem, UpdateRenderQueueItem } from "../types";

export const updateRenderQueue: Array<UpdateRenderQueueItem | UpdateQueueItem> = [];

export const updateRenderItem = async () => {
  const item = updateRenderQueue[0];
  if (item === undefined) {
    return;
  } else {
    const response: Response = await fetch(item.updateUrl, {
      method: "POST",
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json",
      },
      body: JSON.stringify(item.updatePayload),
    });

    const json = await response.json();

    if (item.kind === "updateRender") {
      if (response.ok) {
        const response: Response = await fetch(item.renderUrl, {
          method: "GET",
          headers: {
            Accept: "application/json",
          },
        });

        const contentType = response.headers.get("content-type");

        if (contentType !== null && contentType.includes("application/json")) {
          const json = await response.json();
          if (response.ok) {
            item.sendResponse(json);
          } else {
            const error = `Server error when rendering data. Json error: ${json.error}`;
            console.error(error);
            item.sendResponse({ error });
          }
        } else {
          const text = await response.text();
          const error = `Server error when rendering data. Response body: ${text}`;
          console.error(error);
          item.sendResponse({ error });
        }
      } else {
        console.error("Server error when saving json data", json.error);
        item.sendResponse({ error: json.error });
      }
    } else {
      item.sendResponse({ ok: response.ok, error: json.error });
    }
  }

  updateRenderQueue.shift();

  const itemsLeft = updateRenderQueue.length;

  if (itemsLeft > 1) {
    for (let i = 0; i < itemsLeft - 1; i++) {
      // Skip all the events in the queue, except the last one.
      // Form builder is slow in huge forms, skipping unnecessary update requests
      // is making the overall experience better.
      updateRenderQueue.shift();
    }
  }

  await updateRenderItem();
};
