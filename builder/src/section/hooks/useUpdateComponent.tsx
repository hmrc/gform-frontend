import { useRef, useEffect } from "preact/hooks";

// Do not run effect on component mount. We don't want to refresh the page on page load
export function useUpdateComponent<T>(state: T, refresh: (s: T) => void) {
  const firstUpdate = useRef(true);
  useEffect(() => {
    if (firstUpdate.current) {
      firstUpdate.current = false;
      return;
    }
    refresh(state);
  }, [state]);
}
