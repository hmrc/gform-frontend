import { Dispatch } from "preact/hooks";
import { createContext } from "preact";
import { ExclusiveFieldVisibility, FieldVisibility } from "../types";

export const VisibilityContext = createContext<ExclusiveFieldVisibility>({ field: FieldVisibility.None });
