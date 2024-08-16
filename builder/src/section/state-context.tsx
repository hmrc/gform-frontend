import { createContext } from "preact";
import type { DispatchEvent } from "../types";

export const StateContext = createContext((a: DispatchEvent<any>) => {});
