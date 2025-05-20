import type { Fn1 } from "../purescript.d.ts";



export const isNotFoundError: Fn1<Error, boolean> = (error) => error instanceof Deno.errors.NotFound;

