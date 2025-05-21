import type { Fn1, Fn2 } from "../purescript.d.ts";


export const recursive: Fn1<boolean, Deno.MkdirOptions> = (value) => ({ recursive: value });

export const mode: Fn1<number, Deno.MkdirOptions> = (value) => ({ mode: value });

export const empty: Deno.MkdirOptions = {};

export const combine: Fn2<Deno.MkdirOptions, Deno.MkdirOptions, Deno.MkdirOptions> = (a, b) => ({ ...a, ...b });
