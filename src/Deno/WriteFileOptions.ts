import type { Fn1, Fn2 } from "../purescript.d.ts";


export const empty: Deno.WriteFileOptions = {};

export const append_: Fn1<boolean, Deno.WriteFileOptions> = (value) => ({ append: value });

export const create: Fn1<boolean, Deno.WriteFileOptions> = (value) => ({ create: value });

export const createNew: Fn1<boolean, Deno.WriteFileOptions> = (value) => ({ createNew: value });

export const mode: Fn1<number, Deno.WriteFileOptions> = (value) => ({ mode: value });

export const combine: Fn2<Deno.WriteFileOptions, Deno.WriteFileOptions, Deno.WriteFileOptions> = (a, b) => ({ ...a, ...b });

