import type { Fn1, Fn2 } from "../purescript.d.ts";



export const empty: Deno.OpenOptions = {};

export const read: Fn1<boolean, Deno.OpenOptions> = (value) => ({ read: value });

export const write: Fn1<boolean, Deno.OpenOptions> = (value) => ({ write: value });

export const append_: Fn1<boolean, Deno.OpenOptions> = (value) => ({ append: value });

export const truncate: Fn1<boolean, Deno.OpenOptions> = (value) => ({ truncate: value });

export const create: Fn1<boolean, Deno.OpenOptions> = (value) => ({ create: value });

export const createNew: Fn1<boolean, Deno.OpenOptions> = (value) => ({ createNew: value });

export const mode: Fn1<number, Deno.OpenOptions> = (value) => ({ mode: value });

export const combine: Fn2<Deno.OpenOptions, Deno.OpenOptions, Deno.OpenOptions> = (a, b) => ({ ...a, ...b });

