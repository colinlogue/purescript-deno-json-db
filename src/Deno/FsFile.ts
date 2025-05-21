import type { EffectFn1, EffectFn3, EffectFn4, EffectFn5 } from "../purescript.d.ts";



export const _close: EffectFn1<Deno.FsFile, void> = (file) => {
  file.close();
}

export const _lock: EffectFn4<Deno.FsFile, boolean, () => void, EffectFn1<Error, void>, void> = (file, exclusive, onSuccess, onError) => {
  file.lock(exclusive)
    .then(onSuccess)
    .catch(onError);
}

export const _unlock: EffectFn3<Deno.FsFile, () => void, EffectFn1<Error, void>, void> = (file, onSuccess, onError) => {
  file.unlock()
    .then(onSuccess)
    .catch(onError);
}

export const seekStart: Deno.SeekMode = Deno.SeekMode.Start;

export const seekCurrent: Deno.SeekMode = Deno.SeekMode.Current;

export const seekEnd: Deno.SeekMode = Deno.SeekMode.End;

export const _seek: EffectFn5<number, Deno.SeekMode, Deno.FsFile, () => void, EffectFn1<Error, void>, void> = (offset, whence, file, onSuccess, onError) => {
  file.seek(offset, whence)
    .then(onSuccess)
    .catch(onError);
}

export const _truncate: EffectFn4<number | null, Deno.FsFile, () => void, EffectFn1<Error, void>, void> = (size, file, onSuccess, onError) => {
  file.truncate(size ?? undefined)
    .then(onSuccess)
    .catch(onError);
}