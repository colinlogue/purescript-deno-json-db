import type { EffectFn1, EffectFn3, EffectFn4, EffectFn5 } from "./purescript.d.ts";



export const _mkdir: EffectFn4<Deno.MkdirOptions, string, () => void, EffectFn1<Error, void>, void> = (opts, path, onSuccess, onError) => {
  Deno.mkdir(path, opts)
    .then(onSuccess)
    .catch(onError);
};

export const _readTextFile: EffectFn3<string, EffectFn1<string, void>, EffectFn1<Error, void>, void> = (path, onSuccess, onError) => {
  Deno.readTextFile(path)
    .then(onSuccess)
    .catch(onError);
};

export const _writeTextFile: EffectFn5<Deno.WriteFileOptions, string, string, () => void, EffectFn1<Error, void>, void> = (opts, path, data, onSuccess, onError) => {
  Deno.writeTextFile(path, data, opts)
    .then(onSuccess)
    .catch(onError);
};

export const _open: EffectFn4<Deno.OpenOptions, string, EffectFn1<Deno.FsFile, void>, EffectFn1<Error, void>, void> = (opts, path, onSuccess, onError) => {
  Deno.open(path, opts)
    .then(file => { file.path = path; onSuccess(file); })
    .catch(onError);
}

declare global {
  namespace Deno {
    interface FsFile {
      path?: string;
    }
  }
}
