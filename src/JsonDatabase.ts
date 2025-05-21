import type { EffectFn1, EffectFn3, EffectFn4 } from "./purescript.d.ts";


export const _writeTextToFile: EffectFn4<string, Deno.FsFile, () => void, EffectFn1<Error, void>, void> = (text, file, onSuccess, onError) => {

  const encoder = new TextEncoder();
  const data = encoder.encode(text);
  let position = 0;

  const doWrite = () => {
      const chunk = data.slice(position);
      file.write(chunk)
          .then(numBytesWritten => {
              position += numBytesWritten;
              if (position < data.length) {
                  doWrite();
              } else {
                  onSuccess();
              }
          })
          .catch(onError);
  }
  doWrite();
}

export const _readTextFromFile: EffectFn3<Deno.FsFile, EffectFn1<string, void>, EffectFn1<Error, void>, void> = (file, onSuccess, onError) => {
  const decoder = new TextDecoder();
  let contents = "";

  const doRead = () => {

    const buffer = new Uint8Array(1024);

    file.read(buffer)
      .then(numBytesRead => {
        if (numBytesRead === null) {
          onSuccess(contents);
          return;
        }
        const str = decoder.decode(buffer.subarray(0, numBytesRead), { stream: false });
        contents += str;
        doRead();
      })
      .catch(err => {
        onError(err);
      });
  };

  doRead();
};
