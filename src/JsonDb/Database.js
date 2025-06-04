export const _writeTextToFile = (text, file, onSuccess, onError) => {
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
            }
            else {
                onSuccess();
            }
        })
            .catch(onError);
    };
    doWrite();
};
export const _readTextFromFile = (file, onSuccess, onError) => {
    const decoder = new TextDecoder();
    let contents = "";
    const doRead = () => {
        const buffer = new Uint8Array(1024);
        file.read(buffer)
            .then(numBytesRead => {
            if (numBytesRead === null || numBytesRead === 0) {
                onSuccess(contents);
            }
            else {
                contents += decoder.decode(buffer.subarray(0, numBytesRead));
                doRead();
            }
        })
            .catch(onError);
    };
    doRead();
};
