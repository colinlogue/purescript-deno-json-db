import type { EffectFn1, EffectFn2, EffectFn3, Fn1 } from "../purescript.d.ts";



export const _requestURL: Fn1<Request, string> = (request) => request.url;

export const _jsonResponse: EffectFn1<unknown, Response> = (data) => Response.json(data, { headers: { "Access-Control-Allow-Origin": "*" } });

export const _errorResponse: EffectFn2<string, number, Response> = (message, status) => {
  return Response.json({ error: message }, { status, headers: { "Access-Control-Allow-Origin": "*" } });
};

export const _requestJson: EffectFn3<Request, EffectFn1<unknown, void>, EffectFn1<Error, void>, void> = (request, onSuccess, onError) => {
  request.json()
    .then(onSuccess)
    .catch(onError);
};

export const _requestMethod: Fn1<Request, string> = (request) => request.method;

export const _runServer: EffectFn2<number, EffectFn1<Request, Promise<Response>>, () => void> = (port, handler) => {
  const abortController = new AbortController();
  const signal = abortController.signal;

  const _handler = async (request: Request) => {
    if (request.method === "OPTIONS") {
      return new Response(null, {
        headers: {
          "Access-Control-Allow-Origin": "*",
          "Access-Control-Allow-Methods": "GET, PUT, OPTIONS",
          "Access-Control-Allow-Headers": "Content-Type",
        },
      });
    }
    const response = await handler(request);
    response.headers.set("Access-Control-Allow-Origin", "*");

    console.log({ request, response })

    return response;
  };

  Deno.serve({ port, signal }, _handler);
  return () => {
    abortController.abort();
  };
};
