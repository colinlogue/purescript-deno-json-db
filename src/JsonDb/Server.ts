export const _responseJson = (status: number, data: unknown, headers: Headers): Response => {
  return Response.json(data, { headers, status });
};

export const requestUrl = (request: Request): string => {
  return request.url;
};

export const requestMethod = (request: Request): string => {
  return request.method;
};

export const _requestJson = (request: Request): Promise<unknown> => {
  return request.json();
};

export const newResponse = (method: string, headers: Headers): Response => {
  return new Response(null, {
    status: 200,
    statusText: 'OK',
    headers: headers,
  });
};
