export const _responseJson = (status, data, headers) => {
    return Response.json(data, { headers, status });
};
export const requestUrl = (request) => {
    return request.url;
};
export const requestMethod = (request) => {
    return request.method;
};
export const _requestJson = (request) => {
    return request.json();
};
export const newResponse = (method, headers) => {
    return new Response(null, {
        status: 200,
        statusText: 'OK',
        headers: headers,
    });
};
