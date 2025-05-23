import { main } from './output/Main/index.js';

// Start server
main();

Deno.serve({ port: 8421 }, handler);

async function handler(request: Request): Promise<Response> {
  const html = await Deno.readTextFile('../index.html');
  return new Response(html, { headers: { 'Content-Type': 'text/html' } });
}