// =============================================================================
// RAAB7 invocation server — zero-dependency (Node core only).
//
// The default CMD of the raab-analysis image. Reachable ONLY on the internal
// overlay/pod network — there is NO auth token, so it must never be published
// externally (deploy concern; see docker/DL-2425-rabb-image-ci-plan.md §7).
//
//   POST /run  {"raabId":"<id>"} -> runs `Rscript rmd_wrapper_PEEK_server.R <id>`
//              in /raab7, streams combined stdout+stderr as chunked text/plain,
//              ends with a final line `__EXIT__ <code>` carrying the exit status.
//   GET  /health -> 200 "ok"
//
// USERNAME / PASSWORD / BASIC_AUTH (raab.world creds) are inherited from the
// process env by the spawned Rscript.
// =============================================================================
'use strict';

const http = require('http');
const { spawn } = require('child_process');

const PORT = Number(process.env.PORT || 8000);
const CWD = '/raab7';

const server = http.createServer((req, res) => {
  if (req.method === 'GET' && req.url === '/health') {
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    return res.end('ok');
  }

  if (req.method !== 'POST' || req.url !== '/run') {
    res.writeHead(404, { 'Content-Type': 'text/plain' });
    return res.end('not found\n');
  }

  let buf = '';
  req.on('data', (c) => {
    buf += c;
  });
  req.on('end', () => {
    let raabId;
    try {
      ({ raabId } = JSON.parse(buf || '{}'));
    } catch {
      res.writeHead(400, { 'Content-Type': 'text/plain' });
      return res.end('invalid JSON body\n');
    }
    if (!raabId) {
      res.writeHead(400, { 'Content-Type': 'text/plain' });
      return res.end('missing raabId\n');
    }

    // USERNAME/PASSWORD/BASIC_AUTH are inherited from the process env.
    const child = spawn('Rscript', ['rmd_wrapper_PEEK_server.R', String(raabId)], { cwd: CWD });

    res.writeHead(200, { 'Content-Type': 'text/plain', 'Transfer-Encoding': 'chunked' });
    child.stdout.pipe(res, { end: false });
    child.stderr.pipe(res, { end: false }); // combined, ordered-ish
    child.on('close', (code) => res.end(`\n__EXIT__ ${code}\n`)); // sentinel carries exit status
    child.on('error', (e) => res.end(`\n__EXIT__ 1 ${e.message}\n`));
  });
});

server.listen(PORT, '0.0.0.0', () => {
  // eslint-disable-next-line no-console
  console.log(`raab-analysis HTTP server listening on :${PORT}`);
});
