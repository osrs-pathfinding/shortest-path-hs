const http = require("http");
const fs = require("fs");
const path = require("path");
const { spawn } = require("child_process");

const root = path.resolve(__dirname, "..");
const port = Number(process.env.PORT || 8000);
const maxBodyBytes = 64 * 1024;
const routeTimeoutMs = Number(process.env.HIERARCHY_ROUTE_TIMEOUT_MS || 2 * 60 * 1000);
const startupTimeoutMs = Number(process.env.HIERARCHY_STARTUP_TIMEOUT_MS || 30 * 60 * 1000);
const types = {
  ".html": "text/html",
  ".js": "application/javascript",
  ".css": "text/css",
  ".json": "application/json",
  ".tsv": "text/tab-separated-values"
};
const doorTransportFile = process.env.DOOR_TRANSPORTS_TSV || "/home/matt/shortest-path-tooling/door_transports.tsv";

function json(res, status, value) {
  res.writeHead(status, { "content-type": "application/json; charset=utf-8" });
  res.end(JSON.stringify(value));
}

function validCoordinate(value) {
  return value && typeof value === "object" &&
    Number.isInteger(value.x) && value.x >= 0 && value.x <= 32767 &&
    Number.isInteger(value.y) && value.y >= 0 && value.y <= 32767 &&
    Number.isInteger(value.plane) && value.plane >= 0 && value.plane <= 3;
}

function parseRouteRequest(body) {
  let value;
  try {
    value = JSON.parse(body);
  } catch (_) {
    return { error: "request body must be valid JSON" };
  }
  if (!value || typeof value !== "object" || Array.isArray(value) ||
      !validCoordinate(value.start) || !validCoordinate(value.target) ||
      (value.allowTransports !== undefined && typeof value.allowTransports !== "boolean") ||
      (value.includeExpandedTiles !== undefined && typeof value.includeExpandedTiles !== "boolean") ||
      (value.useHeuristic !== undefined && typeof value.useHeuristic !== "boolean")) {
    return { error: "expected start and target coordinates and optional boolean route settings" };
  }
  return {
    value: {
      start: value.start,
      target: value.target,
      allowTransports: value.allowTransports === undefined ? true : value.allowTransports,
      includeExpandedTiles: value.includeExpandedTiles === true,
      useHeuristic: value.useHeuristic !== false
    }
  };
}

class RouteProcess {
  constructor() {
    this.child = null;
    this.starting = null;
    this.ready = false;
    this.nextId = 1;
    this.pending = new Map();
    this.stdout = "";
  }

  start() {
    if (this.starting) return this.starting;
    this.starting = new Promise((resolve, reject) => {
      const executable = process.env.HIERARCHY_ROUTE_EXECUTABLE;
      const command = executable || "nix-shell";
      const args = executable
        ? ["serve"]
        : ["shell.nix", "--run", "cabal run hierarchy-differential -- serve"];
      const child = spawn(command, args, {
        cwd: root,
        stdio: ["pipe", "pipe", "pipe"]
      });
      let settled = false;
      let startupTimer;
      this.child = child;
      this.ready = false;
      this.stdout = "";

      const failStart = error => {
        if (!settled) {
          settled = true;
          clearTimeout(startupTimer);
          this.starting = null;
          child.kill();
          reject(error);
        }
      };
      const markReady = () => {
        if (!settled) {
          settled = true;
          clearTimeout(startupTimer);
          resolve();
        }
      };
      startupTimer = setTimeout(() => failStart(new Error("hierarchy process did not become ready")), startupTimeoutMs);
      child.once("error", failStart);
      child.stdout.setEncoding("utf8");
      child.stdout.on("data", data => this.onOutput(data, markReady));
      child.stderr.setEncoding("utf8");
      child.stderr.on("data", data => process.stderr.write(`[hierarchy] ${data}`));
      child.once("exit", (code, signal) => {
        const error = new Error(`hierarchy process exited (${code === null ? signal : code})`);
        if (!settled) failStart(error);
        if (this.child !== child) return;
        this.child = null;
        this.ready = false;
        this.starting = null;
        for (const request of this.pending.values()) request.reject(error);
        this.pending.clear();
      });
    });
    return this.starting;
  }

  onOutput(data, resolveStart) {
    this.stdout += data;
    let newline;
    while ((newline = this.stdout.indexOf("\n")) !== -1) {
      const line = this.stdout.slice(0, newline).trim();
      this.stdout = this.stdout.slice(newline + 1);
      if (!line) continue;
      let message;
      try {
        message = JSON.parse(line);
      } catch (_) {
        console.error(`[hierarchy] ${line}`);
        continue;
      }
      if (message.ready === true) {
        this.ready = true;
        resolveStart();
        continue;
      }
      if (!Number.isInteger(message.id)) {
        console.error(`[hierarchy] ignored JSON message without id: ${line}`);
        continue;
      }
      const request = this.pending.get(message.id);
      if (!request) continue;
      this.pending.delete(message.id);
      clearTimeout(request.timer);
      if (message.error) request.reject(new Error(String(message.error)));
      else request.resolve(message.route === undefined ? message : message.route);
    }
  }

  async request(value) {
    const started = process.hrtime.bigint();
    await this.start();
    const ready = process.hrtime.bigint();
    if (!this.child || !this.ready) throw new Error("hierarchy process is not ready");
    const id = this.nextId++;
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pending.delete(id);
        const error = new Error("hierarchy route request timed out");
        reject(error);
        for (const request of this.pending.values()) request.reject(error);
        this.pending.clear();
        const child = this.child;
        this.ready = false;
        this.child = null;
        this.starting = null;
        child?.kill();
      }, routeTimeoutMs);
      this.pending.set(id, {
        resolve: result => resolve({
          ...result,
          timings: {
            ...(result.timings || {}),
            workerReadyMs: Number(ready - started) / 1e6,
            httpWorkerRoundTripMs: Number(process.hrtime.bigint() - ready) / 1e6
          }
        }),
        reject,
        timer
      });
      this.child.stdin.write(`${JSON.stringify({ id, ...value })}\n`, error => {
        if (error) {
          clearTimeout(timer);
          this.pending.delete(id);
          reject(error);
        }
      });
    });
  }
}

const routeProcess = new RouteProcess();

function handleRoute(req, res) {
  if (req.method !== "POST") {
    json(res, 405, { error: "method not allowed" });
    return;
  }
  let size = 0;
  let tooLarge = false;
  const chunks = [];
  req.on("data", chunk => {
    size += chunk.length;
    if (size <= maxBodyBytes) chunks.push(chunk);
    else tooLarge = true;
  });
  req.on("end", async () => {
    if (tooLarge) {
      json(res, 413, { error: "request body too large" });
      return;
    }
    const parsed = parseRouteRequest(Buffer.concat(chunks).toString("utf8"));
    if (parsed.error) {
      json(res, 400, { error: parsed.error });
      return;
    }
    try {
      json(res, 200, await routeProcess.request(parsed.value));
    } catch (error) {
      const status = /timed out/.test(error.message) ? 504 : 503;
      json(res, status, { error: error.message });
    }
  });
}

http.createServer((req, res) => {
  const requestPath = new URL(req.url, "http://127.0.0.1").pathname;
  if (requestPath === "/api/route") {
    handleRoute(req, res);
    return;
  }
  if (requestPath === "/door_transports.tsv") {
    fs.readFile(doorTransportFile, (error, body) => {
      if (error) {
        res.writeHead(404);
        res.end("not found");
        return;
      }
      res.writeHead(200, { "content-type": types[".tsv"] });
      res.end(body);
    });
    return;
  }
  const url = new URL(req.url, `http://127.0.0.1:${port}`);
  let file = path.normalize(path.join(root, url.pathname === "/" ? "viewer/index.html" : url.pathname));

  if (!file.startsWith(root)) {
    res.writeHead(403);
    res.end("forbidden");
    return;
  }

  fs.stat(file, (statErr, stat) => {
    if (!statErr && stat.isDirectory()) file = path.join(file, "index.html");
    fs.readFile(file, (readErr, body) => {
      if (readErr) {
        res.writeHead(404);
        res.end("not found");
        return;
      }
      res.writeHead(200, { "content-type": types[path.extname(file)] || "application/octet-stream" });
      res.end(body);
    });
  });
}).listen(port, "127.0.0.1", () => {
  console.log(`serving http://127.0.0.1:${port}/viewer/`);
});
