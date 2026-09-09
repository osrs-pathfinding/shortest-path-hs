#!/usr/bin/env node

const assert = require("assert");
const child = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");
const {resolveEndpoints, coordinateKey} = require("./world-facts");

const dir = fs.mkdtempSync(path.join(os.tmpdir(), "endpoint-resolution-test-"));
const database = path.join(dir, "facts.duckdb");
child.execFileSync("duckdb", [database, "-c", `
  CREATE TABLE components(component_id INTEGER, structurally_reachable BOOLEAN);
  INSERT INTO components VALUES (1, true), (2, false), (3, true);
  CREATE TABLE tiles(x INTEGER, y INTEGER, plane INTEGER, component_id INTEGER);
  INSERT INTO tiles VALUES
    (10, 10, 0, 1), (20, 20, 0, 2), (22, 20, 0, 1),
    (30, 30, 0, 2), (31, 30, 0, 1),
    (49, 50, 0, 1), (51, 50, 0, 3);
  CREATE TABLE point_access(
    x INTEGER, y INTEGER, plane INTEGER, resolved_x INTEGER, resolved_y INTEGER,
    resolved_plane INTEGER, component_id INTEGER, structurally_reachable BOOLEAN
  );
  INSERT INTO point_access VALUES (60, 60, 0, 59, 60, 0, 1, true);
`]);

try {
  const points = [[10, 10, 0], [20, 21, 0], [30, 30, 0], [40, 40, 0], [50, 50, 0], [60, 60, 0]];
  const facts = resolveEndpoints(points, 2, database);
  const fact = point => facts.get(coordinateKey(point));
  assert.equal(fact(points[0]).method, "exact");
  assert.deepEqual(fact(points[1]).resolved, [22, 20, 0]);
  assert.equal(fact(points[2]).method, "nearest_reachable");
  assert.equal(fact(points[3]).method, "unresolved");
  assert.equal(fact(points[4]).method, "ambiguous");
  assert.deepEqual(fact(points[5]).resolved, [59, 60, 0]);
  assert.equal(fact(points[5]).method, "point_access");
  console.log("endpoint resolution: pass");
} finally {
  fs.rmSync(dir, {recursive: true});
}
