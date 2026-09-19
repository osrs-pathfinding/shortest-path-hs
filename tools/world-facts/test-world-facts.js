#!/usr/bin/env node

const assert = require("assert");
const {classifyPoints, coordinateKey} = require("./world-facts");

const points = [
  [3221, 3218, 0],
  [3439, 3543, 2],
  [2559, 2241, 0],
  [3178, 2447, 0],
  [0, 0, 0],
];
const facts = classifyPoints(points);
const fact = point => facts.get(coordinateKey(point));

assert.equal(fact(points[0]).status, "reachable");
assert.equal(fact(points[1]).status, "reachable");
assert.equal(fact(points[2]).status, "unreachable_component");
assert.equal(fact(points[3]).status, "reachable");
assert.ok(fact(points[3]).componentIds.length >= 1);
assert.equal(fact(points[4]).status, "unresolved");
console.log("world facts batch adapter: pass");
