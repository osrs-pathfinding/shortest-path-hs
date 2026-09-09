#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const {resolveEndpoints} = require("./world-facts");

const root = path.resolve(__dirname, "..");
const input = process.argv[2] || path.join(__dirname, "corpus/routes-v1.json");
const output = process.argv[3] || input;
const database = process.env.WORLD_FACTS_DB;
const routes = JSON.parse(fs.readFileSync(input, "utf8"));
const endpoints = routes.flatMap(route => [route.rawStart, route.rawTarget]);
const resolutions = resolveEndpoints(endpoints, 12, database);

function resolutionFor(point, route, side) {
  const resolution = resolutions.get(point.join(","));
  if (!resolution?.resolved) throw new Error(`${route.id}: ${side} ${point.join(",")}: ${resolution?.method || "unresolved"}`);
  return resolution;
}

function distance(a, b) {
  return Math.max(Math.abs(a[0] - b[0]), Math.abs(a[1] - b[1]));
}

function distanceTag(start, target) {
  if (start[2] !== target[2]) return "cross-world";
  const value = distance(start, target);
  return value >= 1000 ? "cross-world" : value >= 400 ? "long" : value >= 100 ? "regional" : "local";
}

const refined = routes.map(route => {
  const startResolution = resolutionFor(route.rawStart, route, "start");
  const targetResolution = resolutionFor(route.rawTarget, route, "target");
  const start = startResolution.resolved;
  const target = targetResolution.resolved;
  return {
    ...route,
    start,
    target,
    startResolution,
    targetResolution,
    distanceTag: distanceTag(start, target),
    planeTag: `${start[2]}-to-${target[2]}`,
    startRegion: `${Math.floor(start[0] / 512)},${Math.floor(start[1] / 512)}`,
    targetRegion: `${Math.floor(target[0] / 512)},${Math.floor(target[1] / 512)}`,
  };
});

fs.writeFileSync(output, JSON.stringify(refined, null, 2) + "\n");
console.log(`refined ${routes.length} routes in place: ${output}`);
