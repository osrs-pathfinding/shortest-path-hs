#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const {classifyPoints, coordinateKey} = require("./world-facts");
const args = process.argv.slice(2);
let input = "benchmarks/corpus/routes-v1.json";
let factsPath = process.env.WORLD_FACTS_DB || path.resolve("data/world-facts.duckdb");
for (let index = 0; index < args.length; index++) {
  if (args[index] === "--world-facts") {
    if (!args[index + 1]) throw new Error("--world-facts requires a path");
    factsPath = args[++index];
  }
  else if (!args[index].startsWith("--")) input = args[index];
}
const skipWorldFacts = args.includes("--no-world-facts");
const routes = JSON.parse(fs.readFileSync(input, "utf8"));
const tiers = new Set(["smoke", "standard", "full"]);
const profiles = ["early", "mid", "end", "maxed"];
const ids = new Set();
const coordinate = value => Array.isArray(value) && value.length === 3 && value.every(Number.isInteger) && value[0] >= 0 && value[0] <= 32767 && value[1] >= 0 && value[1] <= 32767 && value[2] >= 0 && value[2] <= 3;
const invalid = routes.flatMap(route => {
  const duplicate = ids.has(route.id);
  ids.add(route.id);
  return [
    (!route.id || typeof route.id !== "string" || duplicate) && "id must be unique",
    !coordinate(route.rawStart) && "rawStart must be [x, y, plane]", !coordinate(route.start) && "start must be [x, y, plane]",
    !coordinate(route.rawTarget) && "rawTarget must be [x, y, plane]", !coordinate(route.target) && "target must be [x, y, plane]",
    !route.name && "name is required", !route.category && "category is required",
    !route.startName && "startName is required", !route.targetName && "targetName is required",
    !route.startSource && "startSource is required", !route.targetSource && "targetSource is required",
    (!Array.isArray(route.negativeProfiles || []) || (route.negativeProfiles || []).some(profile => !profiles.includes(profile)) || new Set(route.negativeProfiles || []).size !== (route.negativeProfiles || []).length) && "negativeProfiles must contain unique benchmark profiles",
    (!Array.isArray(route.tiers) || !route.tiers.includes("full") || route.tiers.some(tier => !tiers.has(tier))) && "tiers must include full and only contain smoke, standard, full"
  ].filter(Boolean).map(error => `${route.id || "<missing id>"}: ${error}`);
});
if (invalid.length) throw new Error(invalid.join("\n"));

if (!skipWorldFacts) {
  const facts = classifyPoints(routes.flatMap(route => [route.start, route.target]), factsPath);
  const structuralErrors = routes.flatMap(route => {
    const start = facts.get(coordinateKey(route.start));
    const target = facts.get(coordinateKey(route.target));
    return [
      !start.reachable && `${route.id}: start ${route.startName} [${route.start}] is ${start.status} (${start.componentIds.join(",") || "no component"})`,
      !target.reachable && `${route.id}: target ${route.targetName} [${route.target}] is ${target.status} (${target.componentIds.join(",") || "no component"})`,
    ].filter(Boolean);
  });
  if (structuralErrors.length) throw new Error(structuralErrors.join("\n"));
}

const exclusionPath = path.join(path.dirname(input), "excluded-routes-v1.json");
const exclusions = fs.existsSync(exclusionPath) ? JSON.parse(fs.readFileSync(exclusionPath, "utf8")) : [];
const excludedIds = new Set();
const exclusionErrors = exclusions.flatMap(exclusion => {
  const duplicate = excludedIds.has(exclusion.routeId);
  excludedIds.add(exclusion.routeId);
  return [
    (!exclusion.routeId || duplicate) && "routeId must be unique",
    ids.has(exclusion.routeId) && "route is also in the benchmark corpus",
    !exclusion.reason && "reason is required",
    !exclusion.notes && "notes are required",
  ].filter(Boolean).map(error => `${exclusion.routeId || "<missing id>"}: ${error}`);
});
if (exclusionErrors.length) throw new Error(exclusionErrors.join("\n"));

const oraclePath = path.join(path.dirname(input), "oracle-v1.json");
if (fs.existsSync(oraclePath)) {
  const oracle = JSON.parse(fs.readFileSync(oraclePath, "utf8"));
  const expectedKeys = new Set(routes.flatMap(route => profiles.map(profile => `${route.id}/${profile}`)));
  const oracleErrors = routes.flatMap(route => profiles.flatMap(profile => {
    const key = `${route.id}/${profile}`;
    const expectedReachable = !(route.negativeProfiles || []).includes(profile);
    return [
      !oracle[key] && `${key}: oracle entry is missing`,
      oracle[key] && oracle[key].reachable !== expectedReachable && `${key}: corpus expectation disagrees with oracle`,
    ].filter(Boolean);
  })).concat(Object.keys(oracle).filter(key => !expectedKeys.has(key)).map(key => `${key}: oracle entry is outside the corpus`));
  if (oracleErrors.length) throw new Error(oracleErrors.join("\n"));
}

const sentinelPath = path.join(path.dirname(input), "sentinels-v1.json");
if (fs.existsSync(sentinelPath)) {
  const profileSet = new Set(profiles);
  const sentinels = JSON.parse(fs.readFileSync(sentinelPath, "utf8"));
  const sentinelErrors = sentinels.flatMap(sentinel => [
    !ids.has(sentinel.routeId) && `${sentinel.routeId}: sentinel route does not exist`,
    !profileSet.has(sentinel.accountProfile) && `${sentinel.routeId}: invalid sentinel account profile`,
  ].filter(Boolean));
  if (sentinelErrors.length) throw new Error(sentinelErrors.join("\n"));
}

console.log(`valid corpus: ${routes.length} routes, ${routes.reduce((count, route) => count + (route.negativeProfiles || []).length, 0)} negative cases, ${exclusions.length} excluded routes${skipWorldFacts ? " (structural validation skipped)" : ", all endpoints structurally reachable"}`);
