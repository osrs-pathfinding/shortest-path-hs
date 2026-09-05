#!/usr/bin/env node

const fs = require("fs");
const input = process.argv[2] || "benchmarks/corpus/routes-v1.json";
const routes = JSON.parse(fs.readFileSync(input, "utf8"));
const tiers = new Set(["smoke", "standard", "full"]);
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
    (!Array.isArray(route.tiers) || !route.tiers.includes("full") || route.tiers.some(tier => !tiers.has(tier))) && "tiers must include full and only contain smoke, standard, full"
  ].filter(Boolean).map(error => `${route.id || "<missing id>"}: ${error}`);
});
if (invalid.length) throw new Error(invalid.join("\n"));
console.log(`valid corpus: ${routes.length} routes`);
