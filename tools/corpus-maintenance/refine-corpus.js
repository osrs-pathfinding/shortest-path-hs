#!/usr/bin/env node

const fs = require("fs");
const os = require("os");
const path = require("path");
const child = require("child_process");
const {classifyPoints, coordinateKey, resolveEndpoints} = require("../world-facts/world-facts");

const root = path.resolve(__dirname, "../..");
const gpsRoot = path.resolve(process.env.GPS_PLUGIN_DIR || path.resolve(root, "../../runelite-gps-plugin"));
const questRoot = path.resolve(process.env.QUEST_HELPER_DIR || path.resolve(root, "../../quest-helper"));
const shortestPathRoot = path.resolve(process.env.SHORTEST_PATH_DIR || path.resolve(root, "../shortest-path"));
const corpusRoot = process.env.SHORTEST_PATH_CORPUS_DIR || path.resolve(root, "../shortest-path-corpus");
const corpusPath = path.join(corpusRoot, "corpus/routes-v1.json");
const routes = JSON.parse(fs.readFileSync(corpusPath, "utf8"));
const gpsRevision = child.execFileSync("git", ["-C", gpsRoot, "rev-parse", "HEAD"], {encoding: "utf8"}).trim();
const questRevision = child.execFileSync("git", ["-C", questRoot, "rev-parse", "HEAD"], {encoding: "utf8"}).trim();

function readTsv(file) {
  const rows = fs.readFileSync(file, "utf8").split(/\r?\n/);
  const headerIndex = rows.findIndex(Boolean);
  const header = rows[headerIndex].replace(/^#\s?/, "").split("\t");
  return rows.slice(headerIndex + 1).flatMap((row, index) => {
    if (!row || row.startsWith("#")) return [];
    const values = row.split("\t");
    return [{line: headerIndex + index + 2, ...Object.fromEntries(header.map((key, i) => [key, values[i] || ""]))}];
  });
}

function parseCoordinate(value) {
  const numbers = value.trim().split(/[ ,]+/).map(Number);
  return numbers.length >= 3 && numbers.slice(0, 3).every(Number.isInteger) ? numbers.slice(0, 3) : null;
}

function normalizeName(name) {
  return name.toLowerCase().replace(/\s+clue at \d+, \d+$/, " clue").replace(/\s*\(\d+,\s*\d+\)$/, "").replace(/[^a-z0-9]+/g, " ").trim();
}
function place(name, raw, resolved, source, kind) {
  const resolution = resolved || endpointResolution(raw);
  if (!resolution.resolved) throw new Error(`${name}: ${raw.join(",")}: no structurally reachable tile within radius 12`);
  return {name, raw, resolved: resolution.resolved, resolution, source, kind, logicalId: `${normalizeName(name)}@${Math.floor(resolution.resolved[0] / 32)},${Math.floor(resolution.resolved[1] / 32)},${resolution.resolved[2]}`};
}
function cluster(points) {
  const clusters = [];
  for (const point of points.sort((a, b) => a.name.localeCompare(b.name) || a.raw.join().localeCompare(b.raw.join()))) {
    const duplicate = clusters.some(existing => normalizeName(existing.name) === normalizeName(point.name)
      && existing.resolved[2] === point.resolved[2]
      && Math.max(Math.abs(existing.resolved[0] - point.resolved[0]), Math.abs(existing.resolved[1] - point.resolved[1])) <= 12);
    if (!duplicate) clusters.push(point);
  }
  return clusters;
}

const gpsFile = "src/main/resources/destinations.tsv";
const gpsRows = readTsv(path.join(gpsRoot, gpsFile)).flatMap(row => {
  const raw = [Number(row.x), Number(row.y), Number(row.plane)];
  if (!raw.every(Number.isInteger) || ["bank", "water", "range"].includes(row.category) || /^(altar|anvil|furnace|spinning wheel) \(\d+,\s*\d+\)$/i.test(row.name)) return [];
  return [{name: row.name, raw, source: `runelite-gps-plugin@${gpsRevision.slice(0, 12)}:${gpsFile}:${row.line}`, kind: row.category}];
});
const npcDefinitions = [
  ["Turael", [2931,3536,0], "src/main/java/com/questhelper/helpers/quests/animalmagnetism/AnimalMagnetism.java:228"],
  ["Spria", [3092,3267,0], "src/main/java/com/questhelper/helpers/quests/aporcineofinterest/APorcineOfInterest.java:140"],
  ["Mazchna", [3513,3510,0], "src/main/java/com/questhelper/helpers/achievementdiaries/morytania/MorytaniaEasy.java:231"],
  ["Vannaka", [3146,9913,0], "src/main/java/com/questhelper/helpers/achievementdiaries/varrock/VarrockMedium.java:261"],
  ["Chaeldar", [2446,4430,0], "src/main/java/com/questhelper/helpers/achievementdiaries/lumbridgeanddraynor/LumbridgeMedium.java:242"],
  ["Nieve", [2432,3424,0], "src/main/java/com/questhelper/helpers/quests/monkeymadnessii/MonkeyMadnessII.java:575"],
  ["Duradel", [2869,2982,1], "src/main/java/com/questhelper/helpers/achievementdiaries/karamja/KaramjaHard.java:290"],
];
const rawEndpoints = [
  ...routes.flatMap(route => [route.rawStart, route.rawTarget]),
  ...gpsRows.map(point => point.raw),
  ...npcDefinitions.map(([, raw]) => raw),
];
const endpointResolutions = resolveEndpoints(rawEndpoints);
const endpointResolution = raw => endpointResolutions.get(coordinateKey(raw)) || {resolved: null, method: "unresolved", candidates: []};
const gpsPlaces = cluster(gpsRows.map(point => place(point.name, point.raw, null, point.source, point.kind)));

const transportCoordinates = new Set();
const transportDir = path.join(gpsRoot, "src/main/resources/transports");
for (const file of fs.readdirSync(transportDir).filter(name => name.endsWith(".tsv"))) {
  for (const row of readTsv(path.join(transportDir, file))) for (const field of ["Origin", "Destination"]) {
    const coordinate = parseCoordinate(row[field] || "");
    if (coordinate) transportCoordinates.add(coordinate.join());
  }
}

function existingPlaces(category, pattern) {
  return cluster(routes.filter(route => route.category === category).flatMap(route => [
    place(route.startName, route.rawStart, null, route.startSource, pattern.test(route.startName) ? category : "ordinary"),
    place(route.targetName, route.rawTarget, null, route.targetSource, pattern.test(route.targetName) ? category : "ordinary"),
  ]).filter(point => pattern.test(point.name)));
}
const questPlaces = existingPlaces("quest-natural", /quest step$/i).filter(point => !transportCoordinates.has(point.resolved.join()));
const cluePlaces = existingPlaces("clue-natural", /clue/i).filter(point => !transportCoordinates.has(point.resolved.join()));
const npcPlaces = npcDefinitions.map(([name, raw, source]) => place(`${name} — Slayer master`, raw, null, source, "npc"));
for (const npc of npcPlaces) npc.source = `quest-helper@${questRevision.slice(0, 12)}:${npc.source}`;
const routePlaces = routes.flatMap(route => [
  place(route.startName, route.rawStart, null, route.startSource, route.category),
  place(route.targetName, route.rawTarget, null, route.targetSource, route.category),
]);
const allCandidates = cluster([...gpsPlaces, ...questPlaces, ...cluePlaces, ...npcPlaces]);
const worldFacts = classifyPoints([...allCandidates, ...routePlaces].map(point => point.resolved));
const factFor = point => worldFacts.get(coordinateKey(point.resolved));
const benchmarkEligible = point => factFor(point)?.reachable === true;
const ordinary = allCandidates.filter(point => !/bank/i.test(point.name) && !transportCoordinates.has(point.resolved.join()) && benchmarkEligible(point));
const gpsNatural = ordinary.filter(point => !["quest-natural", "clue-natural"].includes(point.kind));
const featured = gpsNatural.filter(point => point.kind === "npc" || point.kind === "monster" || point.kind === "dungeon" || /guild/i.test(point.name));
const reachableQuestPlaces = questPlaces.filter(benchmarkEligible);
const reachableCluePlaces = cluePlaces.filter(benchmarkEligible);

function hash(value) {
  let result = 2166136261;
  for (const character of value) result = Math.imul(result ^ character.charCodeAt(0), 16777619);
  return result >>> 0;
}
function distance(a, b) { return Math.max(Math.abs(a.resolved[0] - b.resolved[0]), Math.abs(a.resolved[1] - b.resolved[1])); }
function distanceTag(a, b) {
  const d = distance(a, b);
  return a.resolved[2] !== b.resolved[2] || d >= 1000 ? "cross-world" : d >= 400 ? "long" : d >= 100 ? "regional" : "local";
}
function distanceBand(index, a, b) {
  if (a.resolved[2] !== b.resolved[2]) return index % 4 === 3;
  const d = distance(a, b);
  return index % 4 === 0 ? d >= 20 && d < 100 : index % 4 === 1 ? d >= 100 && d < 400 : index % 4 === 2 ? d >= 400 && d < 1000 : d >= 1000;
}
function choose(pool, counts, seed, avoid, predicate = () => true) {
  const candidates = pool.filter(point => point.resolved.join() !== avoid?.resolved.join() && (counts.get(point.logicalId) || 0) < 3 && predicate(point));
  const usable = candidates.length ? candidates : pool.filter(point => point.resolved.join() !== avoid?.resolved.join() && (counts.get(point.logicalId) || 0) < 3);
  if (!usable.length) throw new Error("endpoint reuse cap exhausted");
  return usable.sort((a, b) => (counts.get(a.logicalId) || 0) - (counts.get(b.logicalId) || 0) || hash(`${a.logicalId}:${seed}`) - hash(`${b.logicalId}:${seed}`))[0];
}
function routeFrom(template, start, target) {
  return {
    ...template,
    name: `${start.name} → ${target.name}`,
    rawStart: start.raw, start: start.resolved, rawTarget: target.raw, target: target.resolved,
    startResolution: start.resolution, targetResolution: target.resolution,
    startName: start.name, targetName: target.name, startSource: start.source, targetSource: target.source,
    distanceTag: distanceTag(start, target),
    planeTag: `${start.resolved[2]}-to-${target.resolved[2]}`,
    startRegion: `${Math.floor(start.resolved[0] / 512)},${Math.floor(start.resolved[1] / 512)}`,
    targetRegion: `${Math.floor(target.resolved[0] / 512)},${Math.floor(target.resolved[1] / 512)}`,
  };
}
function regenerate(category, targets, reversePairs, forcedTargets = [], reverseTargets = targets) {
  const templates = routes.filter(route => route.category === category).sort((a, b) => a.id.localeCompare(b.id));
  const startsUsed = new Map(), targetsUsed = new Map(), selected = [];
  const use = (template, start, target) => {
    startsUsed.set(start.logicalId, (startsUsed.get(start.logicalId) || 0) + 1);
    targetsUsed.set(target.logicalId, (targetsUsed.get(target.logicalId) || 0) + 1);
    selected.push(routeFrom(template, start, target));
  };
  for (let pairIndex = 0; pairIndex < reversePairs; pairIndex++) {
    const first = choose(reverseTargets, startsUsed, `${category}:reverse-a:${pairIndex}`);
    const second = choose(ordinary, targetsUsed, `${category}:reverse-b:${pairIndex}`, first, candidate => distanceBand(pairIndex, first, candidate));
    use(templates[selected.length], first, second);
    use(templates[selected.length], second, first);
  }
  while (selected.length < templates.length) {
    const index = selected.length;
    const start = choose(ordinary, startsUsed, `${category}:start:${index}`);
    const forced = forcedTargets[index - reversePairs * 2];
    const target = forced && (targetsUsed.get(forced.logicalId) || 0) < 3 && forced.resolved.join() !== start.resolved.join()
      ? forced
      : choose(targets, targetsUsed, `${category}:target:${index}`, start, candidate => distanceBand(index, start, candidate));
    use(templates[index], start, target);
  }
  return selected;
}

const replacements = new Map([
  ["gps-natural", regenerate("gps-natural", gpsNatural, 10, featured, featured)],
  ["quest-natural", regenerate("quest-natural", reachableQuestPlaces, 10)],
  ["clue-natural", regenerate("clue-natural", reachableCluePlaces, 10)],
]);
let refined = routes.map(route => replacements.has(route.category) ? replacements.get(route.category).shift() : routeFrom(route,
  place(route.startName, route.rawStart, null, route.startSource, route.category),
  place(route.targetName, route.rawTarget, null, route.targetSource, route.category)));

const eligibleCoordinate = coordinate => worldFacts.get(coordinateKey(coordinate))?.reachable === true;
const categoryPools = new Map();
for (const route of routes) for (const endpoint of [
  place(route.startName, route.rawStart, null, route.startSource, route.category),
  place(route.targetName, route.rawTarget, null, route.targetSource, route.category),
]) if (benchmarkEligible(endpoint)) categoryPools.set(route.category, cluster([...(categoryPools.get(route.category) || []), endpoint]));

const startCounts = new Map(), targetCounts = new Map();
for (const route of refined) {
  if (eligibleCoordinate(route.start)) startCounts.set(coordinateKey(route.start), (startCounts.get(coordinateKey(route.start)) || 0) + 1);
  if (eligibleCoordinate(route.target)) targetCounts.set(coordinateKey(route.target), (targetCounts.get(coordinateKey(route.target)) || 0) + 1);
}
function pickReplacement(route, side, other, counts, originalPlane) {
  const pool = categoryPools.get(route.category) || ordinary;
  const candidates = pool.filter(point => point.resolved.join() !== other.resolved.join());
  const predicates = [
    point => point.resolved[2] === originalPlane && distanceTag(side === "start" ? point : other, side === "start" ? other : point) === route.distanceTag,
    point => distanceTag(side === "start" ? point : other, side === "start" ? other : point) === route.distanceTag,
    point => point.resolved[2] === originalPlane,
    () => true,
  ];
  for (const predicate of predicates) {
    const usable = candidates.filter(point => (counts.get(coordinateKey(point.resolved)) || 0) < 4 && predicate(point));
    if (usable.length) return usable.sort((a, b) => (counts.get(coordinateKey(a.resolved)) || 0) - (counts.get(coordinateKey(b.resolved)) || 0) || hash(`${a.logicalId}:repair:${route.id}:${side}`) - hash(`${b.logicalId}:repair:${route.id}:${side}`))[0];
  }
  throw new Error(`${route.id}: no structurally reachable ${side} replacement in ${route.category}`);
}
refined = refined.map(route => {
  if (route.expectedReachable === false || (eligibleCoordinate(route.start) && eligibleCoordinate(route.target))) return route;
  let start = place(route.startName, route.rawStart, null, route.startSource, route.category);
  let target = place(route.targetName, route.rawTarget, null, route.targetSource, route.category);
  const startInvalid = !eligibleCoordinate(route.start), targetInvalid = !eligibleCoordinate(route.target);
  if (startInvalid && targetInvalid) {
    start = pickReplacement(route, "start", target, startCounts, route.start[2]);
    target = pickReplacement(route, "target", start, targetCounts, route.target[2]);
  } else if (startInvalid) start = pickReplacement(route, "start", target, startCounts, route.start[2]);
  else target = pickReplacement(route, "target", start, targetCounts, route.target[2]);
  startCounts.set(coordinateKey(start.resolved), (startCounts.get(coordinateKey(start.resolved)) || 0) + 1);
  targetCounts.set(coordinateKey(target.resolved), (targetCounts.get(coordinateKey(target.resolved)) || 0) + 1);
  return routeFrom(route, start, target);
});

const finalInvalid = refined.filter(route => route.expectedReachable !== false && (!eligibleCoordinate(route.start) || !eligibleCoordinate(route.target)));
if (finalInvalid.length) throw new Error(finalInvalid.map(route => `${route.id}: final route is structurally unreachable`).join("\n"));

for (const category of ["gps-natural", "quest-natural", "clue-natural"]) {
  const selected = refined.filter(route => route.category === category);
  const bankStarts = selected.filter(route => /bank/i.test(route.startName)).length;
  const transportStarts = selected.filter(route => transportCoordinates.has(route.start.join())).length;
  if (bankStarts / selected.length > 0.15) throw new Error(`${category}: too many bank starts`);
  if (transportStarts / selected.length > 0.05) throw new Error(`${category}: too many transport-node starts`);
}
const routePairs = new Set(refined.map(route => `${route.start.join()}>${route.target.join()}`));
const bidirectionalPairs = refined.filter(route => routePairs.has(`${route.target.join()}>${route.start.join()}`)).length / 2;
if (bidirectionalPairs < 40 || bidirectionalPairs > 60) throw new Error(`expected 40-60 bidirectional pairs, found ${bidirectionalPairs}`);
const countsBy = (values, key) => Object.fromEntries([...values.reduce((counts, value) => counts.set(key(value), (counts.get(key(value)) || 0) + 1), new Map())].sort());
if (refined.length !== routes.length) throw new Error(`route count changed: ${routes.length} -> ${refined.length}`);
if (JSON.stringify(countsBy(refined, route => route.category)) !== JSON.stringify(countsBy(routes, route => route.category))) throw new Error("category counts changed");
if (JSON.stringify(countsBy(refined.flatMap(route => route.tiers), tier => tier)) !== JSON.stringify(countsBy(routes.flatMap(route => route.tiers), tier => tier))) throw new Error("tier counts changed");

fs.writeFileSync(corpusPath, JSON.stringify(refined, null, 2) + "\n");
