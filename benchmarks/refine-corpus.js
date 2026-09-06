#!/usr/bin/env node

const fs = require("fs");
const os = require("os");
const path = require("path");
const child = require("child_process");

const root = path.resolve(__dirname, "..");
const gpsRoot = path.resolve(root, "../runelite-gps-plugin");
const questRoot = path.resolve(root, "../quest-helper");
const shortestPathRoot = path.resolve(root, "../shortest-path");
const corpusPath = path.join(__dirname, "corpus/routes-v1.json");
const routes = JSON.parse(fs.readFileSync(corpusPath, "utf8"));
const wiki = JSON.parse(fs.readFileSync(path.join(__dirname, "corpus/wiki-places-v1.json"), "utf8"));
const factsPath = process.env.WORLD_FACTS_DB || path.join(root, "data/world-facts.duckdb");
if (!fs.existsSync(factsPath)) throw new Error(`missing ${factsPath}; run nix-shell --run 'cabal run world-facts' first`);
const reachableCoordinates = new Set(child.execFileSync("duckdb", [factsPath, "-csv", "-noheader", "-c", "SELECT DISTINCT x || ',' || y || ',' || plane FROM point_access WHERE structurally_reachable"], {encoding: "utf8"}).split(/\r?\n/).filter(Boolean).map(value => value.replace(/^"|"$/g, "")));
const structurallyReachable = point => reachableCoordinates.has(point.resolved.join(","));
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

const collisionDir = fs.mkdtempSync(path.join(os.tmpdir(), "route-corpus-collision-"));
child.execFileSync("jar", ["xf", path.join(shortestPathRoot, "src/main/resources/collision-map.zip")], {cwd: collisionDir});
const collision = new Map(fs.readdirSync(collisionDir).map(name => [name, fs.readFileSync(path.join(collisionDir, name))]));
function flag(x, y, plane, direction) {
  const bytes = collision.get(`${Math.floor(x / 64)}_${Math.floor(y / 64)}`);
  if (!bytes) return false;
  const bit = ((plane * 4096) + ((y & 63) * 64) + (x & 63)) * 2 + direction;
  return Math.floor(bit / 8) < bytes.length && !!(bytes[Math.floor(bit / 8)] & (1 << (bit & 7)));
}
function walkable([x, y, plane]) {
  return flag(x, y, plane, 0) || flag(x, y, plane, 1) || flag(x, y - 1, plane, 0) || flag(x - 1, y, plane, 1);
}
function snap(raw) {
  if (walkable(raw)) return raw;
  const [x, y, plane] = raw;
  for (let radius = 1; radius <= 12; radius++) {
    const candidates = [];
    for (let dx = -radius; dx <= radius; dx++) for (let dy = -radius; dy <= radius; dy++) {
      if (Math.max(Math.abs(dx), Math.abs(dy)) === radius && walkable([x + dx, y + dy, plane])) candidates.push([x + dx, y + dy, plane]);
    }
    candidates.sort((a, b) => Math.abs(a[0] - x) + Math.abs(a[1] - y) - Math.abs(b[0] - x) - Math.abs(b[1] - y) || a[1] - b[1] || a[0] - b[0]);
    if (candidates.length) return candidates[0];
  }
  return raw;
}

function normalizeName(name) {
  return name.toLowerCase().replace(/\s+clue at \d+, \d+$/, " clue").replace(/\s*\(\d+,\s*\d+\)$/, "").replace(/[^a-z0-9]+/g, " ").trim();
}
function place(name, raw, resolved, source, kind) {
  return {name, raw, resolved, source, kind, logicalId: `${normalizeName(name)}@${Math.floor(resolved[0] / 32)},${Math.floor(resolved[1] / 32)},${resolved[2]}`};
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
const gpsPlaces = cluster(readTsv(path.join(gpsRoot, gpsFile)).flatMap(row => {
  const raw = [Number(row.x), Number(row.y), Number(row.plane)];
  if (!raw.every(Number.isInteger) || ["bank", "water", "range"].includes(row.category) || /^(altar|anvil|furnace|spinning wheel) \(\d+,\s*\d+\)$/i.test(row.name)) return [];
  return [place(row.name, raw, snap(raw), `runelite-gps-plugin@${gpsRevision.slice(0, 12)}:${gpsFile}:${row.line}`, row.category)];
}));

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
    place(route.startName, route.rawStart, route.start, route.startSource, pattern.test(route.startName) ? category : "ordinary"),
    place(route.targetName, route.rawTarget, route.target, route.targetSource, pattern.test(route.targetName) ? category : "ordinary"),
  ]).filter(point => pattern.test(point.name)));
}
const questPlaces = existingPlaces("quest-natural", /quest step$/i).filter(point => !transportCoordinates.has(point.resolved.join()));
const cluePlaces = existingPlaces("clue-natural", /clue/i).filter(point => !transportCoordinates.has(point.resolved.join()));
const npcPlaces = [
  ["Turael", [2931,3536,0], "src/main/java/com/questhelper/helpers/quests/animalmagnetism/AnimalMagnetism.java:228"],
  ["Spria", [3092,3267,0], "src/main/java/com/questhelper/helpers/quests/aporcineofinterest/APorcineOfInterest.java:140"],
  ["Mazchna", [3513,3510,0], "src/main/java/com/questhelper/helpers/achievementdiaries/morytania/MorytaniaEasy.java:231"],
  ["Vannaka", [3146,9913,0], "src/main/java/com/questhelper/helpers/achievementdiaries/varrock/VarrockMedium.java:261"],
  ["Chaeldar", [2446,4430,0], "src/main/java/com/questhelper/helpers/achievementdiaries/lumbridgeanddraynor/LumbridgeMedium.java:242"],
  ["Nieve", [2432,3424,0], "src/main/java/com/questhelper/helpers/quests/monkeymadnessii/MonkeyMadnessII.java:575"],
  ["Duradel", [2869,2982,1], "src/main/java/com/questhelper/helpers/achievementdiaries/karamja/KaramjaHard.java:290"],
].map(([name, raw, source]) => place(`${name} — Slayer master`, raw, snap(raw), source, "npc"));
for (const npc of npcPlaces) npc.source = `quest-helper@${questRevision.slice(0, 12)}:${npc.source}`;
const wikiPlaces = wiki.places.map(entry => {
  const source = wiki.sources[entry.source];
  return place(`${entry.monster} — ${entry.location}`, entry.coordinate, snap(entry.coordinate), `oldschool-wiki:${source.page}@${source.revision}#Locations`, "monster");
});
const ordinary = cluster([...gpsPlaces, ...questPlaces, ...cluePlaces, ...npcPlaces, ...wikiPlaces])
  .filter(point => !/bank/i.test(point.name) && !transportCoordinates.has(point.resolved.join()) && structurallyReachable(point));
const gpsNatural = ordinary.filter(point => !["quest-natural", "clue-natural"].includes(point.kind));
const featured = gpsNatural.filter(point => point.kind === "npc" || point.kind === "monster" || point.kind === "dungeon" || /guild/i.test(point.name));

function hash(value) {
  let result = 2166136261;
  for (const character of value) result = Math.imul(result ^ character.charCodeAt(0), 16777619);
  return result >>> 0;
}
function distance(a, b) { return Math.max(Math.abs(a.resolved[0] - b.resolved[0]), Math.abs(a.resolved[1] - b.resolved[1])); }
function distanceBand(index, a, b) {
  if (a.resolved[2] !== b.resolved[2]) return index % 4 === 3;
  const d = distance(a, b);
  return index % 4 === 0 ? d >= 20 && d < 100 : index % 4 === 1 ? d >= 100 && d < 400 : index % 4 === 2 ? d >= 400 && d < 1000 : d >= 1000;
}
function choose(pool, counts, seed, avoid, predicate = () => true) {
  const candidates = pool.filter(point => point.resolved.join() !== avoid?.resolved.join() && (counts.get(point.logicalId) || 0) < 3 && predicate(point));
  const usable = candidates.length ? candidates : pool.filter(point => point.resolved.join() !== avoid?.resolved.join());
  if (!usable.length) throw new Error("endpoint reuse cap exhausted");
  return usable.sort((a, b) => (counts.get(a.logicalId) || 0) - (counts.get(b.logicalId) || 0) || hash(`${a.logicalId}:${seed}`) - hash(`${b.logicalId}:${seed}`))[0];
}
function routeFrom(template, start, target) {
  const d = distance(start, target);
  return {
    ...template,
    name: `${start.name} → ${target.name}`,
    rawStart: start.raw, start: start.resolved, rawTarget: target.raw, target: target.resolved,
    startName: start.name, targetName: target.name, startSource: start.source, targetSource: target.source,
    allowTransports: true,
    distanceTag: start.resolved[2] !== target.resolved[2] || d >= 1000 ? "cross-world" : d >= 400 ? "long" : d >= 100 ? "regional" : "local",
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
  ["gps-natural", regenerate("gps-natural", gpsNatural.length ? gpsNatural : ordinary, 10, featured.length ? featured : ordinary, featured.length ? featured : ordinary)],
  ["quest-natural", regenerate("quest-natural", questPlaces.length ? questPlaces : ordinary, 10)],
  ["clue-natural", regenerate("clue-natural", cluePlaces.length ? cluePlaces : ordinary, 10)],
]);
const refined = routes.map(route => replacements.has(route.category) ? replacements.get(route.category).shift() : routeFrom(route,
  place(route.startName, route.rawStart, route.start, route.startSource, route.category),
  place(route.targetName, route.rawTarget, route.target, route.targetSource, route.category)));

const accidentalUnreachable = refined.filter(route => !/unreachable|regression/i.test(route.category)
  && (!reachableCoordinates.has(route.start.join(",")) || !reachableCoordinates.has(route.target.join(","))));
if (accidentalUnreachable.length) {
  const repaired = new Map();
  const used = new Map();
  for (const route of accidentalUnreachable) {
    const start = choose(ordinary, used, `repair:start:${route.id}`);
    const target = choose(ordinary, used, `repair:target:${route.id}`, start, candidate => distanceBand(hash(route.id), start, candidate));
    repaired.set(route.id, routeFrom(route, start, target));
    used.set(start.logicalId, (used.get(start.logicalId) || 0) + 1);
    used.set(target.logicalId, (used.get(target.logicalId) || 0) + 1);
  }
  for (let index = 0; index < refined.length; index++) if (repaired.has(refined[index].id)) refined[index] = repaired.get(refined[index].id);
  console.log(`repaired ${accidentalUnreachable.length} routes with structurally unreachable endpoints`);
}

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

function metrics(selected) {
  const startCounts = new Map(), targetCounts = new Map();
  for (const route of selected) {
    const start = `${normalizeName(route.startName)}@${Math.floor(route.start[0] / 32)},${Math.floor(route.start[1] / 32)},${route.start[2]}`;
    const target = `${normalizeName(route.targetName)}@${Math.floor(route.target[0] / 32)},${Math.floor(route.target[1] / 32)},${route.target[2]}`;
    startCounts.set(start, (startCounts.get(start) || 0) + 1);
    targetCounts.set(target, (targetCounts.get(target) || 0) + 1);
  }
  const pairs = new Set(selected.map(route => `${route.start.join()}>${route.target.join()}`));
  const top = counts => [...counts].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0])).slice(0, 20).map(([name, count]) => `  ${count}  ${name}`).join("\n");
  const bankStarts = selected.filter(route => /bank/i.test(route.startName)).length;
  const bankTargets = selected.filter(route => /bank/i.test(route.targetName)).length;
  const transportStarts = selected.filter(route => transportCoordinates.has(route.start.join())).length;
  const transportTargets = selected.filter(route => transportCoordinates.has(route.target.join())).length;
  return [
    `routes: ${selected.length}`,
    `unique logical starts: ${startCounts.size}`,
    `unique logical targets: ${targetCounts.size}`,
    `unique logical places: ${new Set([...startCounts.keys(), ...targetCounts.keys()]).size}`,
    `bank starts: ${bankStarts} (${(100 * bankStarts / selected.length).toFixed(1)}%)`,
    `bank targets: ${bankTargets} (${(100 * bankTargets / selected.length).toFixed(1)}%)`,
    `transport-node starts: ${transportStarts} (${(100 * transportStarts / selected.length).toFixed(1)}%)`,
    `transport-node targets: ${transportTargets} (${(100 * transportTargets / selected.length).toFixed(1)}%)`,
    `maximum logical start reuse: ${Math.max(...startCounts.values())}`,
    `maximum logical target reuse: ${Math.max(...targetCounts.values())}`,
    `bidirectional pairs: ${selected.filter(route => pairs.has(`${route.target.join()}>${route.start.join()}`)).length / 2}`,
    "top 20 starts:", top(startCounts), "top 20 targets:", top(targetCounts),
  ].join("\n");
}

const sections = [["all", refined], ...["gps-natural", "quest-natural", "clue-natural"].map(category => [category, refined.filter(route => route.category === category)])];
fs.writeFileSync(corpusPath, JSON.stringify(refined, null, 2) + "\n");
fs.writeFileSync(path.join(__dirname, "corpus/coverage-v1.txt"), sections.map(([name, selected]) => `# ${name}\n${metrics(selected)}`).join("\n\n") + "\n");
fs.rmSync(collisionDir, {recursive: true});
