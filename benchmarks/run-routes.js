#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const root = path.resolve(__dirname, "..");
const corpus = JSON.parse(fs.readFileSync(path.join(__dirname, "routes.json"), "utf8"));
const baseUrl = process.env.BENCHMARK_URL || "http://127.0.0.1:8080";
const runs = positiveInteger(process.env.BENCHMARK_RUNS || "3", "BENCHMARK_RUNS");
const filter = process.env.BENCHMARK_FILTER;
const cases = filter ? corpus.filter(test => test.name.toLowerCase().includes(filter.toLowerCase())) : corpus;
const outputPath = process.env.BENCHMARK_OUTPUT || path.join(root, "out", "route-benchmark.json");

if (!cases.length) throw new Error(`no routes match BENCHMARK_FILTER=${JSON.stringify(filter)}`);

function positiveInteger(value, name) {
  const parsed = Number(value);
  if (!Number.isInteger(parsed) || parsed < 1) throw new Error(`${name} must be a positive integer`);
  return parsed;
}

function point([x, y, plane]) {
  return { x, y, plane };
}

async function query(test, useHeuristic) {
  const response = await fetch(`${baseUrl}/api/route`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      start: point(test.start),
      target: point(test.target),
      allowTransports: test.allowTransports,
      includeExpandedTiles: false,
      useHeuristic
    })
  });
  const result = await response.json();
  if (!response.ok || result.error) throw new Error(`${test.name}: ${result.error || response.status}`);
  if (!Number.isSafeInteger(result.cost)) throw new Error(`${test.name}: no finite route found`);
  return result;
}

function percentile(values, fraction) {
  if (!values.length) return null;
  const sorted = [...values].sort((a, b) => a - b);
  return sorted[Math.ceil(fraction * sorted.length) - 1];
}

function median(values) {
  return percentile(values, 0.5);
}

function summariseSamples(samples) {
  const timings = samples.map(sample => sample.timings);
  const fields = ["sourceAttachmentMs", "targetAttachmentMs", "heuristicMs", "abstractSearchMs", "reconstructionMs", "totalMs", "httpWorkerRoundTripMs"];
  return {
    cost: samples[0].cost,
    expandedNodes: median(samples.map(sample => sample.expandedNodes)),
    timings: Object.fromEntries(fields.map(field => [field, median(timings.map(value => value[field] || 0))])),
    search: Object.fromEntries(Object.keys(timings[0].search).map(field => [field, median(timings.map(value => value.search[field]))])),
    samples: samples.map(sample => ({ cost: sample.cost, expandedNodes: sample.expandedNodes, timings: sample.timings }))
  };
}

function categorySummary(results, mode) {
  const grouped = Map.groupBy(results, result => result.category);
  return Object.fromEntries([...grouped].map(([category, values]) => {
    const selected = values.map(value => value[mode]);
    const totals = selected.map(value => value.timings.totalMs);
    return [category, {
      routes: values.length,
      p50Ms: percentile(totals, 0.5),
      p95Ms: percentile(totals, 0.95),
      p50ExpandedNodes: percentile(selected.map(value => value.expandedNodes), 0.5),
      p50SourceAttachmentMs: percentile(selected.map(value => value.timings.sourceAttachmentMs), 0.5),
      p50TargetAttachmentMs: percentile(selected.map(value => value.timings.targetAttachmentMs), 0.5),
      p50HeuristicMs: percentile(selected.map(value => value.timings.heuristicMs), 0.5),
      p50AbstractSearchMs: percentile(selected.map(value => value.timings.abstractSearchMs), 0.5),
      p50ReconstructionMs: percentile(selected.map(value => value.timings.reconstructionMs), 0.5)
    }];
  }));
}

function printCategoryTable(summary, mode) {
  console.log(`\n${mode}:`);
  console.table(Object.entries(summary).map(([category, value]) => ({ category, ...value })));
}

async function main() {
  console.log(`Benchmarking ${cases.length} routes x ${runs} runs x A*/Dijkstra via ${baseUrl}`);
  await query(cases[0], true);
  await query(cases[0], false);

  const results = [];
  for (const [index, test] of cases.entries()) {
    const samples = { astar: [], dijkstra: [] };
    const order = index % 2 ? [["dijkstra", false], ["astar", true]] : [["astar", true], ["dijkstra", false]];
    for (let run = 0; run < runs; run++) {
      for (const [mode, useHeuristic] of order) samples[mode].push(await query(test, useHeuristic));
    }
    const astar = summariseSamples(samples.astar);
    const dijkstra = summariseSamples(samples.dijkstra);
    if (astar.cost !== dijkstra.cost) throw new Error(`${test.name}: A* cost ${astar.cost} != Dijkstra cost ${dijkstra.cost}`);
    if (samples.astar.some(sample => sample.cost !== astar.cost) || samples.dijkstra.some(sample => sample.cost !== dijkstra.cost)) {
      throw new Error(`${test.name}: route cost changed between runs`);
    }
    results.push({ ...test, astar, dijkstra });
    console.log(`${String(index + 1).padStart(2)}/${cases.length} ${test.name}: cost=${astar.cost}, A*=${astar.timings.totalMs.toFixed(1)}ms/${astar.expandedNodes} nodes, Dijkstra=${dijkstra.timings.totalMs.toFixed(1)}ms/${dijkstra.expandedNodes} nodes`);
  }

  const summary = {
    astar: categorySummary(results, "astar"),
    dijkstra: categorySummary(results, "dijkstra")
  };
  fs.mkdirSync(path.dirname(outputPath), { recursive: true });
  fs.writeFileSync(outputPath, JSON.stringify({ generatedAt: new Date().toISOString(), baseUrl, runs, routes: results, summary }, null, 2));
  printCategoryTable(summary.astar, "A*");
  printCategoryTable(summary.dijkstra, "Dijkstra");
  console.log(`\nWrote ${outputPath}`);
}

main().catch(error => {
  console.error(error.stack || error.message);
  process.exitCode = 1;
});
