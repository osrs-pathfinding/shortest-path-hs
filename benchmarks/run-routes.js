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

const modes = [
  ["raw", { finder: "raw", useHeuristic: false }],
  ["tileFull", { finder: "tile-full", useHeuristic: true }]
];
if (process.env.BENCHMARK_INCLUDE_HIERARCHY === "1") {
  modes.push(["hierarchical", { finder: "hierarchical", useHeuristic: true }]);
}

async function query(test, options) {
  const response = await fetch(`${baseUrl}/api/route`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      start: point(test.start),
      target: point(test.target),
      allowTransports: test.allowTransports,
      includeExpandedTiles: false,
      useHeuristic: options.useHeuristic,
      finder: options.finder
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
  const fields = [
    "setupMs",
    "reverseDijkstraMs",
    "seedTableMs",
    "distanceTransformMs",
    "searchMs",
    "totalMs",
    "sourceAttachmentMs",
    "targetAttachmentMs",
    "heuristicMs",
    "abstractSearchMs",
    "reconstructionMs",
    "httpWorkerRoundTripMs"
  ];
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
      p50SetupMs: percentile(selected.map(value => value.timings.setupMs || value.timings.heuristicMs || 0), 0.5),
      p50SearchMs: percentile(selected.map(value => value.timings.searchMs || value.timings.abstractSearchMs || 0), 0.5)
    }];
  }));
}

function printCategoryTable(summary, mode) {
  console.log(`\n${mode}:`);
  console.table(Object.entries(summary).map(([category, value]) => ({ category, ...value })));
}

async function main() {
  console.log(`Benchmarking ${cases.length} routes x ${runs} runs x ${modes.length} modes via ${baseUrl}`);
  for (const [, options] of modes) await query(cases[0], options);

  const results = [];
  const mismatches = [];
  for (const [index, test] of cases.entries()) {
    const samples = Object.fromEntries(modes.map(([mode]) => [mode, []]));
    const order = index % 2 ? [...modes].reverse() : modes;
    for (let run = 0; run < runs; run++) {
      for (const [mode, options] of order) samples[mode].push(await query(test, options));
    }
    const summaries = Object.fromEntries(modes.map(([mode]) => [mode, summariseSamples(samples[mode])]));
    const costs = modes.map(([mode]) => summaries[mode].cost);
    if (!costs.every(cost => cost === costs[0])) mismatches.push({
      name: test.name,
      type: "mode costs differ",
      costs: Object.fromEntries(modes.map(([mode]) => [mode, summaries[mode].cost]))
    });
    for (const [mode] of modes) {
      if (samples[mode].some(sample => sample.cost !== summaries[mode].cost)) mismatches.push({
        name: test.name,
        type: `${mode} route cost changed between runs`,
        costs: samples[mode].map(sample => sample.cost)
      });
    }
    results.push({ ...test, ...summaries });
    console.log(`${String(index + 1).padStart(2)}/${cases.length} ${test.name}: cost=${costs[0]}, ${modes.map(([mode]) => `${mode}=${summaries[mode].cost}/${summaries[mode].timings.totalMs.toFixed(1)}ms/${summaries[mode].expandedNodes}`).join(", ")}`);
  }

  const summary = Object.fromEntries(modes.map(([mode]) => [mode, categorySummary(results, mode)]));
  fs.mkdirSync(path.dirname(outputPath), { recursive: true });
  fs.writeFileSync(outputPath, JSON.stringify({ generatedAt: new Date().toISOString(), baseUrl, runs, routes: results, summary, mismatches }, null, 2));
  for (const [mode] of modes) printCategoryTable(summary[mode], mode);
  if (mismatches.length) {
    console.log("\nMismatches:");
    console.table(mismatches);
  }
  console.log(`\nWrote ${outputPath}`);
}

main().catch(error => {
  console.error(error.stack || error.message);
  process.exitCode = 1;
});
