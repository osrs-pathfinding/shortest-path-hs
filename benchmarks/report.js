#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const [input = "out/route-benchmark.jsonl", output = "out/route-benchmark-bencher.json", sentinelsPath = "benchmarks/corpus/sentinels-v1.json"] = process.argv.slice(2);
const samples = fs.readFileSync(input, "utf8").trim().split(/\r?\n/).filter(Boolean).map(JSON.parse).filter(sample => sample.timings);
const sentinels = new Set(JSON.parse(fs.readFileSync(sentinelsPath, "utf8")).map(item => `${item.routeId}/${item.accountProfile}`));
if (!samples.length) throw new Error(`no benchmark samples in ${input}`);

const percentile = (values, p) => [...values].sort((a, b) => a - b)[Math.ceil(values.length * p) - 1];
const median = values => percentile(values, 0.5);
const grouped = Map.groupBy(samples, sample => `${sample.routeId}/${sample.accountProfile}`);
const cases = [...grouped.values()].map(group => ({ ...group[0], timings: Object.fromEntries(Object.keys(group[0].timings).map(key => [key, median(group.map(sample => sample.timings[key]).filter(Number.isFinite))])), expandedNodes: median(group.map(sample => sample.expandedNodes)) }));
const bmf = {};
const add = (name, metric, value) => {
  if (!Number.isFinite(value)) return;
  (bmf[name] ||= {})[metric] = { value };
};
const metrics = [["totalMs", "total-time", 1e6], ["searchMs", "search-time", 1e6], ["reverseDijkstraMs", "reverse-setup-time", 1e6], ["expandedNodes", "states-popped", 1]];
for (const [profile, values] of Map.groupBy(cases, value => value.accountProfile)) {
  for (const [field, metric, scale] of metrics) for (const p of [0.5, 0.95, 0.99]) add(`aggregate/${profile}/${field}-p${p * 100}`, metric, percentile(values.map(value => field === "expandedNodes" ? value.expandedNodes : value.timings[field]), p) * scale);
  for (const [category, categoryValues] of Map.groupBy(values, value => value.category)) for (const [field, metric, scale] of metrics) add(`category/${profile}/${category}/${field}-p50`, metric, median(categoryValues.map(value => field === "expandedNodes" ? value.expandedNodes : value.timings[field])) * scale);
}
for (const value of cases) if (sentinels.has(`${value.routeId}/${value.accountProfile}`)) {
  const name = `sentinel/${value.routeId}/${value.accountProfile}`;
  add(name, "total-time", value.timings.totalMs * 1e6);
  add(name, "search-time", value.timings.searchMs * 1e6);
  add(name, "reverse-setup-time", value.timings.reverseDijkstraMs * 1e6);
  add(name, "states-popped", value.expandedNodes);
}
fs.mkdirSync(path.dirname(output), { recursive: true });
fs.writeFileSync(output, JSON.stringify(bmf, null, 2));
console.log(`wrote ${output} (${Object.keys(bmf).length} benchmarks)`);
