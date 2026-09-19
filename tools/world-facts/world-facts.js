const child = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");

const root = path.resolve(__dirname, "../..");
const coordinateKey = point => point.join(",");

function classifyPoints(points, database = process.env.WORLD_FACTS_DB || path.join(root, "out/world-facts.duckdb")) {
  if (!fs.existsSync(database)) throw new Error(`missing ${database}; run nix-shell --run 'cabal run world-facts' first`);
  const unique = [...new Map(points.map(point => [coordinateKey(point), point])).values()];
  if (!unique.length) return new Map();
  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "world-facts-"));
  const input = path.join(tempDir, "points.csv");
  fs.writeFileSync(input, `x,y,plane\n${unique.map(point => point.join(",")).join("\n")}\n`);
  const quotedInput = input.replaceAll("'", "''");
  const sql = `
    WITH requested AS (
      SELECT x::INTEGER AS x, y::INTEGER AS y, plane::INTEGER AS plane
      FROM read_csv_auto('${quotedInput}', header = true)
    ), access AS (
      SELECT x, y, plane,
             bool_or(structurally_reachable) AS reachable,
             list(DISTINCT component_id ORDER BY component_id) FILTER (WHERE component_id IS NOT NULL) AS component_ids,
             CASE
               WHEN count(component_id) = 0 THEN 'unresolved'
               WHEN count(DISTINCT component_id) > 1 THEN 'ambiguous/multiple_components'
               WHEN bool_or(structurally_reachable) THEN 'reachable'
               ELSE 'unreachable_component'
             END AS status
      FROM point_access
      GROUP BY x, y, plane
    ), tile_access AS (
      SELECT t.x, t.y, t.plane, c.structurally_reachable AS reachable,
             [t.component_id] AS component_ids,
             CASE WHEN c.structurally_reachable THEN 'reachable' ELSE 'unreachable_component' END AS status
      FROM tiles t JOIN components c USING (component_id)
    )
    SELECT r.x, r.y, r.plane,
           coalesce(a.reachable, t.reachable, false) AS reachable,
           coalesce(a.component_ids, t.component_ids, []) AS component_ids,
           coalesce(a.status, t.status, 'unresolved') AS status
    FROM requested r
    LEFT JOIN access a USING (x, y, plane)
    LEFT JOIN tile_access t USING (x, y, plane)
    ORDER BY r.x, r.y, r.plane
  `;
  try {
    const rows = JSON.parse(child.execFileSync("duckdb", [database, "-json", "-c", sql], {encoding: "utf8", maxBuffer: 64 * 1024 * 1024}));
    return new Map(rows.map(row => [coordinateKey([row.x, row.y, row.plane]), {
      reachable: row.reachable,
      componentIds: row.component_ids || [],
      status: row.status,
    }]));
  } finally {
    fs.rmSync(tempDir, {recursive: true});
  }
}

function resolveEndpoints(points, radius = 12, database = process.env.WORLD_FACTS_DB || path.join(root, "out/world-facts.duckdb")) {
  if (!fs.existsSync(database)) throw new Error(`missing ${database}; run nix-shell --run 'cabal run world-facts' first`);
  const unique = [...new Map(points.map(point => [coordinateKey(point), point])).values()];
  if (!unique.length) return new Map();
  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "world-facts-resolve-"));
  const input = path.join(tempDir, "points.csv");
  fs.writeFileSync(input, `id,x,y,plane\n${unique.map((point, id) => `${id},${point.join(",")}`).join("\n")}\n`);
  const quotedInput = input.replaceAll("'", "''");
  const sql = `
    WITH requested AS (
      SELECT id::INTEGER AS id, x::INTEGER AS x, y::INTEGER AS y, plane::INTEGER AS plane
      FROM read_csv_auto('${quotedInput}', header = true)
    ), candidates AS (
      SELECT r.id, pa.resolved_x AS x, pa.resolved_y AS y, pa.resolved_plane AS plane,
             pa.component_id, 1 AS priority
      FROM requested r
      JOIN point_access pa USING (x, y, plane)
      WHERE pa.structurally_reachable
      UNION ALL
      SELECT r.id, t.x, t.y, t.plane, t.component_id,
             CASE WHEN t.x = r.x AND t.y = r.y THEN 0 ELSE 2 END AS priority
      FROM requested r
      JOIN tiles t ON t.plane = r.plane
        AND t.x BETWEEN r.x - ${radius} AND r.x + ${radius}
        AND t.y BETWEEN r.y - ${radius} AND r.y + ${radius}
      JOIN components c USING (component_id)
      WHERE c.structurally_reachable
    )
    SELECT c.id, c.x, c.y, c.plane, c.component_id, c.priority,
           greatest(abs(c.x - r.x), abs(c.y - r.y)) AS distance,
           abs(c.x - r.x) + abs(c.y - r.y) AS manhattan
    FROM candidates c JOIN requested r USING (id)
    ORDER BY c.id, c.priority, distance, manhattan, c.x, c.y
  `;
  try {
    const rows = JSON.parse(child.execFileSync("duckdb", [database, "-json", "-c", sql], {encoding: "utf8", maxBuffer: 64 * 1024 * 1024}));
    const grouped = new Map();
    for (const row of rows) (grouped.get(row.id) || (grouped.set(row.id, []), grouped.get(row.id))).push(row);
    return new Map(unique.map((point, id) => {
      const candidates = grouped.get(id) || [];
      if (!candidates.length) return [coordinateKey(point), {raw: point, resolved: null, method: "unresolved", candidates: []}];
      const best = candidates[0];
      const tied = candidates.filter(candidate => candidate.priority === best.priority && candidate.distance === best.distance);
      const componentIds = [...new Set(tied.map(candidate => candidate.component_id))].sort((a, b) => a - b);
      return [coordinateKey(point), {
        raw: point,
        resolved: [best.x, best.y, best.plane],
        distance: best.distance,
        method: componentIds.length > 1 ? "ambiguous" : best.priority === 0 ? "exact" : best.priority === 1 ? "point_access" : "nearest_reachable",
        componentIds,
        candidates: tied.map(candidate => [candidate.x, candidate.y, candidate.plane]),
      }];
    }));
  } finally {
    fs.rmSync(tempDir, {recursive: true});
  }
}

module.exports = {classifyPoints, coordinateKey, resolveEndpoints};
