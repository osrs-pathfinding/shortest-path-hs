const child = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");

const root = path.resolve(__dirname, "..");
const coordinateKey = point => point.join(",");

function classifyPoints(points, database = process.env.WORLD_FACTS_DB || path.join(root, "data/world-facts.duckdb")) {
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

module.exports = {classifyPoints, coordinateKey};
