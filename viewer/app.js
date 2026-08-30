const tileBaseUrl = "https://maps.runescape.wiki/osrs/versions/2026-03-04_a/tiles/rendered";
const mapId = -1;
const modeSelect = document.getElementById("mode-select");
const componentSelect = document.getElementById("component-select");
const planeSelect = document.getElementById("plane-select");
const opacityInput = document.getElementById("opacity");
const fitButton = document.getElementById("fit");
const status = document.getElementById("status");
const legend = document.getElementById("legend");
const stats = document.getElementById("stats");
const routeCaseSelect = document.getElementById("route-case-select");
const routeStatus = document.getElementById("route-status");
const routeStats = document.getElementById("route-stats");
const routeInputs = {
  start: ["start-x", "start-y", "start-plane"],
  end: ["end-x", "end-y", "end-plane"]
};

let data = null;
let currentPlane = 0;
let shapeLayer;
let bboxLayer;
let routeLayer;
let expandedLayer;
let regionLayer;
let cutLayer;
let separatorLayer;
let heuristicBounds;
let route = null;
let fixtureRoutes = [];
let routeMarkers = [];
let partitions = [];
let kahipPartitions = [];
let cutEdges = [];
const loadState = { metis: "loading", kahip: "loading", cuts: "loading" };

const WikiTileLayer = L.TileLayer.extend({
  getTileUrl(coords) {
    return `${tileBaseUrl}/${mapId}/${coords.z}/${currentPlane}_${coords.x}_${-(1 + coords.y)}.png`;
  },
  createTile(coords, done) {
    const tile = L.TileLayer.prototype.createTile.call(this, coords, done);
    tile.onerror = () => {};
    return tile;
  }
});

const map = L.map("map", {
  crs: L.CRS.Simple, minZoom: -4, maxZoom: 4, zoomSnap: 1,
  center: [3200, 3200], zoom: 1,
  maxBounds: [[-1000, -1000], [13800, 13800]], maxBoundsViscosity: 0.5
});
new WikiTileLayer("", {
  minZoom: -4, minNativeZoom: -2, maxNativeZoom: 3, maxZoom: 4, noWrap: true
}).addTo(map);
const renderer = L.canvas({ padding: 0.5 });

function fetchJson(path, onSuccess, onError) {
  return fetch(path).then(response => {
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    return response.json();
  }).then(onSuccess).catch(error => {
    (onError || (message => { status.textContent = message; }))(`Optional data unavailable: ${path} (${error.message})`);
  });
}

function coordinate(value) {
  if (typeof value === "string") {
    const [x, y, plane] = value.split("/").map(Number);
    return { x, y, plane };
  }
  return { x: Number(value.x), y: Number(value.y), plane: Number(value.plane ?? value.p ?? 0) };
}

function pointKey(point) { return `${point.x}/${point.y}/${point.plane}`; }

function normaliseRoute(value, name = "") {
  const path = (value.path || []).map(step => ({
    kind: step.kind || "walk", label: step.label || "", coordinate: coordinate(step.coordinate || step.tile || step)
  }));
  return {
    name: value.name || name, cost: value.cost ?? value.hierarchicalCost ?? value.rawCost, expandedNodes: value.expandedNodes,
    expandedTiles: (value.expandedTiles || []).map(coordinate), timings: value.timings,
    heuristicRegions: value.heuristicRegions || [],
    path, start: coordinate(value.start || value.source || path[0]?.coordinate),
    target: coordinate(value.target || path[path.length - 1]?.coordinate)
  };
}

function setRouteInputs(which, point) {
  if (!point || !Number.isFinite(point.x)) return;
  routeInputs[which].forEach((id, index) => { document.getElementById(id).value = [point.x, point.y, point.plane][index]; });
}

function readRouteInputs(which) {
  const values = routeInputs[which].map(id => Number(document.getElementById(id).value));
  if (values.some(value => !Number.isInteger(value))) throw new Error(`Enter integer ${which} coordinates`);
  return { x: values[0], y: values[1], plane: values[2] };
}

function fetchCsv(path, key, onSuccess) {
  fetch(path).then(response => {
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    return response.text();
  }).then(text => {
    loadState[key] = "ready";
    onSuccess(parseCsv(text));
    render();
  }).catch(error => {
    loadState[key] = "missing";
    status.textContent = `Optional data unavailable: ${path} (${error.message})`;
    render();
  });
}

function parseCsv(text) {
  const lines = text.trim().split(/\r?\n/).filter(Boolean);
  if (!lines.length) return [];
  const headers = lines.shift().split(",");
  return lines.map(line => {
    const values = line.split(",");
    return Object.fromEntries(headers.map((header, index) => [header, values[index] ?? ""]));
  });
}

const number = value => Number(value);
const field = (row, ...names) => names.map(name => row[name]).find(value => value !== undefined);
function tile(row) {
  return {
    component: number(field(row, "component")),
    x: number(field(row, "x", "tile_x")), y: number(field(row, "y", "tile_y")),
    plane: number(field(row, "plane")), region: field(row, "region") || "?",
    kind: field(row, "kind") || "leaf", level: field(row, "level") || ""
  };
}

fetchJson("../out/component-shapes.json", json => {
  data = json;
  fillComponentSelect();
  render();
}).catch(() => {});
fetchJson("../out/leak-route.json", json => { route = normaliseRoute(json, "leak route"); render(); }, message => { routeStatus.textContent = message; });
fetchJson("../out/hierarchy-test-routes.json", json => {
  fixtureRoutes = Array.isArray(json) ? json : (json.routes || []);
  routeCaseSelect.replaceChildren(new Option("Select a test case", ""), ...fixtureRoutes.map((item, index) =>
    new Option(item.name || `Case ${index + 1}`, String(index))));
}, message => { routeStatus.textContent = message; });
fetchCsv("../out/metis/partitions.csv", "metis", rows => { partitions = rows.map(tile); });
fetchCsv("../out/metis/kahip-partitions.csv", "kahip", rows => { kahipPartitions = rows.map(tile); });
fetchCsv("../out/metis/cut-edges.csv", "cuts", rows => {
  cutEdges = rows.map(row => ({
    component: number(row.component),
    a: [number(row.ax), number(row.ay), number(row.ap)],
    b: [number(row.bx), number(row.by), number(row.bp)]
  }));
});

componentSelect.addEventListener("change", render);
modeSelect.addEventListener("change", render);
planeSelect.addEventListener("change", () => {
  currentPlane = Number(planeSelect.value);
  map.eachLayer(layer => layer.redraw?.());
  render();
});
opacityInput.addEventListener("input", render);
fitButton.addEventListener("click", fitComponent);
routeCaseSelect.addEventListener("change", () => {
  const selected = fixtureRoutes[Number(routeCaseSelect.value)];
  if (!selected) return;
  route = normaliseRoute(selected, selected.name);
  setRouteInputs("start", route.start); setRouteInputs("end", route.target);
  routeStatus.textContent = `Fixture loaded: ${route.name}`;
  render();
});
document.getElementById("run-route").addEventListener("click", runRoute);
document.getElementById("fit-route").addEventListener("click", fitRoute);
map.on("click", event => {
  const point = { x: Math.round(event.latlng.lng), y: Math.round(event.latlng.lat), plane: currentPlane };
  const which = document.querySelector("input[name=pick-mode]:checked").value;
  setRouteInputs(which, point);
  routeStatus.textContent = `${which} set to ${pointKey(point)}`;
});

function fillComponentSelect() {
  componentSelect.replaceChildren(...data.components.map(component => {
    const option = document.createElement("option");
    option.value = component.id;
    option.textContent = `#${component.id} (${component.tiles.toLocaleString()} tiles)`;
    return option;
  }));
}

function selectedComponent() {
  return data.components.find(component => component.id === Number(componentSelect.value));
}

function inBounds(point, component) {
  return point.plane === Number(planeSelect.value) && point.x >= component.minX && point.x <= component.maxX &&
    point.y >= component.minY && point.y <= component.maxY;
}

function selectedTiles(rows, component) {
  return rows.filter(point => inBounds(point, component));
}

function removeLayers() {
  [shapeLayer, bboxLayer, routeLayer, expandedLayer, regionLayer, cutLayer, separatorLayer, ...routeMarkers].forEach(layer => {
    if (layer) map.removeLayer(layer);
  });
  routeMarkers = [];
}

function render() {
  if (!data || !selectedComponent()) return;
  removeLayers();
  const component = selectedComponent();
  const plane = Number(planeSelect.value);
  const mode = modeSelect.value;
  const fillOpacity = Number(opacityInput.value);
  const bins = data.bins.filter(bin => bin.component === component.id && bin.plane === plane);
  fitButton.textContent = mode === "heuristic" ? "Fit Heuristic" : "Fit Component";
  componentSelect.disabled = mode === "heuristic";

  if (mode !== "heuristic") {
    shapeLayer = L.layerGroup(bins.map(bin => {
      const density = bin.tiles / (data.binSize * data.binSize);
      return L.rectangle([[bin.y, bin.x], [bin.y + data.binSize, bin.x + data.binSize]], {
        renderer, stroke: false,
        fillColor: density > 0.75 ? "#ef4444" : density > 0.35 ? "#f59e0b" : "#2563eb",
        fillOpacity: mode === "manual" ? fillOpacity : Math.min(fillOpacity * 0.35, 0.2)
      });
    })).addTo(map);
    if (component.plane === plane) {
      bboxLayer = L.rectangle([[component.minY, component.minX], [component.maxY + 1, component.maxX + 1]], {
        color: "#111827", fill: false, interactive: false, weight: 2
      }).addTo(map);
    }
  }
  if (mode === "metis" || mode === "kahip") {
    const source = mode === "metis" ? partitions : kahipPartitions.filter(point => point.kind === "leaf");
    drawRegions(selectedTiles(source, component), fillOpacity);
  }
  const heuristicSummary = mode === "heuristic" ? drawHeuristic(fillOpacity) : null;
  if (mode === "difference") {
    drawCuts(component);
    drawSeparators(selectedTiles(kahipPartitions.filter(point => point.kind === "separator"), component));
  }
  drawExpandedTiles();
  drawRoute();
  renderLegend(mode);
  renderStats(component, bins.length, mode, heuristicSummary);
}

function drawHeuristic(fillOpacity) {
  heuristicBounds = null;
  if (!route?.heuristicRegions?.length) return null;
  const values = new Map(route.heuristicRegions.map(row => [`${row.component}:${row.region}`, Number(row.value)]));
  const points = kahipPartitions.filter(point => point.kind === "leaf" && point.plane === currentPlane);
  const partitionedComponents = new Set(points.map(point => point.component));
  const bins = new Map();
  const regions = new Set();
  for (const point of points) {
    const key = `${point.component}:${point.region}`;
    const value = values.get(key);
    if (!Number.isFinite(value)) continue;
    const x = Math.floor(point.x / data.binSize) * data.binSize;
    const y = Math.floor(point.y / data.binSize) * data.binSize;
    bins.set(`${key}:${x}:${y}`, { component: point.component, x, y, region: point.region, value });
    regions.add(key);
  }
  for (const bin of data.bins) {
    if (bin.plane !== currentPlane || partitionedComponents.has(bin.component)) continue;
    const region = `raw-${bin.component}`;
    const value = values.get(`${bin.component}:${region}`);
    if (!Number.isFinite(value)) continue;
    bins.set(`${bin.component}:${region}:${bin.x}:${bin.y}`, { component: bin.component, x: bin.x, y: bin.y, region, value });
    regions.add(`${bin.component}:${region}`);
  }
  const visibleValues = [...bins.values()].map(bin => bin.value);
  if (!visibleValues.length) return { bins: 0, regions: 0, minimum: 0, maximum: 0 };
  const minimum = Math.min(...visibleValues);
  const maximum = Math.max(1, ...visibleValues);
  const range = Math.max(1, maximum - minimum);
  regionLayer = L.layerGroup([...bins.values()].map(bin => {
    const hue = 120 * (1 - Math.min(1, (bin.value - minimum) / range));
    return L.rectangle([[bin.y, bin.x], [bin.y + data.binSize, bin.x + data.binSize]], {
      color: `hsl(${hue} 78% 40%)`, fillColor: `hsl(${hue} 78% 45%)`,
      fillOpacity: Math.min(fillOpacity, 0.68), weight: 1, opacity: 0.9
    }).bindTooltip(`#${bin.component}/${bin.region}: h=${bin.value}`);
  })).addTo(map);
  if (bins.size) {
    const all = [...bins.values()];
    heuristicBounds = [[Math.min(...all.map(bin => bin.y)), Math.min(...all.map(bin => bin.x))],
      [Math.max(...all.map(bin => bin.y)) + data.binSize, Math.max(...all.map(bin => bin.x)) + data.binSize]];
  }
  return { bins: bins.size, regions: regions.size, minimum, maximum };
}

function drawExpandedTiles() {
  if (!route?.expandedTiles?.length) return;
  expandedLayer = L.layerGroup(route.expandedTiles.filter(point => point.plane === currentPlane).map(point =>
    L.circleMarker([point.y + 0.5, point.x + 0.5], {
      renderer, stroke: false, fillColor: "#fde047", fillOpacity: 0.72,
      radius: 3, interactive: false
    })
  )).addTo(map);
}

function drawRoute() {
  if (!route) return;
  const walkSegments = [];
  let previous = route.start;
  let current = previous?.plane === currentPlane ? [[previous.y + 0.5, previous.x + 0.5]] : [];
  const flush = () => { if (current.length > 1) walkSegments.push(L.polyline(current, { color: "#dc2626", weight: 4, opacity: 0.95 })); current = []; };
  route.path.forEach(step => {
    const point = step.coordinate;
    if (step.kind === "transport") {
      flush();
      if (previous?.plane === currentPlane && point.plane === currentPlane) {
        routeMarkers.push(L.polyline([[previous.y + 0.5, previous.x + 0.5], [point.y + 0.5, point.x + 0.5]], { color: "#b91c1c", dashArray: "6 5", weight: 4 }).addTo(map));
      }
      if (point.plane === currentPlane) {
        routeMarkers.push(L.circleMarker([point.y + 0.5, point.x + 0.5], { className: "route-transport", color: "#b91c1c", fillColor: "#f97316", fillOpacity: 1, radius: 7, weight: 2 }).bindTooltip(step.label || "Transport").addTo(map));
        current = [[point.y + 0.5, point.x + 0.5]];
      }
      previous = point;
      return;
    }
    if (point.plane !== currentPlane) { flush(); previous = point; return; }
    if (previous?.plane !== currentPlane) current = [];
    current.push([point.y + 0.5, point.x + 0.5]);
    previous = point;
  });
  flush();
  routeLayer = L.layerGroup(walkSegments).addTo(map);
  ["start", "target"].forEach((name, index) => {
    const point = route[name];
    if (point?.plane === currentPlane) routeMarkers.push(L.circleMarker([point.y + 0.5, point.x + 0.5], { color: index ? "#111827" : "#fff", fillColor: index ? "#111827" : "#16a34a", fillOpacity: 1, radius: 8, weight: 3 }).bindTooltip(index ? "End" : "Start").addTo(map));
  });
  renderRouteStats();
}

function renderRouteStats() {
  if (!route) { routeStats.replaceChildren(); return; }
  const rows = [["route", route.name || "interactive"], ["cost", route.cost ?? "n/a"], ["expanded states", route.expandedNodes ?? "n/a"], ["expanded tiles", route.expandedTiles?.length ?? 0], ["steps", route.path.length]];
  const timings = route.timings;
  if (timings) {
    const milliseconds = value => `${Number(value).toFixed(1)} ms`;
    rows.push(
      ["source attach", milliseconds(timings.sourceAttachmentMs)],
      ["target attach", milliseconds(timings.targetAttachmentMs)],
      ["heuristic", milliseconds(timings.heuristicMs || 0)],
      ["abstract search", milliseconds(timings.abstractSearchMs)],
      ["reconstruction", milliseconds(timings.reconstructionMs)],
      ["total", milliseconds(timings.totalMs)]
    );
    if (timings.httpWorkerRoundTripMs !== undefined) rows.push(["HTTP round trip", milliseconds(timings.httpWorkerRoundTripMs)]);
    if (timings.search) rows.push(["metric edges", Number(timings.search.metricEdges).toLocaleString()]);
  }
  routeStats.replaceChildren(...rows.flatMap(([name, value]) => { const dt = document.createElement("dt"); const dd = document.createElement("dd"); dt.textContent = name; dd.textContent = value; return [dt, dd]; }));
}

async function runRoute() {
  try {
    const body = {
      start: readRouteInputs("start"), target: readRouteInputs("end"),
      allowTransports: document.getElementById("allow-transports").checked,
      includeExpandedTiles: document.getElementById("include-expanded").checked,
      useHeuristic: document.getElementById("route-algorithm").value === "astar"
    };
    routeStatus.textContent = "Requesting route...";
    const response = await fetch("/api/route", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) });
    const result = await response.json();
    if (!response.ok || !result.ok) throw new Error(result.error || `HTTP ${response.status}`);
    route = { ...normaliseRoute(result, body.useHeuristic ? "region A*" : "Dijkstra"), start: body.start, target: body.target };
    routeStatus.textContent = "Route loaded from Haskell.";
    render();
  } catch (error) { routeStatus.textContent = `Route error: ${error.message}`; }
}

function fitRoute() {
  if (!route) { routeStatus.textContent = "No route to fit."; return; }
  const endpoints = [route.start, route.target].filter(point => point?.plane === currentPlane);
  const points = endpoints.concat(route.path.map(step => step.coordinate).filter(point => point.plane === currentPlane)).map(point => [point.y, point.x]);
  if (points.length) map.fitBounds(points, { padding: [30, 30], maxZoom: 4 });
}

function regionColor(region) {
  let hash = 2166136261;
  for (const char of String(region)) hash = Math.imul(hash ^ char.charCodeAt(0), 16777619);
  return `hsl(${(hash >>> 0) % 360} 78% 45%)`;
}

function drawRegions(points, fillOpacity) {
  const bins = new Map();
  for (const point of points) {
    const x = Math.floor(point.x / 16) * 16;
    const y = Math.floor(point.y / 16) * 16;
    const key = `${point.region}:${x}:${y}`;
    const bin = bins.get(key) || { region: point.region, x, y };
    bins.set(key, bin);
  }
  regionLayer = L.layerGroup([...bins.values()].map(bin => L.rectangle(
    [[bin.y, bin.x], [bin.y + 16, bin.x + 16]], {
      color: regionColor(bin.region), fillColor: regionColor(bin.region),
      fillOpacity: Math.min(fillOpacity, 0.62), weight: 1, opacity: 0.9, interactive: false
    }
  ))).addTo(map);
}

function drawCuts(component) {
  const edges = cutEdges.filter(edge => edge.a[2] === currentPlane && edge.b[2] === currentPlane &&
    [edge.a, edge.b].every(point => point[0] >= component.minX && point[0] <= component.maxX &&
      point[1] >= component.minY && point[1] <= component.maxY));
  cutLayer = L.layerGroup(edges.map(edge => L.polyline(
    [[edge.a[1] + 0.5, edge.a[0] + 0.5], [edge.b[1] + 0.5, edge.b[0] + 0.5]],
    { color: "#00e5ff", weight: 4, opacity: 1, interactive: false }
  ))).addTo(map);
}

function drawSeparators(points) {
  separatorLayer = L.layerGroup(points.map(point => L.circleMarker(
    [point.y + 0.5, point.x + 0.5], {
      color: "#ff1493", fillColor: "#ff1493", fillOpacity: 1,
      radius: 7, weight: 2, opacity: 1, interactive: false
    }
  ))).addTo(map);
}

function renderLegend(mode) {
  const items = mode === "difference" ? [
    ["#00e5ff", "METIS cut edges", "legend-line"],
    ["#ff1493", "KaHIP separator tiles", ""]
  ] : mode === "heuristic" ? [["#16a34a", "Low heuristic", ""], ["#dc2626", "High heuristic", ""]] :
    mode === "metis" ? [["#38bdf8", "METIS leaf regions", ""]] :
    mode === "kahip" ? [["#fb923c", "KaHIP leaf regions", ""]] :
    [["#2563eb", "Manual walking components", ""]];
  if (route?.expandedTiles?.length) items.push(["#fde047", "Expanded abstract tiles", ""]);
  legend.replaceChildren(...items.map(([color, label, className]) => {
    const item = document.createElement("div"); item.className = "legend-item";
    const swatch = document.createElement("span"); swatch.className = `legend-swatch ${className}`;
    swatch.style.background = color; item.append(swatch, document.createTextNode(label)); return item;
  }));
}

function renderStats(component, visibleBins, mode, heuristicSummary) {
  const source = mode === "metis" ? selectedTiles(partitions, component) :
    mode === "kahip" ? selectedTiles(kahipPartitions.filter(point => point.kind === "leaf"), component) : [];
  const regions = new Set(source.map(point => point.region));
  const selectedCuts = cutEdges.filter(edge => [edge.a, edge.b].every(point =>
    point[2] === currentPlane && point[0] >= component.minX && point[0] <= component.maxX &&
    point[1] >= component.minY && point[1] <= component.maxY));
  const selectedSeparators = selectedTiles(kahipPartitions.filter(point => point.kind === "separator"), component);
  const rows = mode === "heuristic" ? [
    ["mode", "A* heuristic"], ["plane", currentPlane],
    ["heuristic regions", heuristicSummary?.regions.toLocaleString() || "0"],
    ["rendered bins", heuristicSummary?.bins.toLocaleString() || "0"]
  ] : [
    ["mode", { manual: "Manual", metis: "METIS", kahip: "KaHIP", heuristic: "A* heuristic", difference: "Difference" }[mode]],
    ["tiles", component.tiles.toLocaleString()], ["bbox", `${component.minX},${component.minY}..${component.maxX},${component.maxY}`],
    ["plane", component.plane], ["visible bins", visibleBins.toLocaleString()]
  ];
  if (mode === "manual") rows.push(["banks", component.banks.toLocaleString()], ["interesting", component.interestingTiles.toLocaleString()]);
  if (mode === "metis" || mode === "kahip") rows.push(["assigned tiles", source.length.toLocaleString()], ["leaf regions", regions.size.toLocaleString()]);
  if (mode === "heuristic") {
    if (heuristicSummary) rows.push(["heuristic range", `${heuristicSummary.minimum}..${heuristicSummary.maximum}`]);
  }
  if (mode === "difference") rows.push(["METIS cut edges", selectedCuts.length.toLocaleString()], ["KaHIP separators", selectedSeparators.length.toLocaleString()]);
  stats.replaceChildren(...rows.flatMap(([name, value]) => {
    const dt = document.createElement("dt"); const dd = document.createElement("dd");
    dt.textContent = name; dd.textContent = value; return [dt, dd];
  }));
  const missing = Object.entries(loadState).filter(([, state]) => state === "missing").map(([name]) => name);
  const loading = Object.entries(loadState).filter(([, state]) => state === "loading").map(([name]) => name);
  const modeMissing = mode === "metis" && loadState.metis === "missing" ? "METIS assignments are unavailable." :
    mode === "kahip" && loadState.kahip === "missing" ? "KaHIP assignments are unavailable." :
    mode === "difference" && (loadState.cuts === "missing" || loadState.kahip === "missing") ? "One or more difference inputs are unavailable." : "";
  status.textContent = [mode === "manual" ? "Manual data loaded." : "", modeMissing,
    missing.length ? `Missing optional files: ${missing.join(", ")}.` : "",
    loading.length ? `Loading optional data: ${loading.join(", ")}.` : ""].filter(Boolean).join(" ");
}

function fitComponent() {
  if (modeSelect.value === "heuristic" && heuristicBounds) {
    map.fitBounds(heuristicBounds, { padding: [30, 30], maxZoom: 2 });
    return;
  }
  const component = selectedComponent();
  map.fitBounds([[component.minY, component.minX], [component.maxY + 1, component.maxX + 1]], { padding: [30, 30], maxZoom: 2 });
}
