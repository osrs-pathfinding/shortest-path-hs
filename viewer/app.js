const tileBaseUrl = "https://maps.runescape.wiki/osrs/versions/2026-03-04_a/tiles/rendered";
const mapId = -1;

const componentSelect = document.getElementById("component-select");
const planeSelect = document.getElementById("plane-select");
const opacityInput = document.getElementById("opacity");
const fitButton = document.getElementById("fit");
const stats = document.getElementById("stats");

let data = null;
let currentPlane = 0;
let shapeLayer = null;
let bboxLayer = null;

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
  crs: L.CRS.Simple,
  minZoom: -4,
  maxZoom: 4,
  zoomSnap: 1,
  center: [3200, 3200],
  zoom: 1,
  maxBounds: [[-1000, -1000], [13800, 13800]],
  maxBoundsViscosity: 0.5
});

const baseLayer = new WikiTileLayer("", {
  minZoom: -4,
  minNativeZoom: -2,
  maxNativeZoom: 3,
  maxZoom: 4,
  noWrap: true
}).addTo(map);

const renderer = L.canvas({ padding: 0.5 });

fetch("../out/component-shapes.json")
  .then(response => {
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    return response.json();
  })
  .then(json => {
    data = json;
    fillComponentSelect();
    render();
  })
  .catch(error => {
    stats.textContent = `Could not load ../out/component-shapes.json: ${error.message}`;
  });

componentSelect.addEventListener("change", render);
planeSelect.addEventListener("change", () => {
  currentPlane = Number(planeSelect.value);
  baseLayer.redraw();
  render();
});
opacityInput.addEventListener("input", render);
fitButton.addEventListener("click", fitComponent);

function fillComponentSelect() {
  componentSelect.replaceChildren(...data.components.map(component => {
    const option = document.createElement("option");
    option.value = component.id;
    option.textContent = `#${component.id} (${component.tiles.toLocaleString()} tiles)`;
    return option;
  }));
}

function selectedComponent() {
  const id = Number(componentSelect.value);
  return data.components.find(component => component.id === id);
}

function render() {
  if (!data) return;
  if (shapeLayer) map.removeLayer(shapeLayer);
  if (bboxLayer) map.removeLayer(bboxLayer);

  const component = selectedComponent();
  const plane = Number(planeSelect.value);
  const binSize = data.binSize;
  const fillOpacity = Number(opacityInput.value);
  const bins = data.bins.filter(bin => bin.component === component.id && bin.plane === plane);

  shapeLayer = L.layerGroup(bins.map(bin => {
    const density = bin.tiles / (binSize * binSize);
    return L.rectangle([[bin.y, bin.x], [bin.y + binSize, bin.x + binSize]], {
      renderer,
      stroke: false,
      fillColor: density > 0.75 ? "#ef4444" : density > 0.35 ? "#f59e0b" : "#2563eb",
      fillOpacity
    });
  })).addTo(map);

  if (component.plane === plane) {
    bboxLayer = L.rectangle([[component.minY, component.minX], [component.maxY + 1, component.maxX + 1]], {
      color: "#111827",
      fill: false,
      interactive: false,
      weight: 2
    }).addTo(map);
  }

  renderStats(component, bins.length);
}

function fitComponent() {
  const component = selectedComponent();
  map.fitBounds([[component.minY, component.minX], [component.maxY + 1, component.maxX + 1]], {
    padding: [30, 30],
    maxZoom: 2
  });
}

function renderStats(component, visibleBins) {
  const rows = [
    ["tiles", component.tiles.toLocaleString()],
    ["bbox", `${component.minX},${component.minY}..${component.maxX},${component.maxY}`],
    ["plane", component.plane],
    ["visible bins", visibleBins.toLocaleString()],
    ["banks", component.banks.toLocaleString()],
    ["origins", component.origins.toLocaleString()],
    ["destinations", component.destinations.toLocaleString()],
    ["global dests", component.globalDestinations.toLocaleString()],
    ["interesting", component.interestingTiles.toLocaleString()]
  ];

  stats.replaceChildren(...rows.flatMap(([name, value]) => {
    const dt = document.createElement("dt");
    const dd = document.createElement("dd");
    dt.textContent = name;
    dd.textContent = value;
    return [dt, dd];
  }));
}
