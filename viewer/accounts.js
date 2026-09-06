const profileRoot = document.getElementById("profile");
const tabsRoot = document.getElementById("profile-tabs");
const status = document.getElementById("status");
const itemNames = new Map();
let profiles = [];
let selectedView = 0;

const label = value => String(value).replace(/([a-z])([A-Z])/g, "$1 $2").replace(/^./, letter => letter.toUpperCase());
const itemIcon = id => `https://static.runelite.net/cache/item/icon/${id}.png`;

function element(tag, classes, text) {
  const node = document.createElement(tag);
  if (classes) node.className = classes;
  if (text !== undefined) node.textContent = text;
  return node;
}

function entries(counts) {
  return Object.entries(counts).sort(([left], [right]) => left.localeCompare(right, undefined, { numeric: true }));
}

function itemTile([id, count]) {
  const tile = element("div", "relative grid min-h-20 place-items-center rounded border border-stone-300 bg-stone-50 px-1 pb-1 pt-2 text-center");
  const numericId = /^\d+$/.test(id);
  if (numericId) {
    const image = element("img", "h-8 w-9 object-contain [image-rendering:pixelated]");
    image.src = itemIcon(id);
    image.alt = "";
    image.width = 36;
    image.height = 32;
    tile.append(image);
  }
  const name = numericId ? (itemNames.get(Number(id)) || `Item ${id}`) : label(id.toLowerCase().replaceAll("_", " "));
  tile.append(element("span", "w-full break-words text-[11px] leading-3 text-stone-700", name));
  if (count !== 1) tile.append(element("span", "absolute right-1 top-0.5 text-[10px] font-bold text-amber-700", Number(count).toLocaleString()));
  tile.title = `ID ${id}, quantity ${Number(count).toLocaleString()}`;
  return tile;
}

function itemGrid(counts, slots = 0) {
  const grid = element("div", "grid grid-cols-4 gap-1.5");
  const items = entries(counts);
  items.forEach(item => grid.append(itemTile(item)));
  for (let index = items.length; index < slots; index++) grid.append(element("div", "min-h-20 rounded border border-dashed border-stone-200 bg-stone-50/50"));
  return grid;
}

function section(title, body) {
  const node = element("section", "rounded-md border border-stone-300 bg-white p-4 shadow-sm");
  node.append(element("h2", "mb-3 text-sm font-semibold", title), body);
  return node;
}

function facts(values) {
  const list = element("dl", "grid grid-cols-[minmax(0,1fr)_auto] gap-x-4 gap-y-2 text-sm");
  values.forEach(([name, value]) => list.append(
    element("dt", "text-stone-600", name),
    element("dd", "m-0 text-right font-medium", value)
  ));
  return list;
}

function profileSummary(profile) {
  return facts([
    ["Completed quests", profile.questCount.toLocaleString()],
    ["Fairy rings", profile.fairyRings ? "Unlocked" : "Locked"],
    ["Quetzal platforms", profile.quetzalPlatforms.length.toLocaleString()],
    ["Bank item types", Object.keys(profile.bank).length.toLocaleString()],
    ["Spellbook", profile.runtime.spellbook]
  ]);
}

function skillLevels(profile) {
  const grid = element("div", "grid grid-cols-2 gap-x-4 gap-y-1.5 sm:grid-cols-3 lg:grid-cols-4");
  Object.entries(profile.levels).sort(([left], [right]) => left.localeCompare(right)).forEach(([name, level]) => {
    const row = element("div", "flex justify-between gap-2 border-b border-stone-100 py-1 text-xs");
    row.append(element("span", "text-stone-600", name), element("strong", "font-semibold", level));
    grid.append(row);
  });
  return grid;
}

function pohDetails(profile) {
  const poh = profile.poh;
  const unlocks = ["fairyRing", "spiritTree", "obelisk", "mountedGlory", "mountedXerics", "mountedDigsite", "mountedMythical"]
    .filter(key => poh[key]).map(label);
  const portals = poh.portals.includes("*") ? "All portals" : (poh.portals.join(", ") || "None");
  return facts([
    ["Location", poh.location],
    ["Jewellery box", label(poh.jewelleryBox.replace("JewelleryBox", ""))],
    ["Portals", portals],
    ["Fixtures", unlocks.join(", ") || "None"]
  ]);
}

function diaryDetails(profile) {
  const grid = element("div", "grid gap-x-5 gap-y-1.5 sm:grid-cols-2");
  Object.entries(profile.diaries).forEach(([name, tier]) => {
    const row = element("div", "flex justify-between gap-3 border-b border-stone-100 py-1 text-xs");
    row.append(element("span", "text-stone-600", name), element("strong", "font-semibold", tier));
    grid.append(row);
  });
  return grid;
}

function bankDetails(profile) {
  const details = element("details", "rounded-md border border-stone-300 bg-white shadow-sm");
  details.append(element("summary", "cursor-pointer select-none px-4 py-3 text-sm font-semibold", `Bank (${Object.keys(profile.bank).length} item types)`));
  const body = element("div", "border-t border-stone-200 p-4");
  body.append(itemGrid(profile.bank));
  details.append(body);
  return details;
}

function renderProfile(index) {
  selectedView = index;
  const profile = profiles[index];
  tabsRoot.querySelectorAll("button").forEach((tab, tabIndex) => {
    const active = tabIndex === index;
    tab.setAttribute("aria-selected", String(active));
    tab.className = `border border-stone-300 px-4 py-2 text-sm font-semibold first:rounded-l-md last:rounded-r-md ${active ? "relative z-10 bg-stone-800 text-white" : "-ml-px bg-white text-stone-700 hover:bg-stone-50"}`;
  });
  profileRoot.replaceChildren();
  const heading = element("div", "mb-5");
  heading.append(element("h2", "text-2xl font-semibold", `${label(profile.name)} account`));
  heading.append(element("p", "mt-1 text-sm text-stone-600", "Compiled benchmark profile used by route correctness and performance runs."));
  const carried = element("div", "grid gap-4 lg:grid-cols-2");
  carried.append(section("Inventory", itemGrid(profile.inventory, 28)), section("Rune pouch", itemGrid(profile.runePouch, 4)));
  if (Object.keys(profile.equipment).length) carried.append(section("Equipment", itemGrid(profile.equipment)));
  const progression = element("div", "mt-4 grid gap-4 lg:grid-cols-2");
  progression.append(section("Account", profileSummary(profile)), section("Player-owned house", pohDetails(profile)), section("Levels", skillLevels(profile)), section("Achievement diaries", diaryDetails(profile)));
  profileRoot.replaceChildren(heading, carried, progression, bankDetails(profile));
}

function renderProgression() {
  selectedView = "progression";
  tabsRoot.querySelectorAll("button").forEach((tab, index) => {
    const active = index === profiles.length;
    tab.setAttribute("aria-selected", String(active));
    tab.className = `border border-stone-300 px-4 py-2 text-sm font-semibold first:rounded-l-md last:rounded-r-md ${active ? "relative z-10 bg-stone-800 text-white" : "-ml-px bg-white text-stone-700 hover:bg-stone-50"}`;
  });
  const heading = element("div", "mb-5");
  heading.append(element("h2", "text-2xl font-semibold", "Bank progression"));
  heading.append(element("p", "mt-1 text-sm text-stone-600", "Unified view of every bank item available to each benchmark profile."));

  const wrapper = element("div", "overflow-x-auto rounded-md border border-stone-300 bg-white shadow-sm");
  const table = element("table", "w-full min-w-3xl border-collapse text-sm");
  const head = element("thead", "sticky top-0 bg-stone-100 text-left text-xs uppercase text-stone-600");
  const headRow = element("tr");
  headRow.append(element("th", "px-3 py-2 font-semibold", "Item"));
  profiles.forEach(profile => headRow.append(element("th", "w-24 px-3 py-2 text-center font-semibold", label(profile.name))));
  head.append(headRow);
  const body = element("tbody", "divide-y divide-stone-200");
  const ids = [...new Set(profiles.flatMap(profile => Object.keys(profile.bank)))]
    .sort((left, right) => left.localeCompare(right, undefined, { numeric: true }));
  ids.forEach(id => {
    const row = element("tr", "hover:bg-stone-50");
    const item = element("td", "px-3 py-2");
    const itemContent = element("div", "flex min-w-56 items-center gap-3");
    if (/^\d+$/.test(id)) {
      const image = element("img", "h-8 w-9 shrink-0 object-contain [image-rendering:pixelated]");
      image.src = itemIcon(id);
      image.alt = "";
      image.width = 36;
      image.height = 32;
      itemContent.append(image);
    }
    const text = element("div", "min-w-0");
    text.append(element("div", "font-medium", /^\d+$/.test(id) ? (itemNames.get(Number(id)) || `Item ${id}`) : label(id.toLowerCase().replaceAll("_", " "))));
    text.append(element("div", "font-mono text-[11px] text-stone-500", id));
    itemContent.append(text);
    item.append(itemContent);
    row.append(item);
    profiles.forEach(profile => {
      const available = Object.hasOwn(profile.bank, id);
      row.append(element("td", `px-3 py-2 text-center text-xs font-semibold ${available ? "text-emerald-700" : "text-stone-300"}`, available ? "Yes" : "-"));
    });
    body.append(row);
  });
  table.append(head, body);
  wrapper.append(table);
  profileRoot.replaceChildren(heading, wrapper);
}

fetch("/api/profiles").then(response => {
  if (!response.ok) throw new Error(`HTTP ${response.status}`);
  return response.json();
}).then(data => {
  profiles = data.profiles;
  profiles.forEach((profile, index) => {
    const tab = element("button", "", label(profile.name));
    tab.type = "button";
    tab.setAttribute("role", "tab");
    tab.addEventListener("click", () => renderProfile(index));
    tabsRoot.append(tab);
  });
  const progressionTab = element("button", "", "Progression");
  progressionTab.type = "button";
  progressionTab.setAttribute("role", "tab");
  progressionTab.addEventListener("click", renderProgression);
  tabsRoot.append(progressionTab);
  status.classList.add("hidden");
  profileRoot.classList.remove("hidden");
  renderProfile(0);
}).catch(error => {
  status.textContent = `Could not load account builds: ${error.message}`;
  status.className = "text-sm text-red-700";
});

fetch("https://prices.runescape.wiki/api/v1/osrs/mapping").then(response => response.ok ? response.json() : []).then(mapping => {
  mapping.forEach(item => itemNames.set(item.id, item.name));
  if (profiles.length) selectedView === "progression" ? renderProgression() : renderProfile(selectedView);
}).catch(() => {});
