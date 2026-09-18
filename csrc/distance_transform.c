#include <HsFFI.h>
#include <stdint.h>

static HsInt spm_inf(void) {
  return sizeof(HsInt) == 8 ? (HsInt)INT64_MAX : (HsInt)INT32_MAX;
}

static HsInt add1(HsInt value) {
  HsInt inf = spm_inf();
  return value == inf ? inf : value + 1;
}

static void improve(HsInt *values, HsInt width, HsInt height, HsInt x, HsInt y, HsInt nx, HsInt ny) {
  if (nx < 0 || nx >= width || ny < 0 || ny >= height) return;
  HsInt here = y * width + x;
  HsInt candidate = add1(values[ny * width + nx]);
  if (candidate < values[here]) values[here] = candidate;
}

void spm_chebyshev_transform(
  HsInt width, HsInt height, HsInt seed_count,
  const HsInt *seed_offsets, const HsInt *seed_costs, HsInt *values
) {
  HsInt size = width * height;
  HsInt inf = spm_inf();
  for (HsInt i = 0; i < size; i++) values[i] = inf;
  for (HsInt i = 0; i < seed_count; i++) {
    HsInt offset = seed_offsets[i];
    HsInt cost = seed_costs[i];
    if (offset >= 0 && offset < size && cost < values[offset]) values[offset] = cost;
  }
  for (HsInt y = 0; y < height; y++) for (HsInt x = 0; x < width; x++) {
    improve(values, width, height, x, y, x - 1, y - 1);
    improve(values, width, height, x, y, x, y - 1);
    improve(values, width, height, x, y, x + 1, y - 1);
    improve(values, width, height, x, y, x - 1, y);
  }
  for (HsInt y = height - 1; y >= 0; y--) for (HsInt x = width - 1; x >= 0; x--) {
    improve(values, width, height, x, y, x + 1, y);
    improve(values, width, height, x, y, x - 1, y + 1);
    improve(values, width, height, x, y, x, y + 1);
    improve(values, width, height, x, y, x + 1, y + 1);
  }
}
