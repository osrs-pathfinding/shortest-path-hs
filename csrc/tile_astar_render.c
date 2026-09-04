#include <HsFFI.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

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
  HsInt width,
  HsInt height,
  HsInt seed_count,
  const HsInt *seed_offsets,
  const HsInt *seed_costs,
  HsInt *values
) {
  HsInt size = width * height;
  HsInt inf = spm_inf();

  for (HsInt i = 0; i < size; i++) values[i] = inf;
  for (HsInt i = 0; i < seed_count; i++) {
    HsInt offset = seed_offsets[i];
    HsInt cost = seed_costs[i];
    if (offset >= 0 && offset < size && cost < values[offset]) values[offset] = cost;
  }

  for (HsInt y = 0; y < height; y++) {
    for (HsInt x = 0; x < width; x++) {
      improve(values, width, height, x, y, x - 1, y - 1);
      improve(values, width, height, x, y, x, y - 1);
      improve(values, width, height, x, y, x + 1, y - 1);
      improve(values, width, height, x, y, x - 1, y);
    }
  }

  for (HsInt y = height - 1; y >= 0; y--) {
    for (HsInt x = width - 1; x >= 0; x--) {
      improve(values, width, height, x, y, x + 1, y);
      improve(values, width, height, x, y, x - 1, y + 1);
      improve(values, width, height, x, y, x, y + 1);
      improve(values, width, height, x, y, x + 1, y + 1);
    }
  }
}

static uint8_t scale_channel(HsInt value, HsInt min_value, HsInt max_value, HsInt low, HsInt high) {
  HsInt range = max_value > min_value ? max_value - min_value : 1;
  double linear = (double)(value - min_value) / (double)range;
  double logarithmic = log1p((double)(value - min_value)) / log1p((double)range);
  double fraction = (linear + logarithmic) * 0.5;
  HsInt t = (HsInt)(fraction * 255.0);
  if (t < 0) t = 0;
  if (t > 255) t = 255;
  return (uint8_t)(low + ((high - low) * t) / 255);
}

void spm_rgba_tile(
  HsInt point_count,
  const HsInt *xs,
  const HsInt *ys,
  const HsInt *values,
  HsInt tile_x,
  HsInt tile_y,
  HsInt min_value,
  HsInt max_value,
  HsInt bank_layer,
  uint8_t *rgba
) {
  memset(rgba, 0, 256 * 256 * 4);
  (void)bank_layer;
  for (HsInt i = 0; i < point_count; i++) {
    HsInt local_x = xs[i] - tile_x * 256;
    HsInt local_y = ys[i] - tile_y * 256;
    if (local_x < 0 || local_x >= 256 || local_y < 0 || local_y >= 256) continue;
    HsInt row = 255 - local_y;
    HsInt base = (row * 256 + local_x) * 4;
    HsInt value = values[i];
    rgba[base] = scale_channel(value, min_value, max_value, 40, 250);
    rgba[base + 1] = scale_channel(value, min_value, max_value, 220, 40);
    rgba[base + 2] = scale_channel(value, min_value, max_value, 110, 20);
    rgba[base + 3] = 255;
  }
}
