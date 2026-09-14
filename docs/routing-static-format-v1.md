# Routing static artifact v1

## Purpose

`routing-static-v1.bin` is the versioned, immutable Haskell-to-Java boundary
for the prepared Tile A* world. It contains the static search tiles, static
sites, topology attachments, separator crossings, reachable banks, and sparse
same-component walking network. It does not contain account state or a query.

All multi-byte values are little-endian. There is no compression, alignment
padding, Haskell constructor tag, list encoding, or machine-sized integer.

## Header

The file starts with:

| Bytes | Field |
|---:|---|
| 8 | ASCII `RSPWORLD` |
| 4 | `u32` version, exactly `1` |
| 4 | `u32` flags, exactly `0` |

The decoder rejects a wrong magic, a version other than 1, non-zero flags, and
trailing bytes.

## Primitive values

`u32` and `i32` are fixed-width 32-bit values in little-endian byte order.
Counts, lengths, and CSR offsets are `u32`, but every value must be at most
`Integer.MAX_VALUE`; Java may therefore safely allocate/index the arrays with
signed `int` indices. Node IDs, component IDs, site IDs, and costs are `i32`.
Walking masks are raw `u8` values.

A packed tile uses the low 32 bits as follows:

```text
bits  0..14: x
bits 15..29: y
bits 30..31: plane
```

Packed tiles have unsigned 32-bit ordering. Plane 2 and plane 3 tiles can set
bit 31. Java must retain the raw bits in an `int` and must use
`Integer.compareUnsigned(a, b)` for binary search or sorting.

The north-node and south-node arrays use `i32` sentinel `-1` for “no
corresponding search node”. Other node references are indices into the search
arrays.

## Payload

Fields occur exactly in this order after the header.

### A. Forward-search base tiles

```text
u32 searchCount
u32 searchTiles[searchCount]
i32 searchComponents[searchCount]
u8  walkingMasks[searchCount]
i32 northNodes[searchCount]
i32 southNodes[searchCount]
```

The arrays are positionally aligned. `searchTiles` is strictly ascending in
unsigned packed-tile order. Every north/south value is `-1` or an index in
`[0, searchCount)`.

### B. Static spatial sites

```text
u32 siteCount
u32 siteTiles[siteCount]
```

`siteTiles` is strictly ascending in unsigned packed-tile order. A tile-to-site
map is intentionally omitted; binary search or a load-time map is equivalent.

### C. Site to routing-component attachments

```text
u32 siteComponentValueCount
u32 siteComponentOffsets[siteCount + 1]
i32 siteComponentIds[siteComponentValueCount]
```

Site `s` uses values in the half-open range
`[siteComponentOffsets[s], siteComponentOffsets[s + 1])`. Offsets start at
zero, are non-decreasing, and end at `siteComponentValueCount`.

### D. Routing-component to static-site attachments

```text
u32 routingComponentCount
u32 componentSiteValueCount
u32 componentSiteOffsets[routingComponentCount + 1]
i32 componentSiteIds[componentSiteValueCount]
```

Component `c` uses values in
`[componentSiteOffsets[c], componentSiteOffsets[c + 1])`. The component count
is the represented array capacity, normally `maxComponentId + 1`; empty groups
are retained. The ordering in this direction is preserved from Haskell and is
not required to match the ordering in section C. Logical membership in both
directions must agree.

### E. Structurally reachable banks

```text
u32 bankCount
u32 reachableBankTiles[bankCount]
```

The tiles are strictly ascending in unsigned packed-tile order.

### F. Separator crossings

```text
u32 crossingCount
i32 crossingFromSite[crossingCount]
i32 crossingToSite[crossingCount]
i32 crossingCosts[crossingCount]
```

Arrays are positionally aligned and preserve `topologySeparatorCrossings`
order. Both endpoints are site IDs, are distinct, and costs are non-negative.
Each crossing is one directed source record; the account-specific Java graph
should add both directed edges, as the Haskell graph does.

### G. Sparse same-component walking network

```text
u32 sparseOriginalCount
u32 sparseVertexCount
u32 sparseSteinerCount
u32 sparseUndirectedEdgeCount
u32 sparseAdjacencyCount
u32 sparseOffsets[sparseVertexCount + 1]
i32 sparseDestinations[sparseAdjacencyCount]
i32 sparseWeights[sparseAdjacencyCount]
```

The graph is CSR. Vertex `v` uses adjacency entries in
`[sparseOffsets[v], sparseOffsets[v + 1])`. Offsets start at zero, are
non-decreasing, and end at `sparseAdjacencyCount`. Destinations are in
`[0, sparseVertexCount)` and weights are non-negative. The current network is
undirected, so `sparseAdjacencyCount == 2 * sparseUndirectedEdgeCount`.
Also, `sparseOriginalCount == siteCount` and
`sparseSteinerCount == sparseVertexCount - sparseOriginalCount`.

## Runtime attachment semantics

The artifact intentionally omits natural components and collision data. The
Java plugin already owns raw walking-neighbor calculation. For an arbitrary
point, reproduce routing attachments as follows:

1. Unsigned-binary-search the packed point in `searchTiles`. If present,
   return the corresponding `searchComponents` entry.
2. Otherwise enumerate the plugin's authoritative raw walking neighbors,
   unsigned-binary-search each neighbor in `searchTiles`, and collect unique
   corresponding `searchComponents` IDs.

Return component IDs in ascending order if a stable order is desired. This is
equivalent to the current Haskell `routingPointAttachments` for static sites,
separator endpoints, reachable banks, and transport endpoints because
`searchTiles` contains exactly the structurally reachable routing tiles.

## Deliberately omitted facts

The artifact does not contain `staticSiteTileIndex`, natural components, full
routing-component owner tables, an independent structural-reachability set,
`World`, raw transports, account state, account-specific `SiteGraph`, reverse
search results, heuristic generators, or query search state. These are either
derivable, already owned by the Java plugin, or query/account-specific.

## Java reading sketch

```java
ByteBuffer buffer = ByteBuffer.wrap(bytes);
buffer.order(ByteOrder.LITTLE_ENDIAN);

byte[] magic = new byte[8];
buffer.get(magic);                 // must equal ASCII "RSPWORLD"
int version = buffer.getInt();     // must equal 1
int flags = buffer.getInt();       // must equal 0

long rawCount = Integer.toUnsignedLong(buffer.getInt());
if (rawCount > Integer.MAX_VALUE) throw new IOException("count overflow");

int packedTile = buffer.getInt();  // raw u32 bits
int cmp = Integer.compareUnsigned(a, b); // never signed-sort packed tiles
```

Read each count immediately before its array(s), allocate primitive Java
arrays, and reject malformed offsets/references before exposing the loaded
world to routing.
