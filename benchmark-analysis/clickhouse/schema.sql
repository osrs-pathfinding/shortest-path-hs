CREATE DATABASE IF NOT EXISTS osrs_bench;

CREATE TABLE IF NOT EXISTS osrs_bench.runs
(
    run_id String,
    created_at DateTime64(3, 'UTC'),
    git_commit String,
    git_branch LowCardinality(String),
    git_dirty UInt8,
    corpus_id String,
    profile_set_id String,
    suite_id String,
    benchmark_tier LowCardinality(String),
    route_count UInt32,
    case_count UInt32,
    testbed LowCardinality(String),
    hostname LowCardinality(String),
    runner_version String,
    notes String,
    metadata Map(String, String)
)
ENGINE = MergeTree
ORDER BY (created_at, run_id);

CREATE TABLE IF NOT EXISTS osrs_bench.samples
(
    run_id String,
    profile LowCardinality(String),
    route_id String,
    route_label String,
    category LowCardinality(String),
    distance_tag LowCardinality(String),
    plane_tag LowCardinality(String),
    sample_index UInt16,
    expectation LowCardinality(String),
    reachable UInt8,
    cost Nullable(UInt64),
    oracle_reachable UInt8,
    oracle_cost Nullable(UInt64),
    status LowCardinality(String),
    correct UInt8,
    metrics Map(String, Float64),
    dimensions Map(String, String),
    raw_json String
)
ENGINE = MergeTree
ORDER BY (run_id, profile, route_id, sample_index);

ALTER TABLE osrs_bench.samples ADD COLUMN IF NOT EXISTS expectation LowCardinality(String) AFTER sample_index;
ALTER TABLE osrs_bench.samples ADD COLUMN IF NOT EXISTS reachable UInt8 AFTER expectation;
ALTER TABLE osrs_bench.samples ADD COLUMN IF NOT EXISTS cost Nullable(UInt64) AFTER reachable;
ALTER TABLE osrs_bench.samples ADD COLUMN IF NOT EXISTS oracle_reachable UInt8 AFTER cost;
ALTER TABLE osrs_bench.samples ADD COLUMN IF NOT EXISTS oracle_cost Nullable(UInt64) AFTER oracle_reachable;

CREATE OR REPLACE VIEW osrs_bench.case_metrics AS
SELECT
    run_id, profile, route_id, any(route_label) AS route_label,
    any(category) AS category, any(distance_tag) AS distance_tag,
    any(plane_tag) AS plane_tag, expectation, metric,
    quantileExact(0.5)(sample_value) AS value,
    avg(sample_value) AS mean, stddevPop(sample_value) AS stddev,
    min(sample_value) AS minimum, max(sample_value) AS maximum, count() AS repetitions
FROM
(
    SELECT run_id, profile, route_id, route_label, category, distance_tag, expectation,
           plane_tag, arrayJoin(mapKeys(metrics)) AS metric,
           metrics[metric] AS sample_value
    FROM osrs_bench.samples
)
GROUP BY run_id, profile, route_id, expectation, metric;
