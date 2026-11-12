# rSpacing 0.10.04 version status

- Structural base: verified complete rSpacing 0.08 archive.
- Imported selectively: run-spec source, tests, documentation, and example from experimental 0.09.
- New modules: irrigation, fertilisation, and cultivar validation.
- Public exports: 77.
- C# solution: not included and not modified.
- Validation completed here: archive integrity, DESCRIPTION/NAMESPACE consistency,
  source/test/documentation inventory, duplicate export check, and static syntax balance.
- Validation still required in an R-enabled environment: `devtools::document()`,
  `devtools::test()`, `devtools::check()`, and render of the canonical v0.08 Rmd.

## Scope boundary

This archive is a consolidation candidate, not yet the canonical release. It integrates the complete v0.08 API, validated run-spec objects, irrigation, fertilisation, cultivar validation, and the five-year workflow example. Morris package integration and the direct current Tool2 request adapter remain for the next verified increment after R checks.

## 0.10.01 correction

The reserved R keyword `repeat` was replaced by the valid argument name `repeat_rotation` in the rotation constructor and its documentation.

## 0.10.02 correction

`prepare_spacing_config_set()` now converts the validated base object with `as.list()` and explicitly preserves names before applying scenario overrides. This prevents required properties from disappearing during R Markdown execution.

## 0.10.04 correction

Run-spec JSON import now restores the embedded parameter catalogue before validation. Both row-oriented and column-oriented JSON layouts are covered, and a two-parameter round-trip test reproduces the documented workflow.

## 0.10.04 correction

`collect_spacing()` now handles zero-row tables natively and aligns heterogeneous CSV schemas before combining runs. The fix removes the need to filter empty outputs in calling reports.
