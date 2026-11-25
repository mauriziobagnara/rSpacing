# rSpacing 0.10.09 version status

- **Status:** release candidate, proposed as the next stable version after 0.10.05 (0.10.06 is reserved for the experimental branch).
- **Requires:** Spacing.Tool2 >= 0.09.10 for model runs; R >= 4.3.
- **Content since 0.10.05:** complete scenarios and soil profiles (0.10.07), management events (0.10.08), rotation options, the APSIM cultivar catalogue, English labels and full documentation (0.10.09). See `NEWS.md`.
- **Checks:** `R CMD check` with examples and tests: no errors; all tests pass, including three integration tests with real Tool2 0.09.10 runs.
- **Workflows shipped in `inst/rmarkdown/`:** operational manual rev10, cookbook v1.00, scenario comparison v1.00, five-year v1.08, Morris v1.18, input uncertainty v1.06.
- **Name note:** the report configuration constructor is `spacing_report_spec()`, because `spacing_report()` is the result accessor.
