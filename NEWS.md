# DDcentutils 0.1.0

## New features

This is the first release of this R experimental package that is currently under development by the Ecosystem Modeling and Data Consortium (EMDC) Team at Colorado State University (CSU).

We warmly welcome contributions from the community!
Whether it's bug reports, feature suggestions, documentation improvements, or code — all input is valued.

We created a Discussions forum on GitHub to facilitate Q&A about the package and suggest ideas for further development: <https://github.com/CSU-Soil-Carbon-Solution-Center/DDcentutils/discussions>.

You can also report bugs by creating an issue on the repository.
For more about creating an issue, please visit: <https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/creating-an-issue>.

This package aims to facilitate the use of the DayCent® model and the visualization of DayCent model run results.
This package also provides functions to facilitate input file building and management to be used in a DayCent model run.

### Schedule file builder

Added a full tidy-table schedule engine, replacing the earlier incomplete `schFileBuilder.r` draft:

- `read_sch()` / `write_sch()`: parse a DayCent `.sch` file into `site_table` / `block_table` / `event_table` data frames and render them back, preserving source line numbers for validator findings.
- `build_sch()`: build a schedule from tidy tables, filling documented site-header defaults and deriving a single block when none is supplied.
- `validate_sch()`: structural and semantic checks (unknown events, invalid day-of-year, unpaired crop/tree events, missing library references), ported from the EMDC platform's preflight input-QC rules, returning a data frame of findings with `severity_max()` and `format_finding()` helpers.
- `read_library100()` / `read_library_dir()`: resolve `.100` library block IDs referenced by schedule events.
- `read_sch_tables()` / `write_sch_tables()`: round-trip the three tables to and from CSV.
- `sch_cli()` (`inst/scripts/ddcent-sch.R`): shell access to `build` / `validate` / `inspect`.

See the "Building schedule files" section of the README for the table → `.sch` → validate loop.

## Bug fixes

None yet.

## Breaking changes

None yet.

## Known issues/Limitations

This package enforces a specific directory structure. See the README for required conventions before use.
