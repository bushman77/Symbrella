# Official Canadian geographic evidence — source priority

This change upgrades the Core evidence resolver without weakening its
source-specific verification. It stays on the development branch.

## Primary sources and provenance

1. **Municipal population:** Statistics Canada Table 17-10-0155-01,
   "Population estimates, July 1, by census subdivision, 2021 boundaries".
   The WDS API method getFullTableDownloadCSV returns the official full-table
   archive. The population provider reads the *latest actual published annual
   reference year*, not a projected current-year number. As of October 2026,
   this is the July 1, 2025 municipal estimate series released January 14,
   2026. Newer municipal releases will become available automatically when
   the table updates.

   Official dataset:
   https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710015501

   WDS metadata/download API:
   https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/17100155/en

2. **Named Canadian place coordinates:** Natural Resources Canada, Canadian
   Geographical Names Database (CGNDB) GeoName API. Each place name must match
   exactly and uniquely, with an official-name filter and a provincial
   jurisdiction match. A successful result carries its CGNDB record URL.

   Official API documentation:
   https://natural-resources.canada.ca/maps-tools-publications/maps/geographical-names-canada/geoname-service-api

3. **Fallback:** Existing Wikidata P1082 dated population statements / P625
   coordinates. This fallback is explicitly marked as Wikidata. It may not be
   as current as the official source.

No numbers or place coordinates are manually seeded in runtime code.

## Implementation

- Core.Tools.WebRetrieval.StatisticsCanadaPopulation
  - Uses the official WDS full-table CSV archive
  - Local cache at XDG_CACHE_HOME/symbrella/statistics_canada_17100155.csv
    (or ~/.cache/symbrella/... if XDG_CACHE_HOME is unset)
  - Refresh checks every seven days; if offline, previously downloaded
    official data can be used and retains the original year
  - Matches municipality name AND province, refuses ambiguous or missing
    values and future reference periods
  - Emits measurement_type: :official_estimate and reference_year, plus
    source_table and source_url
  - The legacy census_year key remains temporarily for backwards-compatible
    response and comparison schemas but is NOT a census classification
- Core.Tools.WebRetrieval.CanadianGeoNames
  - Strict exact-name and jurisdiction checking
  - Haversine separation based on two verified representative point locations
  - Never interprets the result as a road route or street-address GPS fix
- Core.Tools.EvidenceResolver
  - For population: Statistics Canada, then Wikidata, then existing reference
    search within its three-attempt budget
  - For coordinates: Natural Resources Canada, then Wikidata within the
    bounded three-attempt budget
  - Existing injected coordinate providers still take precedence for tests
    or explicit overrides
- Core.Response.PopulationComparison, VerifiedPopulation and Grounding
  - Retain measurement type and source-series identity
  - Compare only same-period compatible sources; unlike a current estimate,
    a dated Wikidata statement may not be a census count
  - Authoritative rendering labels Statistics Canada values as official
    estimates, preserves citations, and rejects incompatible comparisons

The newest publication *of a dataset* is not necessarily the current
calendar year's population. A 2025 official estimate is labeled as 2025,
not presented as an observed 2026 population. BC Stats also publishes
municipal estimates through 2025, but is not used as an invented or
independent corroboration of the same series.

## Run tests

    cd ~/Symbrella
    git pull origin development
    mix test apps/core/test/core/tools/official_canadian_sources_test.exs
    mix test apps/core/test/core/tools/plural_population_grounding_test.exs
    mix test apps/core/test/core/response/population_comparison_test.exs
    mix test

## Live verification

Run a first official-provider lookup in the same Termux environment:

    mix run -e 'IO.inspect(Core.Tools.WebRetrieval.StatisticsCanadaPopulation.population("Kamloops BC"), limit: :infinity)'

And inspect the NRCan API lookup:

    mix run -e 'IO.inspect(Core.Tools.WebRetrieval.CanadianGeoNames.place("Kamloops BC"), limit: :infinity)'

Expected primary population evidence contains:
source_type: :statistics_canada, measurement_type: :official_estimate,
source_table: "17-10-0155-01", reference_year: 2025
(unless an official later municipal reference period has been published).

Run the three-turn Kamloops/Maple Ridge live test again and inspect
web_retrieval.support, evidence attempts, typed measurement provenance,
and authoritative_plan. If the official API fails or its exact entity
cannot be verified, inspect the documented Wikidata fallback rather than
silently claiming official verification.

The government APIs could not be exercised against the live runtime in the
development environment that created this change; verify the source response
format and output in Termux before considering production deployment.
