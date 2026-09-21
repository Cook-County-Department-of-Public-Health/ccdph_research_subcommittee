## =============================================================================
## drinking_water_dataprep.R
##
## Consolidated data-processing pipeline for the "Drinking Water and
## Environmental Justice" data brief (drinking_water.qmd). This script runs
## every data-acquisition/processing step documented in the qmd's Methodology
## section end-to-end (service area boundaries, SDWIS/ECHO violation and
## compliance data, and municipal Lead Service Line inventories), and writes
## a single, lightweight .RData file containing only the objects the
## *rendered document* actually needs.
##
## RATIONALE: drinking_water.qmd sets `eval: false` globally and shows the
## Methodology chunks for documentation purposes only -- they are not meant
## to be re-run (and re-download ~400+ MB of EPA data) every time the
## document is rendered. Instead, this script is run separately/on a
## refresh cadence, and the qmd's one `eval: true` setup chunk simply
## load()s its output (see "Load prepared analysis data" chunk near the top
## of drinking_water.qmd) to make `cook_county` and
## `cook_cws_with_violations` available to the Analysis section's
## interactive leaflet map.
##
## USAGE:
##   Run with working directory set to this file's own directory
##   (scripts/qmd/), matching the relative data/ paths used throughout
##   drinking_water.qmd:
##     Rscript drinking_water_dataprep.R
##   or, interactively:
##     setwd("scripts/qmd"); source("drinking_water_dataprep.R")
##
## OUTPUTS (all under scripts/qmd/data/, created if missing):
##   data/spatial/cook_county_water_system_boundaries.{gpkg,shp}
##   data/spatial/cook_county_water_systems_with_violations.shp
##   data/spatial/cook_county_lead_service_lines_combined.gpkg  (if any
##       municipal Lead Service Line endpoints are resolved -- see Section 3)
##   data/tabular/cook_county_pws_inventory.csv
##   data/tabular/cook_county_sdwa_violations.csv
##   data/tabular/cook_county_lead_copper_samples.csv
##   data/tabular/cook_county_pws_violation_summary.csv
##   data/tabular/cook_county_lsl_summary_by_municipality.csv  (ditto)
##   data/drinking_water_dataprep.RData  <-- loaded directly by the .qmd
## =============================================================================

## ---- 0. Setup ----------------------------------------------------------------

library(dplyr)      # data wrangling
library(tidyr)      # replace_na
library(stringr)    # str_starts, str_to_upper
library(readr)      # read_csv, write_csv
library(sf)         # spatial operations
library(httr2)      # ArcGIS REST Feature Service queries (see fetch_arcgis_features())
library(geojsonsf)  # parse ArcGIS REST GeoJSON responses into sf
library(purrr)      # pmap, compact, map
library(tigris)     # counties()
library(ggplot2)    # quick visual QA plot

## NOTE ON arcgislayers: this script deliberately does NOT use the
## arcgislayers package. arcgislayers requires rlang >= 1.3.0, and on
## systems where administrative policy pins rlang to an older version
## (e.g. 1.1.7), calling any arcgislayers function (even arc_open()) fails
## with "namespace 'rlang' ... is already loaded, but >= 1.3.0 is required"
## -- rlang cannot be reloaded at a different version mid-session once any
## other attached package (dplyr, tidyr, etc.) has already loaded it. All
## ArcGIS REST Feature Service access below instead goes through
## fetch_arcgis_features() (httr2 + geojsonsf), which has no rlang version
## requirement beyond what dplyr/tidyr already need.

## EPA's Public Water System Service Area Boundaries dataset includes some
## invalid polygons (self-intersections, duplicate vertices), especially
## among EPA-modeled boundaries. sf's default spherical geometry engine (s2)
## is strict about validity and will error on these -- fall back to the more
## forgiving GEOS planar engine for the duration of this script. Geometries
## are still explicitly repaired with st_make_valid() as a second safeguard.
sf::sf_use_s2(FALSE)

out_spatial_dir <- "data/spatial"
out_tabular_dir <- "data/tabular"
out_rdata_path  <- "data/drinking_water_dataprep.RData"

dir.create(out_spatial_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tabular_dir, showWarnings = FALSE, recursive = TRUE)


## =============================================================================
## SECTION 1. Public Water System Service Area Boundaries
## (mirrors qmd Methodology > "Public Water System Service Area Boundaries")
##
## Source: EPA Office of Water, Public Water System Service Area Boundaries v3
## https://www.epa.gov/ground-water-and-drinking-water/public-water-system-service-areas
## =============================================================================

feature_server_url <- "https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/Water_System_Boundaries/FeatureServer"

## ---- 1.1 Cook County, IL boundary used as a spatial filter -------------------
## Avoids downloading the entire national dataset (44,000+ systems).

cook_county <- counties(state = "IL", cb = TRUE, year = 2023) %>%
  filter(NAME == "Cook") %>%
  st_transform(4326)

## ---- 1.2 ArcGIS REST Feature Service helpers (httr2 + geojsonsf) ---------------
## No arcgislayers dependency -- see the rlang note in Section 0 above. These
## two helpers cover everything arcgislayers would otherwise provide for this
## script: inspecting a FeatureServer's layer list, and paginated feature
## retrieval with an optional bounding-box spatial filter.

# Lightweight replacement for arc_open() + print(): fetch and print the
# FeatureServer's layer listing so the correct layer index (there may be
# more than one, e.g. active vs. historical) can be confirmed.
print_arcgis_layers <- function(base_url) {
  service_info <- request(base_url) %>%
    req_url_query(f = "json") %>%
    req_perform() %>%
    resp_body_json()

  layers <- service_info$layers
  if (length(layers) == 0) {
    message("No layers reported for ", base_url)
    return(invisible(NULL))
  }

  message("Layers available on ", base_url, ":")
  for (lyr in layers) {
    message(sprintf("  [%s] %s", lyr$id, lyr$name))
  }
  invisible(service_info)
}

# Replacement for arc_select(): paginated ArcGIS REST Feature Service query,
# returned as an sf object. Esri REST services usually cap each response at
# ~1,000-2,000 records (maxRecordCount), so page through results with
# resultOffset until a short/empty page signals the last one.
fetch_arcgis_features <- function(base_url, layer_id = 0, where = "1=1",
                                   out_fields = "*", geometry_sf = NULL,
                                   batch_size = 1000) {
  query_url <- paste0(base_url, "/", layer_id, "/query")

  # Bounding-box prefilter, mirroring arc_select(filter_geom = ...): a cheap
  # server-side narrowing pass, refined with an exact st_filter() afterward.
  geom_param <- NULL
  if (!is.null(geometry_sf)) {
    bb <- st_bbox(st_transform(geometry_sf, 4326))
    geom_param <- sprintf(
      '{"xmin":%f,"ymin":%f,"xmax":%f,"ymax":%f,"spatialReference":{"wkid":4326}}',
      bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]
    )
  }

  all_features <- list()
  offset <- 0
  repeat {
    req <- request(query_url) %>%
      req_url_query(
        where = where,
        outFields = out_fields,
        f = "geojson",
        resultOffset = offset,
        resultRecordCount = batch_size,
        geometry = geom_param,
        geometryType = if (!is.null(geom_param)) "esriGeometryEnvelope" else NULL,
        inSR = if (!is.null(geom_param)) 4326 else NULL,
        spatialRel = if (!is.null(geom_param)) "esriSpatialRelIntersects" else NULL
      )

    resp <- req_perform(req)
    txt <- resp_body_string(resp)

    batch_sf <- geojson_sf(txt)
    if (nrow(batch_sf) == 0) break

    all_features[[length(all_features) + 1]] <- batch_sf
    if (nrow(batch_sf) < batch_size) break
    offset <- offset + batch_size
  }

  if (length(all_features) == 0) {
    return(all_features)  # empty list -- caller decides how to handle
  }

  dplyr::bind_rows(all_features) %>%
    st_as_sf() %>%
    st_make_valid()
}

## ---- 1.3 Pull Cook County community water system boundaries -------------------

# List the layers/sublayers on this FeatureServer to confirm the correct
# layer index (there may be more than one, e.g. active vs. historical).
print_arcgis_layers(feature_server_url)

# Layer 0 is the main community water system boundaries layer. Pull only
# features whose bounding box intersects the Cook County boundary.
cook_cws_raw <- fetch_arcgis_features(
  base_url    = feature_server_url,
  layer_id    = 0,
  geometry_sf = cook_county
)

if (length(cook_cws_raw) == 0 || nrow(cook_cws_raw) == 0) {
  stop("No community water system boundary features were returned for the ",
       "Cook County bounding box -- check feature_server_url and the ",
       "FeatureServer's availability before continuing.")
}

# Repair invalid geometries (self-intersections, duplicate vertices).
cook_cws_valid <- cook_cws_raw %>%
  st_make_valid()

n_invalid <- sum(!st_is_valid(cook_cws_valid))
if (n_invalid > 0) {
  message(n_invalid, " feature(s) remain invalid after st_make_valid(); ",
          "these will be dropped before clipping.")
  cook_cws_valid <- cook_cws_valid[st_is_valid(cook_cws_valid), ]
}

cook_cws <- cook_cws_valid %>%
  st_filter(cook_county, .predicate = st_intersects)  # precise clip to county polygon

## ---- 1.4 Save and quick visual QA ---------------------------------------------

glimpse(st_drop_geometry(cook_cws))

st_write(cook_cws, file.path(out_spatial_dir, "cook_county_water_system_boundaries.gpkg"),
          delete_dsn = TRUE)
st_write(cook_cws, file.path(out_spatial_dir, "cook_county_water_system_boundaries.shp"),
          delete_dsn = TRUE)

print(
  ggplot() +
    geom_sf(data = cook_county, fill = NA, color = "black", linewidth = 0.6) +
    geom_sf(data = cook_cws, aes(fill = PWS_Name), color = "white", linewidth = 0.1,
            show.legend = FALSE, alpha = 0.85) +
    theme_minimal() +
    labs(
      title = "Community Water System Service Areas, Cook County, IL",
      caption = "Source: U.S. EPA Public Water System Service Area Boundaries (v3)"
    )
)

## NOTES:
## - This layer covers COMMUNITY water systems only; non-community systems
##   (schools, gas stations, small transient supplies) live in a separate
##   FeatureServer.
## - The boundary data mixes EPA-modeled boundaries with authoritative
##   state/utility-sourced boundaries -- check the SOURCE/DATA_SRC field to
##   flag modeled estimates vs. verified boundaries.


## =============================================================================
## SECTION 2. Drinking Water Violation and Compliance (SDWIS/ECHO)
## (mirrors qmd Methodology > "Drinking Water Violation and Compliance")
##
## Source: EPA ECHO - Safe Drinking Water Act (SDWA) Data Downloads
## Summary/data dictionary: https://echo.epa.gov/tools/data-downloads/sdwa-download-summary
## Bulk download index:     https://echo.epa.gov/files/echodownloads/
## =============================================================================

zip_url  <- "https://echo.epa.gov/files/echodownloads/SDWA_latest_downloads.zip"
zip_path <- file.path(out_tabular_dir, "SDWA_latest_downloads.zip")

## ---- 2.1 Download the bulk SDWA dataset ---------------------------------------
## Large file (~400+ MB) containing every public water system nationally;
## EPA refreshes it quarterly. Distributed as a single bulk ZIP of CSVs.

if (!file.exists(zip_path)) {
  download.file(zip_url, destfile = zip_path, mode = "wb")
}

## ---- 2.2 Extract only the files needed -----------------------------------------

needed_files <- c(
  "SDWA_PUB_WATER_SYSTEMS.csv",
  "SDWA_VIOLATIONS_ENFORCEMENT.csv",
  "SDWA_GEOGRAPHIC_AREAS.csv",
  "SDWA_SERVICE_AREAS.csv",
  "SDWA_REF_CODE_VALUES.csv",
  "SDWA_LCR_SAMPLES.csv"          # Lead & Copper Rule sample data
)

zip_contents <- unzip(zip_path, list = TRUE)$Name

# Match case-insensitively in case EPA changes casing between quarterly releases.
files_to_extract <- zip_contents[
  toupper(zip_contents) %in% toupper(needed_files)
]

missing <- setdiff(toupper(needed_files), toupper(files_to_extract))
if (length(missing) > 0) {
  message("Note: the following expected files were not found in this quarter's ZIP: ",
          paste(missing, collapse = ", "),
          "\nCheck zip_contents (printed below) for the current file list/naming.")
  print(zip_contents)
}

unzip(zip_path, files = files_to_extract, exdir = out_tabular_dir, overwrite = TRUE)

## ---- 2.3 Read in and filter to Illinois -----------------------------------------
## PWSID's first two characters are the state/region code (e.g., "IL"), so
## every table can be filtered to Illinois systems before any join.

read_il <- function(filename) {
  path <- file.path(out_tabular_dir, filename)
  if (!file.exists(path)) return(NULL)
  read_csv(path, guess_max = 100000, show_col_types = FALSE) %>%
    filter(str_starts(PWSID, "IL"))
}

pws         <- read_il("SDWA_PUB_WATER_SYSTEMS.csv")
violations  <- read_il("SDWA_VIOLATIONS_ENFORCEMENT.csv")
geo_areas   <- read_il("SDWA_GEOGRAPHIC_AREAS.csv")
svc_areas   <- read_il("SDWA_SERVICE_AREAS.csv")
lcr_samples <- read_il("SDWA_LCR_SAMPLES.csv")

# Reference codes are not state-specific -- read in full (small file).
ref_codes <- read_csv(file.path(out_tabular_dir, "SDWA_REF_CODE_VALUES.csv"),
                       show_col_types = FALSE)

## ---- 2.4 Narrow to Cook County systems -------------------------------------------
## SDWA_GEOGRAPHIC_AREAS.csv's COUNTY_SERVED field is the most direct way to
## identify Cook County systems within the Illinois subset. This is
## self-reported by water systems/states and may be incomplete -- discrepancies
## against the boundary-based (spatial join) approach in Section 1 are
## retained and reported rather than reconciled.

cook_pwsids <- geo_areas %>%
  filter(str_to_upper(COUNTY_SERVED) == "COOK") %>%
  distinct(PWSID) %>%
  pull(PWSID)

message(length(cook_pwsids), " unique PWSIDs identified as serving Cook County ",
        "via the COUNTY_SERVED field.")

pws_cook        <- pws        %>% filter(PWSID %in% cook_pwsids)
violations_cook <- violations %>% filter(PWSID %in% cook_pwsids)
lcr_cook        <- lcr_samples %>% filter(PWSID %in% cook_pwsids)

## ---- 2.5 Decode key coded fields using the reference table -----------------------

decode <- function(df, code_col, value_type, new_col) {
  lookup <- ref_codes %>%
    filter(VALUE_TYPE == value_type) %>%
    select(VALUE_CODE, VALUE_DESCRIPTION) %>%
    rename(!!new_col := VALUE_DESCRIPTION)

  df %>%
    left_join(lookup, by = setNames("VALUE_CODE", code_col))
}

violations_cook <- violations_cook %>%
  decode("VIOLATION_CODE", "VIOLATION_CODE", "violation_description") %>%
  decode("CONTAMINANT_CODE", "CONTAMINANT_CODE", "contaminant_description")

## ---- 2.6 Summarize violations by system -------------------------------------------
## OWNER_TYPE_CODE: F = Federal, L = Local government, M = Public/Private,
##   N = Native American, P = Private, S = State.
## RULE_CODE == "350" isolates Lead and Copper Rule violations specifically.
## VIOLATION_STATUS "Unaddressed"/"Addressed" (vs. Resolved/Archived) are
##   treated here as "unresolved" -- i.e. not yet subject to a resolving
##   enforcement action, per EPA's compliance status categories.

violation_summary <- violations_cook %>%
  group_by(PWSID) %>%
  summarise(
    n_violations           = n(),
    n_health_based         = sum(IS_HEALTH_BASED_IND == "Y", na.rm = TRUE),
    n_unresolved           = sum(VIOLATION_STATUS %in% c("Unaddressed", "Addressed"), na.rm = TRUE),
    n_lead_copper_rule     = sum(RULE_CODE == "350", na.rm = TRUE),
    most_recent_violation  = max(VIOL_LAST_REPORTED_DATE, na.rm = TRUE),
    .groups = "drop"
  )

pws_cook_summary <- pws_cook %>%
  select(PWSID, PWS_NAME, OWNER_TYPE_CODE, POPULATION_SERVED_COUNT,
         PRIMARY_SOURCE_CODE, PWS_ACTIVITY_CODE, SERVICE_CONNECTIONS_COUNT) %>%
  left_join(violation_summary, by = "PWSID") %>%
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0)))

## ---- 2.7 Save tabular outputs -------------------------------------------------------

write_csv(pws_cook,         file.path(out_tabular_dir, "cook_county_pws_inventory.csv"))
write_csv(violations_cook,  file.path(out_tabular_dir, "cook_county_sdwa_violations.csv"))
write_csv(lcr_cook,         file.path(out_tabular_dir, "cook_county_lead_copper_samples.csv"))
write_csv(pws_cook_summary, file.path(out_tabular_dir, "cook_county_pws_violation_summary.csv"))

## ---- 2.8 Join violation summary onto the service area boundaries -------------------
## This is the key spatial + tabular join: it produces cook_cws_with_violations,
## the sf object the Analysis section's interactive leaflet map is built on.

cook_cws_with_violations <- cook_cws %>%
  left_join(pws_cook_summary, by = c("PWSID" = "PWSID"))

st_write(cook_cws_with_violations,
          file.path(out_spatial_dir, "cook_county_water_systems_with_violations.shp"),
          delete_dsn = TRUE)

## NOTES:
## - PWSID prefix filtering (str_starts(PWSID, "IL")) captures Illinois-primacy
##   systems; a small number of tribal systems may be coded under an EPA
##   region prefix instead -- check EPA_REGION if tribal systems are in scope.
## - This is quarterly-snapshot data (SUBMISSIONYEARQUARTER field); for a
##   violations *history* across time, EPA's cumulative-year bulk download or
##   the individual violations file's date fields would be needed instead.


## =============================================================================
## SECTION 3. Lead Service Lines (municipal inventory aggregation)
## (mirrors qmd Methodology > "Lead Service Lines")
##
## CONTEXT: Unlike SDWIS/ECHO compliance data or the EPA service area
## boundaries, there is no existing national/state-level aggregation of
## parcel/line-level lead service line inventories -- each municipality
## publishes its own, typically as an Esri "Lead Service Line Inventory"
## Experience Builder/Web AppBuilder app backed by a hosted FeatureServer.
## This section assumes those per-municipality FeatureServer URLs have been
## manually identified (see notes below) and aggregates them. Coverage is
## necessarily partial; this section is written to degrade gracefully (skip
## and warn, not fail the whole script) when no endpoints are yet resolved.
## =============================================================================

## ---- 3.1 Config table of municipal endpoints ---------------------------------------
## THIS IS THE PART THAT REQUIRES MANUAL COLLECTION (see qmd Methodology
## section for the browser/DevTools workflow used to resolve each URL).
## Populate as endpoints are found; leave feature_server_url as NA for
## municipalities not yet resolved.

municipal_endpoints <- tibble::tribble(
  ~municipality,       ~feature_server_url,                                    ~layer_id, ~notes,
  "Glenview",          NA_character_,                                          0L,        "Experience Builder app confirmed at experience.arcgis.com/experience/f110f2dabfdb46ceb3bd04055de8ee9b; underlying FeatureServer not yet resolved -- likely hosted via GIS Consortium (public.gisconsortium.org) shared backend",
  "Rolling Meadows",   NA_character_,                                          0L,        "Public map referenced at cityrm.org/935/Lead-Service-Line-Inventory; endpoint not yet resolved",
  "Algonquin",         NA_character_,                                          0L,        "Public map referenced at algonquin.org; endpoint not yet resolved",
  "Northbrook",        NA_character_,                                          0L,        "Village publishes inventory but map endpoint not yet resolved",
  "Elgin",             NA_character_,                                          0L,        "Village publishes an address-lookup map; endpoint not yet resolved",
  "Clarendon Hills",   NA_character_,                                          0L,        "Endpoint not yet resolved",
  "Park Ridge",        NA_character_,                                          0L,        "Endpoint not yet resolved",
  "North Riverside",   NA_character_,                                          0L,        "Endpoint not yet resolved",
  "Lincolnwood",       NA_character_,                                          NA_integer_, "Publishes inventory only as a static PDF, not an interactive map -- not compatible with this pipeline; would need separate PDF-table extraction"
)

## ---- 3.2 Standardized field mapping --------------------------------------------------
## Esri template DEFAULT field names are used as the target schema:
##   UtilityStatus / CustomerStatus / EntireServiceLineStatus
##   (values: Lead, Galvanized Requiring Replacement (GRR), Non-Lead, Unknown)

standard_fields <- c(
  "UtilityStatus", "CustomerStatus", "EntireServiceLineStatus",
  "LocationID", "Address"
)

## ---- 3.3 Pull function for a single municipality ---------------------------------------

pull_municipal_lsl <- function(municipality, url, layer_id) {
  if (is.na(url)) {
    message(municipality, ": no endpoint on file -- skipping.")
    return(NULL)
  }

  tryCatch({
    # fetch_arcgis_features() (Section 1.2) -- no spatial filter here, since
    # each municipal endpoint already covers just that municipality.
    data <- fetch_arcgis_features(base_url = url, layer_id = layer_id)
    if (length(data) == 0 || nrow(data) == 0) {
      message(municipality, ": pull returned no features -- skipping.")
      return(NULL)
    }
    data$municipality <- municipality
    data
  }, error = function(e) {
    message(municipality, ": pull failed -- ", conditionMessage(e))
    NULL
  })
}

## ---- 3.4 Pull all municipalities, harmonize schemas, and combine -----------------------

harmonize <- function(df) {
  present <- intersect(standard_fields, names(df))
  missing <- setdiff(standard_fields, names(df))
  if (length(missing) > 0) {
    message(unique(df$municipality), ": missing expected fields -- ",
            paste(missing, collapse = ", "))
  }
  df %>% select(municipality, all_of(present), geometry)
}

pulled <- municipal_endpoints %>%
  filter(!is.na(feature_server_url)) %>%
  pmap(function(municipality, feature_server_url, layer_id, notes) {
    pull_municipal_lsl(municipality, feature_server_url, layer_id)
  }) %>%
  compact()   # drop NULLs (failed/skipped pulls)

if (length(pulled) == 0) {
  ## No endpoints resolved yet -- this is expected/partial, not fatal. Skip
  ## the rest of this section rather than stopping the whole dataprep run.
  message("No municipal Lead Service Line endpoints were successfully pulled ",
          "(municipal_endpoints has no resolved feature_server_url values). ",
          "Skipping Lead Service Line aggregation -- populate municipal_endpoints ",
          "with resolved FeatureServer URLs to enable this section.")
  cook_lsl_combined <- NULL
} else {
  cook_lsl_combined <- map(pulled, harmonize) %>%
    bind_rows() %>%
    st_as_sf()

  summary_by_municipality <- cook_lsl_combined %>%
    st_drop_geometry() %>%
    count(municipality, EntireServiceLineStatus)

  print(summary_by_municipality)

  st_write(cook_lsl_combined,
           file.path(out_spatial_dir, "cook_county_lead_service_lines_combined.gpkg"),
           delete_dsn = TRUE)
  write_csv(summary_by_municipality,
            file.path(out_tabular_dir, "cook_county_lsl_summary_by_municipality.csv"))
}

## NOTES:
## - Report the count of municipalities with (a) no public inventory found,
##   (b) PDF-only inventory, (c) interactive map but endpoint unresolved, and
##   (d) successfully aggregated -- itself a meaningful empirical finding
##   about inventory transparency/fragmentation.
## - Field definitions differ by municipality even under Esri's shared
##   template if local staff customized domains/labels -- manually verify
##   value-level equivalence (e.g. "Unknown" vs. "Lead Status Unknown")
##   before treating combined counts as directly comparable.


## =============================================================================
## SECTION 4. Save essential elements for document rendering
##
## Only the objects drinking_water.qmd's evaluated (eval: true) chunks
## actually reference are persisted here, so the .RData stays small and the
## rendered document's data source is limited to what it needs:
##
##   cook_county              - sf polygon; Cook County boundary, used as the
##                               reference outline in the Analysis section's
##                               interactive leaflet map.
##   cook_cws_with_violations - sf polygons; community water system service
##                               areas joined to the system-level SDWA
##                               violation summary. This is the sole data
##                               source for the interactive map (fill,
##                               hover labels, and the derived violation
##                               density measure computed in the .qmd).
##
## All other intermediate/raw objects (pws_cook, violations_cook, lcr_cook,
## geo_areas, svc_areas, ref_codes, cook_cws, cook_lsl_combined, etc.) are
## available as the CSV/GeoPackage/Shapefile outputs written above for
## anyone who needs them, but are intentionally left out of the RData file
## the document loads at render time.
## =============================================================================

save(cook_county, cook_cws_with_violations, file = out_rdata_path, compress = "xz")

message("Saved essential rendering data (cook_county, cook_cws_with_violations) to ",
        out_rdata_path)
