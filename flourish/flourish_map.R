# =========================================================
# SPI -> Flourish Automated Map Update Pipeline
# =========================================================
#
# Chart 26427135 is a projection (choropleth) MAP
# (@flourish/projection-map). The `flourishcharts` R package
# has NO bind function for maps, so we drive the Flourish
# **Live API** directly.
#
# Strategy (replicate-with-override):
#   - Keep 26427135 in the app as the STYLED BASE
#     (geometry, colours, projection, bindings all live there).
#   - Pass `base_visualisation_id` to the Live API and override
#     ONLY the `regions` dataset with fresh SPI values.
#   - The base already binds columns `Economy` + `SPI.INDEX`,
#     so we simply resupply those same column names.
#
# NOTE: The Live API renders an EPHEMERAL embed. It does not
# rewrite the saved editor chart. This script produces a
# self-contained HTML file showing the up-to-date map.
# =========================================================

library(dplyr)
library(spiR)
library(jsonlite)


# =========================================================
# Configuration
# =========================================================

FLOURISH_VIS_ID <- "26427135"

API_KEY <- Sys.getenv("FLOURISH_API_KEY")
if (!nzchar(API_KEY)) {
  stop("FLOURISH_API_KEY is not set. Add it to your .Renviron.")
}

OUTPUT_HTML <- file.path(if (dir.exists("flourish")) "flourish" else ".",
                         "flourish_test.html")


# =========================================================
# 1. Get fresh SPI data, shaped for the map's `regions` dataset
# =========================================================
# Column names MUST match the base visualisation's bindings:
#   geometry (join key) -> "Economy"  (ISO3 code)
#   value               -> "SPI.INDEX"

spi_regions <- spi_index() %>%
  filter(date == 2024) %>%
  transmute(
    Economy   = iso3c,
    SPI.INDEX = SPI.INDEX
  ) %>%
  filter(!is.na(Economy))

head(spi_regions)


# =========================================================
# 2. Serialise the regions data to JSON (array-of-objects)
# =========================================================

regions_json <- toJSON(spi_regions, dataframe = "rows", auto_unbox = TRUE, na = "null")


# =========================================================
# 3. Build a self-contained HTML embed via the Flourish Live API
# =========================================================
# A projection map has FOUR datasets: regions, regions_geometry
# (the country shapes), points and lines. Overriding only
# `regions` would drop the geometry and render a blank map.
#
# So we fetch the published base config at runtime, swap ONLY
# `data.regions` with the fresh SPI values, and hand the complete
# object (geometry + styling + bindings + metadata) to the Live API.
#
# SECURITY: the enterprise API key is written into this HTML and
# is visible client-side. Keep the output file internal / do not
# publish it to an untrusted location.

html <- paste0(
  '<!DOCTYPE html>\n',
  '<html lang="en">\n',
  '<head>\n',
  '  <meta charset="utf-8">\n',
  '  <meta name="viewport" content="width=device-width, initial-scale=1">\n',
  '  <title>SPI Index 2024 - Map</title>\n',
  '  <style>html,body{margin:0;height:100%}#chart{width:100%;height:100vh}</style>\n',
  '</head>\n',
  '<body>\n',
  '  <div id="chart"></div>\n',
  '  <script src="https://cdn.flourish.rocks/flourish-live-v5.min.js"></script>\n',
  '  <script>\n',
  '    var spiRegions = ', regions_json, ';\n',
  '    var BASE_ID = ', toJSON(FLOURISH_VIS_ID, auto_unbox = TRUE), ';\n',
  '    var API_KEY = ', toJSON(API_KEY, auto_unbox = TRUE), ';\n',
  '    fetch("https://public.flourish.studio/visualisation/" + BASE_ID + "/visualisation-object.json")\n',
  '      .then(function (r) { return r.json(); })\n',
  '      .then(function (base) {\n',
  '        base.data.regions = spiRegions;   // inject fresh SPI values\n',
  '        // The Live API parser rejects null cells (typeof null === "object"),\n',
  '        // so replace any null/undefined with an empty string across all datasets.\n',
  '        Object.keys(base.data).forEach(function (ds) {\n',
  '          (base.data[ds] || []).forEach(function (row) {\n',
  '            Object.keys(row).forEach(function (k) {\n',
  '              if (row[k] === null || row[k] === undefined) { row[k] = ""; }\n',
  '            });\n',
  '          });\n',
  '        });\n',
  '        new Flourish.Live({\n',
  '          container: "#chart",\n',
  '          api_key: API_KEY,\n',
  '          template: base.template,\n',
  '          version: base.version,\n',
  '          state: base.state,\n',
  '          bindings: base.bindings,\n',
  '          data: base.data,\n',
  '          metadata: base.metadata\n',
  '        });\n',
  '      })\n',
  '      .catch(function (e) {\n',
  '        document.getElementById("chart").innerHTML =\n',
  '          "<pre>Failed to load base visualisation: " + e + "</pre>";\n',
  '      });\n',
  '  </script>\n',
  '</body>\n',
  '</html>\n'
)

writeLines(html, OUTPUT_HTML, useBytes = TRUE)


# =========================================================
# 4. Open the updated map in the browser
# =========================================================

utils::browseURL(normalizePath(OUTPUT_HTML))

message("Done. Updated map written to: ", normalizePath(OUTPUT_HTML))