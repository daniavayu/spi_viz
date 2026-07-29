# SPI Visualization Pipeline — Flourish + R

**Proposal:** Adopt a *Flourish-as-template + R-as-data* pipeline for SPI charts, instead of building and maintaining custom visualization functions in R and publishing their output to our website.

---

## The decision

We need recurring, publication-quality SPI visualizations (e.g. the choropheth SPI map) that refresh as new `spiR` data lands. Two ways to get there:

- **Option A — Build it all in R.** Write and maintain R plotting functions (ggplot/leaflet/plotly), export static images or HTML widgets, and publish those to our web page.
- **Option B — Flourish templates driven by R data (proposed).** Design the chart once in Flourish (styling, interactivity, branding), and use R only to feed it fresh data through Flourish's Live API.

---

## How the proposed pipeline works

```
1. Design & style the chart ONCE in Flourish  →  publish as a "base" visualization
2. R (spiR) pulls fresh SPI data
3. R injects that data into the base via Flourish's Live API (base_visualisation_id + data override)
4. Output is an up-to-date interactive chart, re-run on a schedule
```

- Flourish holds the **look** (colors, projection, legend, tooltips, title, geometry).
- R holds the **data** (`spi_index()` → the values to display).
- Each run re-reads the base styling, so any design change made in Flourish is inherited automatically — no code change required.

This is Flourish's officially documented "replicate a published visualization" pattern.

---

## Why this is better than building visuals in R

| Dimension | Option A: R visuals + publish | Option B: Flourish + R data (proposed) |
|---|---|---|
| **Interactivity** | Limited; hand-coded, heavy to maintain | Built-in (zoom, search, tooltips, animation) |
| **Design & branding** | Re-implemented in code per chart | Point-and-click in Flourish; reusable base |
| **Maintenance** | Ongoing R/JS/CSS upkeep per chart | Styling maintained in Flourish, not code |
| **Who can edit the look** | Only R developers | Any analyst via the Flourish editor |
| **Refresh with new data** | Re-run R export | Re-run R (data only) |
| **Consistency across charts** | Manual | One base → many charts inherit styling |
| **Time to add a new chart** | High (build from scratch) | Low (clone/adjust a Flourish template) |

**Net:** R does what R is best at (data), Flourish does what it is best at (design + interactivity). We stop maintaining visualization code.

---

## What to be aware of (honest limitations)

- **The API does not overwrite the saved Flourish chart.** It renders a *live* chart from template + fresh data. The deliverable that updates is the R-generated HTML embed (or a hosted copy of it), not the chart sitting in the Flourish editor. This is a documented Flourish behavior, not a limitation of our implementation.
- **Enterprise feature.** Flourish's Live API is an enterprise add-on and requires an API key. We already have one.
- **API key handling.** The key is embedded client-side in the output. For public hosting we should restrict/rotate the key and avoid committing it to shared/synced folders.
- **Automation for a public link.** Manual re-runs work today. For a always-fresh public URL, we host the generated HTML and schedule the R run (e.g., GitHub Actions + GitHub Pages).

---

## Status

- Working proof of concept: the SPI Index map (Flourish projection map) is being driven end-to-end by `spiR` data through R today.
- Effort to extend to additional SPI charts: low — clone a Flourish base and repoint the R data step.

## Recommended next steps

1. Confirm the Flourish enterprise/API entitlement and key-management policy.
2. Standardize one or two SPI "base" templates in Flourish (map + time series).
3. Decide the publishing model: internal review (manual runs) vs. public auto-refresh (hosted + scheduled).
4. Rotate the current API key before any hosting.
