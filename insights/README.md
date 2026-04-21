# insights

Self-contained analytical notes built **only** from CSV files that any public user can obtain from the Innovation Strategy Explorer app's **Download CSV** buttons — no database access required.

## Current notes

- [`flowstoHICs.Rmd`](flowstoHICs.Rmd) — HIC-bound spillover per innovation, compared across LMIC-origin (excl. China), China-origin, and HIC-origin filings, by technology.

## One-time setup

The notes use headless Chrome (via the `chromote` R package) to fetch their underlying CSVs from the live app on first render. Install once:

```r
install.packages(c("rmarkdown", "chromote"))
```

You also need Google Chrome (or Chromium) installed on the machine doing the rendering. No browser extension, no Selenium, no Docker.

## Workflow

### 1. Render locally

```r
rmarkdown::render("insights/flowstoHICs.Rmd")
```

On first render, missing CSVs in `insights/data/` are fetched headlessly. Open the resulting `flowstoHICs.html` next to the `.Rmd` to review.

### 2. Bundle for deployment

To make the notes reachable on the deployed Shiny app:

```r
source("insights/render_all.R")   # renders every .Rmd in insights/ and
                                   # copies the HTML into inst/insights_html/
```

### 3. Deploy the Shiny app as usual (Posit Publisher)

Once deployed, each note is reachable at:

```
https://<your-app-url>/insights/<note>.html
```

For the example note:

```
https://mondpanther-iseapp2.share.connect.posit.cloud/insights/flowstoHICs.html
```

These URLs can be shared directly or hyperlinked from a blog post. The notes are **not** added to the app's navigation — they just piggy-back on the same Connect deployment domain.

## File layout

```
insights/
├── README.md
├── flowstoHICs.Rmd
├── render_all.R                 # batch renderer + deploy-bundler
├── R/
│   └── fetch_flow_csv.R         # chromote-based CSV downloader
└── data/                        # cached CSVs (git-ignored)

inst/
└── insights_html/               # rendered HTML served by the Shiny app
    └── flowstoHICs.html         # created by render_all.R
```

## How it works

The Shiny package registers a resource path on load:

```r
shiny::addResourcePath("insights", system.file("insights_html", package = "innovationStrategyExplorer"))
```

so anything under `inst/insights_html/` is served at `/insights/<file>` by Shiny, Posit Connect Cloud, or a local `runAppPackage()` session. See [`R/onLoadUnload.R`](../R/onLoadUnload.R).

To remove a note from the deployed app, delete its file from `inst/insights_html/` and redeploy.
