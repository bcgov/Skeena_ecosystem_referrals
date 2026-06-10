# Skeena_ecosystem_referrals


Automated spatial analysis tool for ecosystem referral support process. This tool reads in a referral Area of Interest (AOI) via a [BC Geomark](https://apps.gov.bc.ca/pub/geomark/) URL or local spatial file, compares it against configured reference layers, and generates a summary report of overlaps, intersections, and nearby features.

## Features

- **Polygon AOI analysis:**
  - Percent overlap with polygon reference layers (e.g., UWR, OGMA, parks)
  - Percent overlap of each raster value within the AOI
  - Linear feature density and type within the AOI (roads, streams)
  - Point count and type within the AOI (invasive plants, species at risk)
  - Proximity analysis: features within a configurable buffer distance
- **Line AOI analysis:**
  - Length of overlap with polygon/raster reference layers
  - Intersection with linear features
  - Proximity analysis for nearby points and polygons
- **Core Ecosystem (CE) layers:** AU-level overlap percentages and concerns
- **WHR Suitability layers:** Summary of habitat rating classes within AOI
- **Configurable reference layers** via `referral_layers.xlsx`
- **Self-contained HTML report** with maps and tables

## Prerequisites

- [R](https://cran.r-project.org/) (>= 4.1.0)
- [Quarto](https://quarto.org/) (>= 1.3)
- Required R packages (installed automatically on first run):
  - `sf`, `terra`, `dplyr`, `knitr`, `ggplot2`, `units`, `tidyr`, `jsonlite`, `readxl`

## Quick Start

1. **Clone this repository:**

   ```bash
   git clone <repository-url>
   cd bcgov-referral-process
   ```

2. **Add your spatial data files** to `data/input/` (see [Adding Reference Layers](#adding-or-updating-reference-layers)).

3. **Run the analysis** with a geomark URL:

   ```bash
   quarto render referral_analysis.qmd -P geomark_input:https://apps.gov.bc.ca/pub/geomark/geomarks/gm-abcdefghijklmnopqrstuv
   ```

   Or with a local spatial file:

   ```bash
   quarto render referral_analysis.qmd -P geomark_input:path/to/aoi.gpkg
   ```

4. **View the report:** Open the generated `referral_analysis.html` in a browser.

### Optional Parameters

| Parameter | Default | Description |
|---|---|---|
| `geomark_input` | *(required)* | Geomark URL or path to a local spatial file |
| `config_path` | `referral_layers.xlsx` | Path to the layer configuration workbook |

Example with parameters:

```bash
quarto render referral_analysis.qmd \
  -P geomark_input:https://apps.gov.bc.ca/pub/geomark/geomarks/gm-abc123 \
  -P config_path:referral_layers.xlsx
```

## Project Structure

```
bcgov-referral-process/
├── R/
│   ├── utils.R                  # Backwards-compatible utility loader
│   ├── utils_core.R             # Core setup, AOI loading, parsing helpers
│   ├── utils_layers.R           # Layer config, loading, and initialisation
│   ├── utils_analysis_polygon.R # Polygon AOI analysis functions
│   ├── utils_analysis_line.R    # Line AOI analysis functions
│   ├── utils_analysis_proximity.R
│   ├── utils_analysis_special.R # CE and WHR summaries
│   └── utils_report_helpers.R   # Reporting helpers
├── data/
│   ├── input/                   # Reference layer data files (not tracked)
│   └── output/                  # Generated reports (not tracked)
├── referral_analysis.qmd        # Main Quarto report document
├── referral_layers.xlsx         # Layer configuration (focus/key values/spatial rules)
├── .gitignore
└── README.md
```

## Adding or Updating Reference Layers

Reference layers are managed primarily through `referral_layers.xlsx`. The analysis reads these columns directly:

| Column | Description |
|---|---|
| `Focus` | Reporting group heading in the output |
| `Name` | Human-readable layer name |
| `Source Data Name` | Data source/catalogue reference |
| `Layer` | Layer identifier/name (used for local file matching) |
| `BCDC_ID` | Optional BCDC dataset/layer ID |
| `Geometry` | `Polygon`, `Line`, `Point`, or `Raster` |
| `Spatial_information_polygon` | Required reporting metric when AOI is polygon |
| `Spatial_information_linestring` | Required reporting metric when AOI is linestring |
| `Key_values` | Comma/semicolon-separated attribute fields to include |
| `Distance_outside_plot` | Optional per-layer outside distance for nearby-feature checks and reporting (e.g., `1 km`) |

### To add/update a layer:

1. Place the spatial data file in `data/input/`.
2. Add or update a row in `referral_layers.xlsx`.
3. Re-render the report.

### Supported data formats:

- **Vector:** GeoPackage (`.gpkg`), Shapefile (`.shp`), GeoJSON (`.geojson`)
- **Raster:** GeoTIFF (`.tif`)

## How It Works

### Analysis Flow

1. **Load AOI:** The geomark is fetched (from URL or file) and reprojected to BC Albers (EPSG:3005).
2. **Detect geometry type:** The AOI is classified as polygon or line.
3. **Load reference layers:** Layers from `referral_layers.xlsx` are resolved to local files in `data/input/` and reprojected.
  - During initialisation, missing BCDC/downloaded layers are cached using deterministic names derived from the workbook `Name`/`Layer` fields so future runs reuse local copies instead of re-downloading.
4. **Run analysis:** Based on the AOI type and layer geometry:

   | AOI Type | Layer Type | Analysis |
   |---|---|---|
   | Polygon | Polygon | Area and % overlap |
   | Polygon | Line | Length and density within AOI |
   | Polygon | Point | Count and attributes within AOI |
   | Polygon | Raster | % of AOI by raster value |
   | Line | Polygon | Length of line within polygon |
   | Line | Line | Intersection detection |
   | Line | Point | Proximity (within buffer) |
   | Line | Raster | Raster values along line |

5. **Outside-distance check:** For layers with `Distance_outside_plot` populated, nearby features are checked and reported using that layer-specific distance. This can vary between layers and can be changed in the workbook without editing the Quarto file.
6. **Generate report:** Results are compiled into an HTML report with tables and maps.

### Reporting behaviour

- Layers are reported by `Focus`.
- Layers with no direct intersection/overlap are ignored unless they have configured nearby hits from `Distance_outside_plot`.
- If `Key_values` is blank, spatial metrics only are reported.

### Specialized Analyses

- **Core Ecosystem (CE) layers** (prefixed `CE_`): Reports percent of each Analysis Unit (AU) overlapping the AOI, along with CE concerns (road density, ECA, etc.).
- **WHR Suitability layers** (named `WHR_Suitability`): Summarises the percentage of each Wildlife Habitat Rating class within the AOI.

## Customisation

### Using R directly (without Quarto)

If you prefer to run the analysis from an R script rather than Quarto:

```r
utils_files <- c(
  "R/utils_core.R",
  "R/utils_layers.R",
  "R/utils_analysis_polygon.R",
  "R/utils_analysis_line.R",
  "R/utils_analysis_proximity.R",
  "R/utils_analysis_special.R",
  "R/utils_report_helpers.R"
)
invisible(lapply(utils_files, source))
setup_packages()

# Ensure configured layers exist locally (downloads where possible)
init <- initialize_referral_layers("referral_layers.xlsx")
if (length(init$missing_layers) > 0) {
  message("Missing layers: ", paste(init$missing_layers, collapse = ", "))
}

# Optional: force refresh from remote sources (default is FALSE)
# init <- initialize_referral_layers("referral_layers.xlsx", overwrite_existing = TRUE)

# Optional: suppress progress logging
# init <- initialize_referral_layers("referral_layers.xlsx", verbose = FALSE)

# Load AOI
aoi <- load_geomark("https://apps.gov.bc.ca/pub/geomark/geomarks/gm-abc123")
aoi_type <- get_geometry_type(aoi)

# Load a specific layer from initialized config (cache-aware)
config <- init$config
layer <- load_layer(
  config$layer_path[[2]],
  geometry_type = config$geometry_type[[2]],
  bcdc_id = config$bcdc_id[[2]],
  layer_name = config$name[[2]],
  layer_identifier = config$layer_identifier[[2]],
  search_dir = "data/input"
)

# Run analysis
if (aoi_type == "polygon") {
  result <- polygon_overlap(aoi, layer, c("UWR_NUMBER", "SPECIES"))
}
```

### Modifying the Report Template

The Quarto document (`referral_analysis.qmd`) can be customised:

- **Output format:** Change `format: html` to `format: pdf` or `format: docx` in the YAML header.
- **Styling:** Modify the `theme` parameter in the YAML header.
- **Additional sections:** Add new code chunks for custom analyses.

## Troubleshooting

| Issue | Solution |
|---|---|
| "No geomark input provided" | Pass the `-P geomark_input:...` parameter when rendering |
| "Layer file not found" | Ensure a matching file exists in `data/input/` for the `Layer` or `Name` value in `referral_layers.xlsx` |
| Package installation fails | Run `install.packages(c("sf", "terra", "dplyr", "readr", "knitr", "ggplot2", "units", "tidyr", "jsonlite"))` manually in R |
| CRS mismatch warnings | All layers are automatically reprojected to BC Albers (EPSG:3005) |