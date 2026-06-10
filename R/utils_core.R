# =============================================================================
# Core Utility Functions
# =============================================================================

# BC Albers projection (EPSG code used throughout for consistency)
BC_ALBERS_EPSG <- 3005

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

attach_single_field_tables <- function(grouped_result, raw_result, group_fields,
                                       summary_fun) {
  if (length(group_fields) <= 1) {
    return(grouped_result)
  }

  max_split_rows <- getOption("referral.max_split_table_rows", Inf)
  if (is.data.frame(raw_result) && nrow(raw_result) > max_split_rows) {
    return(grouped_result)
  }

  split_tables <- stats::setNames(
    lapply(group_fields, function(field) summary_fun(raw_result, field)),
    group_fields
  )
  attr(grouped_result, "split_tables") <- split_tables
  grouped_result
}

#' Install and load required packages
#'
#' Checks for required packages and installs any that are missing, then loads
#' them into the session.
setup_packages <- function() {
  required_packages <- c(
    "sf", "terra", "dplyr", "knitr", "ggplot2",
    "units", "tidyr", "jsonlite", "readxl", "DBI", "RSQLite",
    "leaflet", "htmltools", "raster"
  )
  missing <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  invisible(lapply(required_packages, \(x) suppressPackageStartupMessages(library(x, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))))
}

# -----------------------------------------------------------------------------
# Geomark / AOI Loading
# -----------------------------------------------------------------------------

extract_geomark_id <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  value <- trimws(as.character(x[[1]]))
  if (is.na(value)) {
    return(NA_character_)
  }
  if (value == "") {
    return(NA_character_)
  }

  id_match <- regmatches(value, regexpr("gm-[A-Za-z0-9]+", value, perl = TRUE))
  if (length(id_match) == 1 && nzchar(id_match)) {
    return(id_match)
  }

  if (grepl("^[A-Za-z0-9]+$", value) && !grepl("^https?://", value)) {
    return(paste0("gm-", value))
  }

  NA_character_
}

build_geomark_feature_url <- function(geomark_input) {
  id <- extract_geomark_id(geomark_input)
  if (is.na(id)) {
    return(NA_character_)
  }
  paste0("https://apps.gov.bc.ca/pub/geomark/geomarks/", id, "/feature.geojson")
}

is_likely_html_payload <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  raw_head <- readBin(con, what = "raw", n = 4096)
  if (length(raw_head) == 0) {
    return(TRUE)
  }
  head_txt <- tolower(rawToChar(raw_head))
  grepl("<!doctype html|<html|<head|<body", head_txt)
}

validate_geojson_payload <- function(path, source_url) {
  if (!file.exists(path) || file.info(path)$size <= 0) {
    stop("Downloaded Geomark payload is empty: ", source_url)
  }

  if (is_likely_html_payload(path)) {
    stop(
      "Geomark download returned HTML instead of geodata: ", source_url,
      "\nUse a Geomark URL/ID that can be resolved to the REST feature endpoint."
    )
  }

  txt <- readChar(path, nchars = min(file.info(path)$size, 20000L), useBytes = TRUE)
  if (!jsonlite::validate(txt)) {
    stop("Geomark payload is not valid JSON/GeoJSON: ", source_url)
  }

  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  geojson_type <- parsed$type %||% ""
  if (!geojson_type %in% c("Feature", "FeatureCollection")) {
    stop("Geomark payload is JSON but not GeoJSON Feature/FeatureCollection: ", source_url)
  }

  invisible(TRUE)
}

#' Load a geomark AOI from a URL or file path
#'
#' Supports BC geomark URLs (GeoJSON endpoint), local GeoPackage, Shapefile,
#' or GeoJSON files.
#'
#' @param geomark_input Character string: a geomark URL
#'   (e.g., "https://apps.gov.bc.ca/pub/geomark/geomarks/gm-...") or a local
#'   file path to a spatial file.
#' @return An sf object reprojected to BC Albers (EPSG:3005).
load_geomark <- function(geomark_input) {
  input <- trimws(as.character(geomark_input %||% ""))
  if (input == "") {
    stop("AOI input is empty. Provide a Geomark URL/ID or a local spatial file path.")
  }

  if (file.exists(input)) {
    aoi <- sf::st_read(input, quiet = TRUE)
  } else {
    feature_url <- if (grepl("^https?://", input)) {
      direct_geojson <- grepl("\\.geojson($|\\?)", input, ignore.case = TRUE)
      is_geomark_page <- grepl("/geomarks/gm-[A-Za-z0-9]+(/)?$", sub("\\?.*$", "", input), ignore.case = TRUE)

      if (direct_geojson && !is_geomark_page) {
        input
      } else {
        build_geomark_feature_url(input)
      }
    } else {
      build_geomark_feature_url(input)
    }

    if (is.na(feature_url)) {
      stop(
        "Could not interpret AOI input as a file path, Geomark URL, or Geomark ID: ",
        input
      )
    }

    tmp_geojson <- tempfile(pattern = "geomark_", fileext = ".geojson")
    on.exit(unlink(tmp_geojson), add = TRUE)
    utils::download.file(feature_url, tmp_geojson, mode = "wb", quiet = TRUE)
    validate_geojson_payload(tmp_geojson, feature_url)
    aoi <- sf::st_read(tmp_geojson, quiet = TRUE)
  }

  if (!inherits(aoi, "sf") || nrow(aoi) == 0 || is.null(sf::st_geometry(aoi))) {
    stop("AOI did not load as a valid sf geodata object.")
  }

  aoi <- sf::st_transform(aoi, BC_ALBERS_EPSG)
  if (any(!sf::st_is_valid(aoi))) {
    aoi <- sf::st_make_valid(aoi)
  }
  aoi
}

#' Determine the geometry type of an sf object
#'
#' Returns "polygon", "line", or "point".
#'
#' @param sf_obj An sf object.
#' @return Character string: "polygon", "line", or "point".
get_geometry_type <- function(sf_obj) {
  geom_type <- sf::st_geometry_type(sf_obj, by_geometry = FALSE)
  geom_type <- as.character(geom_type)
  if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
    return("polygon")
  } else if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
    return("line")
  } else if (geom_type %in% c("POINT", "MULTIPOINT")) {
    return("point")
  } else {
    return("unknown")
  }
}

#' Normalise a geometry type string to expected values
#'
#' @param x Character geometry label.
#' @return One of "polygon", "line", "point", "raster", or "unknown".
normalise_geometry_type <- function(x) {
  val <- tolower(trimws(as.character(x %||% "")))
  if (val %in% c("polygon", "polygons", "poly", "multipolygon")) {
    return("polygon")
  }
  if (val %in% c("line", "lines", "linestring", "multilinestring")) {
    return("line")
  }
  if (val %in% c("point", "points", "multipoint")) {
    return("point")
  }
  if (val %in% c("raster", "grid", "tif", "geotiff")) {
    return("raster")
  }
  "unknown"
}

#' Parse configured key values into a clean field list
#'
#' @param key_values Character scalar from the config.
#' @return Character vector of candidate field names.
parse_key_values <- function(key_values) {
  if (is.null(key_values) || is.na(key_values) || trimws(key_values) == "") {
    return(NULL)
  }
  tokens <- unlist(strsplit(as.character(key_values), "[,;]"))
  tokens <- trimws(tokens)
  tokens <- tokens[tokens != ""]
  # Keep simple attribute names only (skip narrative conditional text)
  tokens <- tokens[grepl("^[A-Za-z_][A-Za-z0-9_]*$", tokens)]
  unique(tokens)
}

#' Parse a distance string (e.g., "1 km", "500 m") to metres
#'
#' @param x Distance string.
#' @param default_m Default distance if blank/NA.
#' @return Numeric distance in metres or NA.
parse_distance_m <- function(x, default_m = NA_real_) {
  if (is.null(x) || is.na(x) || trimws(x) == "") {
    return(default_m)
  }
  raw <- tolower(trimws(as.character(x)))
  num <- suppressWarnings(as.numeric(gsub("[^0-9.]+", "", raw)))
  if (is.na(num)) {
    return(default_m)
  }
  if (grepl("km", raw)) {
    return(num * 1000)
  }
  if (grepl("m", raw)) {
    return(num)
  }
  # Bare number defaults to metres.
  num
}

#' Check whether two terra extents overlap
#'
#' @param ext1 A terra extent-like object.
#' @param ext2 A terra extent-like object.
#' @return Logical TRUE when extents overlap (or touch), FALSE otherwise.
extents_overlap <- function(ext1, ext2) {
  if (is.null(ext1) || is.null(ext2)) {
    return(FALSE)
  }

  v1 <- tryCatch(as.vector(ext1), error = function(e) NULL)
  v2 <- tryCatch(as.vector(ext2), error = function(e) NULL)
  if (is.null(v1) || is.null(v2) || length(v1) < 4 || length(v2) < 4) {
    return(FALSE)
  }

  # Vector order is xmin, xmax, ymin, ymax.
  !(v1[[2]] < v2[[1]] || v2[[2]] < v1[[1]] || v1[[4]] < v2[[3]] || v2[[4]] < v1[[3]])
}

#' Resolve a local layer file from a layer identifier/name
#'
#' @param layer_identifier Layer source identifier.
#' @param layer_name Human-readable layer name.
#' @param search_dir Directory to search for local files.
#' @return Path to the first matching file, or NA.
resolve_local_layer_path <- function(layer_identifier, layer_name,
                                     search_dir = "data/input") {
  candidates <- unique(c(
    as.character(layer_identifier %||% ""),
    as.character(layer_name %||% "")
  ))
  candidates <- trimws(candidates)
  candidates <- candidates[candidates != ""]

  # Keep explicit paths first.
  explicit <- candidates[file.exists(candidates)]
  if (length(explicit) > 0) {
    return(explicit[[1]])
  }

  if (!dir.exists(search_dir)) {
    return(NA_character_)
  }

  files <- list.files(search_dir,
    pattern = "\\.(gpkg|shp|geojson|json|tif|tiff)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0) {
    return(NA_character_)
  }

  stems <- tools::file_path_sans_ext(basename(files))
  for (cand in candidates) {
    stem <- tools::file_path_sans_ext(basename(cand))
    hit <- which(tolower(stems) == tolower(stem))
    if (length(hit) > 0) {
      return(files[[hit[[1]]]])
    }
  }

  NA_character_
}
