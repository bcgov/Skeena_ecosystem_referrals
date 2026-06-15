# =============================================================================
# Layer Loading and Initialisation Functions
# =============================================================================

# Allow this module to be sourced independently.
if (!exists("BC_ALBERS_EPSG", inherits = TRUE)) {
  source("R/utils_core.R")
}

# Bind shared core symbols locally for standalone/static analysis friendliness.
BC_ALBERS_EPSG <- get("BC_ALBERS_EPSG", inherits = TRUE)
normalise_geometry_type <- get("normalise_geometry_type", inherits = TRUE)
resolve_local_layer_path <- get("resolve_local_layer_path", inherits = TRUE)
parse_distance_m <- get("parse_distance_m", inherits = TRUE)
parse_key_values <- get("parse_key_values", inherits = TRUE)

#' SQL-quote an identifier for OGR/SQLite queries
#'
#' @param x Identifier string.
#' @return Quoted identifier.
quote_sql_ident <- function(x) {
  paste0('"', gsub('"', '""', as.character(x)), '"')
}

#' Build a selective read query for GeoPackage layers
#'
#' Uses `Key_values` fields plus geometry to avoid loading unnecessary
#' attributes when reading vector layers.
#'
#' @param layer_path Path to vector dataset.
#' @param fields_to_extract Character vector of desired attribute names.
#' @return SQL query string or NULL when unavailable.
build_select_query <- function(layer_path, fields_to_extract = NULL) {
  ext <- tolower(tools::file_ext(layer_path))
  if (!identical(ext, "gpkg")) {
    return(NULL)
  }
  if (is.null(fields_to_extract) || length(fields_to_extract) == 0) {
    return(NULL)
  }

  layer_info <- tryCatch(
    suppressWarnings(suppressMessages(sf::st_layers(layer_path))),
    error = function(e) NULL
  )
  if (is.null(layer_info)) {
    return(NULL)
  }

  layer_col <- if ("name" %in% names(layer_info)) {
    "name"
  } else if ("layer_name" %in% names(layer_info)) {
    "layer_name"
  } else {
    NA_character_
  }
  if (is.na(layer_col) || length(layer_info[[layer_col]]) == 0) {
    return(NULL)
  }

  table_name <- as.character(layer_info[[layer_col]][[1]])
  if (!nzchar(trimws(table_name))) {
    return(NULL)
  }

  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    return(NULL)
  }

  con <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), layer_path), error = function(e) NULL)
  on.exit(
    {
      if (!is.null(con)) {
        try(DBI::dbDisconnect(con), silent = TRUE)
      }
    },
    add = TRUE
  )
  if (is.null(con)) {
    return(NULL)
  }

  escaped_table <- gsub("'", "''", table_name)
  geom_query <- paste0(
    "SELECT column_name FROM gpkg_geometry_columns WHERE lower(table_name) = lower('",
    escaped_table,
    "') LIMIT 1"
  )
  geom_col <- tryCatch(DBI::dbGetQuery(con, geom_query)$column_name[[1]], error = function(e) NA_character_)
  if (is.na(geom_col) || !nzchar(trimws(geom_col))) {
    geom_col <- "geom"
  }

  attrs_query <- paste0("PRAGMA table_info(", quote_sql_ident(table_name), ")")
  attrs <- tryCatch(DBI::dbGetQuery(con, attrs_query)$name, error = function(e) character(0))
  attrs <- as.character(attrs)
  available <- intersect(as.character(fields_to_extract), attrs)

  select_cols <- unique(c(geom_col, available))
  if (length(select_cols) == 0) {
    return(NULL)
  }

  select_sql <- paste(vapply(select_cols, quote_sql_ident, character(1)), collapse = ", ")
  paste0(
    "SELECT ",
    select_sql,
    " FROM ",
    quote_sql_ident(table_name)
  )
}

#' Build a safe cache stem from XLSX layer metadata
#'
#' @param layer_name Human-readable layer name from config.
#' @param layer_identifier Source layer identifier from config.
#' @param fallback Fallback stem when both names are empty.
#' @return Sanitized file stem.
layer_cache_stem <- function(layer_name,
                             layer_identifier = NA_character_,
                             fallback = "layer") {
  pick <- as.character(layer_name %||% "")
  if (!nzchar(trimws(pick))) {
    pick <- as.character(layer_identifier %||% "")
  }
  if (!nzchar(trimws(pick))) {
    pick <- fallback
  }
  stem <- gsub("[^A-Za-z0-9_-]+", "_", trimws(pick))
  stem <- gsub("_+", "_", stem)
  stem <- gsub("^_+|_+$", "", stem)
  if (!nzchar(stem)) {
    stem <- fallback
  }
  stem
}

#' Normalize text for permissive layer name matching
#'
#' @param x Character input.
#' @return Lowercase alphanumeric-only key.
normalise_layer_key <- function(x) {
  out <- tolower(trimws(as.character(x %||% "")))
  out <- gsub("[^a-z0-9]+", "", out)
  out
}

#' Preferred on-disk extension for a layer geometry type
#'
#' @param geometry_type Geometry type string.
#' @return Extension without leading dot.
cache_extension_for_geometry <- function(geometry_type = "polygon") {
  if (identical(normalise_geometry_type(geometry_type), "raster")) {
    return("tif")
  }
  "gpkg"
}

#' Build deterministic cache path for a configured layer
#'
#' @param layer_name Human-readable layer name.
#' @param layer_identifier Source layer identifier.
#' @param geometry_type Geometry type.
#' @param search_dir Base cache directory.
#' @param extension Optional explicit extension.
#' @return Full file path in search_dir.
layer_cache_path <- function(layer_name,
                             layer_identifier = NA_character_,
                             geometry_type = "polygon",
                             search_dir = "data/input",
                             extension = NA_character_) {
  ext <- as.character(extension)
  if (length(ext) == 0 || is.na(ext) || !nzchar(trimws(ext))) {
    ext <- cache_extension_for_geometry(geometry_type)
  }
  stem <- layer_cache_stem(layer_name, layer_identifier)
  file.path(search_dir, paste0(stem, ".", ext))
}

#' Resolve local path for configured layer using deterministic cache first
#'
#' @param layer_identifier Source layer identifier.
#' @param layer_name Human-readable layer name.
#' @param geometry_type Geometry type.
#' @param search_dir Directory to search.
#' @return Existing path or NA.
resolve_layer_path_for_config <- function(layer_identifier,
                                          layer_name,
                                          geometry_type = "polygon",
                                          search_dir = "data/input") {
  exts <- c(cache_extension_for_geometry(geometry_type), "gpkg", "shp", "geojson", "json", "tif", "tiff")
  exts <- unique(exts)

  for (ext in exts) {
    candidate <- layer_cache_path(
      layer_name = layer_name,
      layer_identifier = layer_identifier,
      geometry_type = geometry_type,
      search_dir = search_dir,
      extension = ext
    )
    if (file.exists(candidate) && is_readable_layer_file(candidate, geometry_type = geometry_type)) {
      return(candidate)
    }
  }

  fallback <- resolve_local_layer_path(layer_identifier, layer_name, search_dir = search_dir)
  if (!is.na(fallback) && is_readable_layer_file(fallback, geometry_type = geometry_type)) {
    return(fallback)
  }

  # Final fuzzy match pass for local files that differ by punctuation/separators.
  files <- list.files(
    search_dir,
    pattern = "\\.(gpkg|shp|geojson|json|tif|tiff)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) > 0) {
    keys <- unique(c(
      normalise_layer_key(layer_name),
      normalise_layer_key(tools::file_path_sans_ext(basename(layer_identifier)))
    ))
    keys <- keys[nzchar(keys)]
    if (length(keys) > 0) {
      stems <- tools::file_path_sans_ext(basename(files))
      file_keys <- vapply(stems, normalise_layer_key, character(1))
      hit <- which(file_keys %in% keys)
      if (length(hit) > 0) {
        for (idx in hit) {
          cand <- files[[idx]]
          if (is_readable_layer_file(cand, geometry_type = geometry_type)) {
            return(cand)
          }
        }
      }
    }
  }

  NA_character_
}

#' Check if a local file can be read as the expected spatial type
#'
#' @param layer_path File path.
#' @param geometry_type Expected geometry type.
#' @return Logical TRUE when readable, otherwise FALSE.
is_readable_layer_file <- function(layer_path, geometry_type = "polygon") {
  if (is.na(layer_path) || trimws(layer_path) == "" || !file.exists(layer_path)) {
    return(FALSE)
  }

  # Quick signature checks avoid noisy GDAL errors when an HTML page is saved
  # with a spatial extension (for example, catalogue landing pages as .gpkg).
  ext <- tolower(tools::file_ext(layer_path))
  sig_raw <- tryCatch(readBin(layer_path, what = "raw", n = 32), error = function(e) raw(0))
  sig_txt <- tryCatch(rawToChar(sig_raw, multiple = FALSE), error = function(e) "")

  if (ext == "gpkg") {
    sqlite_magic <- charToRaw("SQLite format 3")
    has_sqlite_magic <- length(sig_raw) >= length(sqlite_magic) &&
      identical(sig_raw[seq_along(sqlite_magic)], sqlite_magic)
    if (!has_sqlite_magic) {
      return(FALSE)
    }
  }

  if (ext %in% c("tif", "tiff")) {
    tif_ii <- length(sig_raw) >= 4 &&
      identical(sig_raw[1:4], as.raw(c(0x49, 0x49, 0x2A, 0x00)))
    tif_mm <- length(sig_raw) >= 4 &&
      identical(sig_raw[1:4], as.raw(c(0x4D, 0x4D, 0x00, 0x2A)))
    if (!(tif_ii || tif_mm)) {
      return(FALSE)
    }
  }

  sig_lower <- tolower(sig_txt)
  if (startsWith(sig_lower, "<!doctype html") || startsWith(sig_lower, "<html")) {
    return(FALSE)
  }

  geom <- normalise_geometry_type(geometry_type)
  if (identical(geom, "raster")) {
    ok <- tryCatch(
      {
        r <- terra::rast(layer_path)
        !is.null(r)
      },
      error = function(e) FALSE
    )
    return(isTRUE(ok))
  }

  ok <- tryCatch(
    {
      lyr <- suppressWarnings(suppressMessages(sf::st_layers(layer_path)))
      layer_col <- if ("name" %in% names(lyr)) {
        "name"
      } else if ("layer_name" %in% names(lyr)) {
        "layer_name"
      } else {
        NA_character_
      }
      !is.null(lyr) && !is.na(layer_col) && length(lyr[[layer_col]]) > 0
    },
    error = function(e) FALSE
  )
  isTRUE(ok)
}

#' Best-effort BCDC layer retrieval
#'
#' @param bcdc_id BCDC identifier from config (UUID or layer name).
#' @param geometry_type Expected geometry type.
#' @return sf object or NULL.
load_layer_from_bcdc <- function(bcdc_id, geometry_type = "polygon") {
  if (!requireNamespace("bcdata", quietly = TRUE)) {
    warning("Package 'bcdata' is not installed; cannot load BCDC layer: ", bcdc_id)
    return(NULL)
  }

  ns <- asNamespace("bcdata")
  out <- NULL

  if (exists("bcdc_get_data", envir = ns, inherits = FALSE)) {
    fun <- get("bcdc_get_data", envir = ns)
    out <- tryCatch(
      do.call(fun, list(record = bcdc_id)),
      error = function(e) NULL
    )
  }

  if (is.null(out) && exists("bcdc_query_geodata", envir = ns, inherits = FALSE)) {
    qfun <- get("bcdc_query_geodata", envir = ns)
    out <- tryCatch(
      {
        q <- do.call(qfun, list(bcdc_id))
        if (inherits(q, "sf")) q else NULL
      },
      error = function(e) NULL
    )
  }

  if (is.null(out)) {
    warning("Failed to load BCDC layer for ID/name: ", bcdc_id)
    return(NULL)
  }

  if (!inherits(out, "sf")) {
    out <- tryCatch(sf::st_as_sf(out), error = function(e) NULL)
  }
  if (is.null(out)) {
    warning("BCDC result could not be converted to sf: ", bcdc_id)
    return(NULL)
  }

  out <- sf::st_transform(out, BC_ALBERS_EPSG)
  if (any(!sf::st_is_valid(out))) {
    out <- sf::st_make_valid(out)
  }
  out
}

#' Load the layers configuration from an Excel workbook
#'
#' Reads the referral_layers.xlsx workbook and returns a standardised
#' data.frame of all layer definitions.
#'
#' @param config_path Path to the .xlsx configuration workbook.
#' @param default_buffer_m Default buffer distance (metres) when
#'   Distance_outside_plot is empty.
#' @return A data.frame of layer definitions.
load_layers_config <- function(config_path = "referral_layers.xlsx",
                               default_buffer_m = NA_real_) {
  if (!file.exists(config_path)) {
    stop("Layers configuration file not found: ", config_path)
  }

  ext <- tolower(tools::file_ext(config_path))

  if (ext %in% c("xlsx", "xls")) {
    raw <- readxl::read_excel(config_path)
    names(raw) <- gsub("[^a-z0-9]+", "_", tolower(names(raw)))

    required <- c(
      "focus", "name", "layer", "geometry",
      "spatial_information_polygon", "spatial_information_linestring"
    )
    missing_cols <- required[!required %in% names(raw)]
    if (length(missing_cols) > 0) {
      stop("Config is missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    source_data_name <- if ("source_data_name" %in% names(raw)) raw$source_data_name else NA_character_
    bcdc_id <- if ("bcdc_id" %in% names(raw)) raw$bcdc_id else NA_character_
    key_values <- if ("key_values" %in% names(raw)) raw$key_values else NA_character_
    additional_notes <- if ("additional_notes" %in% names(raw)) raw$additional_notes else NA_character_
    distance_outside_m <- if ("distance_outside_plot" %in% names(raw)) {
      vapply(raw$distance_outside_plot, parse_distance_m, numeric(1), default_m = default_buffer_m)
    } else {
      rep(default_buffer_m, nrow(raw))
    }

    config <- data.frame(
      focus = raw$focus,
      name = raw$name,
      layer_name = make.unique(ifelse(is.na(raw$name) | trimws(raw$name) == "", raw$layer, raw$name)),
      source_data_name = source_data_name,
      layer_identifier = raw$layer,
      bcdc_id = bcdc_id,
      geometry_type = vapply(raw$geometry, normalise_geometry_type, character(1)),
      spatial_information_polygon = raw$spatial_information_polygon,
      spatial_information_linestring = raw$spatial_information_linestring,
      key_values = key_values,
      distance_outside_m = distance_outside_m,
      additional_notes = additional_notes,
      stringsAsFactors = FALSE
    )

    config$fields_to_extract <- lapply(config$key_values, parse_key_values)
    config$layer_type <- "file"
    config$cache_stem <- vapply(
      seq_len(nrow(config)),
      function(i) layer_cache_stem(config$name[[i]], config$layer_identifier[[i]], fallback = paste0("layer_", i)),
      character(1)
    )
    config$layer_path <- vapply(
      seq_len(nrow(config)),
      function(i) {
        candidate <- resolve_layer_path_for_config(
          layer_identifier = config$layer_identifier[[i]],
          layer_name = config$name[[i]],
          geometry_type = config$geometry_type[[i]],
          search_dir = "data/input"
        )
        if (!is.na(candidate) && file.exists(candidate)) {
          return(candidate)
        }

        src_name <- as.character(config$source_data_name[[i]] %||% "")
        if (!nzchar(src_name) || grepl("^https?://", src_name)) {
          return(NA_character_)
        }

        src_candidate <- resolve_local_layer_path(src_name, config$name[[i]], search_dir = "data/input")
        if (!is.na(src_candidate) &&
          file.exists(src_candidate) &&
          is_readable_layer_file(src_candidate, geometry_type = config$geometry_type[[i]])) {
          return(src_candidate)
        }

        NA_character_
      },
      character(1)
    )
    config$enabled <- TRUE
    return(config)
  }

  stop("Unsupported config format (expected .xlsx or .xls): ", config_path)
}

#' Safely load a spatial layer
#'
#' Attempts to load a spatial layer from the given path. Returns NULL with a
#' warning if the file is missing or cannot be read.
#'
#' @param layer_path File path to the spatial layer.
#' @param layer_type Either "file" for vector/raster files.
#' @return An sf object (for vector data), a SpatRaster (for raster), or NULL.
load_layer <- function(layer_path,
                       layer_type = "file",
                       geometry_type = "polygon",
                       fields_to_extract = NULL,
                       bcdc_id = NA_character_,
                       layer_name = NA_character_,
                       layer_identifier = NA_character_,
                       search_dir = "data/input",
                       cache_bcdc = TRUE) {
  if (!is.na(layer_path) && trimws(layer_path) != "" && file.exists(layer_path) &&
    !is_readable_layer_file(layer_path, geometry_type = geometry_type)) {
    warning("Layer cache file is unreadable; ignoring cached file: ", layer_path)
    layer_path <- NA_character_
  }

  if (is.na(layer_path) || trimws(layer_path) == "" || !file.exists(layer_path)) {
    resolved <- resolve_layer_path_for_config(
      layer_identifier = layer_identifier,
      layer_name = layer_name,
      geometry_type = geometry_type,
      search_dir = search_dir
    )
    if (!is.na(resolved) && file.exists(resolved)) {
      layer_path <- resolved
    }
  }

  if ((is.na(layer_path) || trimws(layer_path) == "" || !file.exists(layer_path)) &&
    !is.na(bcdc_id) && trimws(bcdc_id) != "") {
    lyr <- load_layer_from_bcdc(bcdc_id, geometry_type = geometry_type)
    if (!is.null(lyr)) {
      if (isTRUE(cache_bcdc) && inherits(lyr, "sf")) {
        if (!dir.exists(search_dir)) {
          dir.create(search_dir, recursive = TRUE, showWarnings = FALSE)
        }
        cache_path <- layer_cache_path(
          layer_name = layer_name,
          layer_identifier = layer_identifier,
          geometry_type = geometry_type,
          search_dir = search_dir,
          extension = "gpkg"
        )
        tryCatch(
          sf::st_write(lyr, cache_path, delete_dsn = TRUE, quiet = TRUE),
          error = function(e) NULL
        )
      }
      return(lyr)
    }
  }

  if (is.na(layer_path) || trimws(layer_path) == "" || !file.exists(layer_path)) {
    warning("Layer file not found, skipping: ", layer_path)
    return(NULL)
  }
  tryCatch(
    {
      if (geometry_type == "raster") {
        lyr <- terra::rast(layer_path)
      } else {
        select_query <- build_select_query(layer_path, fields_to_extract = fields_to_extract)
        if (!is.null(select_query)) {
          lyr <- tryCatch(
            sf::st_read(layer_path, query = select_query, quiet = TRUE),
            error = function(e) NULL
          )
        } else {
          lyr <- NULL
        }

        if (is.null(lyr)) {
          lyr <- sf::st_read(layer_path, quiet = TRUE)
        }

        if (!is.null(fields_to_extract) && length(fields_to_extract) > 0) {
          keep <- unique(c(fields_to_extract, attr(lyr, "sf_column")))
          keep <- keep[keep %in% names(lyr)]
          if (length(keep) > 0) {
            lyr <- lyr[, keep, drop = FALSE]
          }
        }
        lyr <- sf::st_transform(lyr, BC_ALBERS_EPSG)
        if (any(!sf::st_is_valid(lyr))) {
          lyr <- sf::st_make_valid(lyr)
        }
      }
      lyr
    },
    error = function(e) {
      warning("Failed to load layer: ", layer_path, "\n  ", conditionMessage(e))
      NULL
    }
  )
}

#' Download a file-based layer from a URL
#'
#' @param url Layer URL.
#' @param dest_dir Destination directory.
#' @param target_stem Optional deterministic cache stem.
#' @param geometry_type Expected geometry type.
#' @param overwrite_existing Whether to overwrite existing readable files.
#' @return Downloaded spatial file path, or NA if download/resolve fails.
download_layer_file <- function(url,
                                dest_dir = "data/input",
                                target_stem = NA_character_,
                                geometry_type = "polygon",
                                overwrite_existing = FALSE,
                                verbose = TRUE) {
  if (is.na(url) || trimws(url) == "" || !grepl("^https?://", url)) {
    return(NA_character_)
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  clean_url <- sub("\\?.*$", "", url)
  source_ext <- tolower(tools::file_ext(clean_url))
  file_name <- basename(clean_url)
  if (is.na(file_name) || trimws(file_name) == "") {
    file_name <- paste0("layer_", format(Sys.time(), "%Y%m%d%H%M%S"))
  }

  if (!is.na(target_stem) && nzchar(trimws(target_stem))) {
    ext <- if (nzchar(source_ext)) source_ext else cache_extension_for_geometry(geometry_type)
    file_name <- paste0(target_stem, ".", ext)
  }

  dest_file <- file.path(dest_dir, file_name)

  if (!isTRUE(overwrite_existing) &&
    file.exists(dest_file) &&
    is_readable_layer_file(dest_file, geometry_type = geometry_type)) {
    if (isTRUE(verbose)) {
      message("Reusing existing downloaded file: ", dest_file)
    }
    return(dest_file)
  }

  if (isTRUE(overwrite_existing) && file.exists(dest_file)) {
    try(unlink(dest_file), silent = TRUE)
  }

  ok <- tryCatch(
    {
      if (isTRUE(verbose)) {
        message("Downloading layer from URL: ", url)
      }
      utils::download.file(url, destfile = dest_file, mode = "wb", quiet = TRUE)
      if (isTRUE(verbose)) {
        message("Downloaded to: ", dest_file)
      }
      TRUE
    },
    error = function(e) FALSE
  )

  if (!ok || !file.exists(dest_file)) {
    return(NA_character_)
  }

  if (grepl("\\.zip$", dest_file, ignore.case = TRUE)) {
    unzip_folder <- if (!is.na(target_stem) && nzchar(trimws(target_stem))) {
      target_stem
    } else {
      tools::file_path_sans_ext(basename(dest_file))
    }
    unzip_dir <- file.path(dest_dir, unzip_folder)
    dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)

    if (!isTRUE(overwrite_existing) && dir.exists(unzip_dir)) {
      existing <- list.files(
        unzip_dir,
        pattern = "\\.(gpkg|shp|geojson|json|tif|tiff)$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
      if (length(existing) > 0) {
        readable <- existing[vapply(existing, is_readable_layer_file, logical(1), geometry_type = geometry_type)]
        if (length(readable) > 0) {
          if (isTRUE(verbose)) {
            message("Reusing existing extracted file: ", readable[[1]])
          }
          return(readable[[1]])
        }
      }
    }

    extracted <- tryCatch(
      utils::unzip(dest_file, exdir = unzip_dir),
      error = function(e) character(0)
    )
    spatial <- extracted[grepl("\\.(gpkg|shp|geojson|json|tif|tiff)$", extracted, ignore.case = TRUE)]
    if (length(spatial) > 0) {
      for (candidate in spatial) {
        if (is_readable_layer_file(candidate, geometry_type = geometry_type)) {
          if (isTRUE(verbose)) {
            message("Using extracted spatial file: ", candidate)
          }
          return(candidate)
        }
      }
    }
    return(NA_character_)
  }

  if (!is_readable_layer_file(dest_file, geometry_type = geometry_type)) {
    warning("Downloaded file is not a readable spatial layer: ", dest_file)
    try(unlink(dest_file), silent = TRUE)
    return(NA_character_)
  }

  dest_file
}

#' Initialise local layer data for referral analysis
#'
#' Attempts to ensure each configured layer is available locally by checking
#' existing files, downloading URL-based layers, and optionally pulling missing
#' vector layers from BCDC. Warns with a consolidated list of layers still
#' missing after all attempts.
#'
#' @param config_path Path to layer config workbook.
#' @param search_dir Folder for local input data.
#' @param download_missing Whether to attempt downloading missing layers.
#' @param download_from_bcdc Whether to attempt BCDC retrieval for missing layers.
#' @param warn_missing Whether to emit a warning for unresolved layers.
#' @param overwrite_existing Whether to force re-download/re-write even when a
#'   readable local file already exists for the configured layer.
#' @param verbose Whether to print progress messages for reuse/download actions.
#' @param url_fallback_for_bcdc Whether URL download should be attempted for
#'   layers that already have a BCDC identifier when BCDC retrieval fails.
#' @param default_buffer_m Default distance (metres) for blank buffer values.
#' @return A list with `config`, `status`, `downloaded_layers`, and
#'   `missing_layers`.
initialize_referral_layers <- function(config_path = "referral_layers.xlsx",
                                       search_dir = "data/input",
                                       download_missing = TRUE,
                                       download_from_bcdc = TRUE,
                                       warn_missing = TRUE,
                                       overwrite_existing = FALSE,
                                       verbose = TRUE,
                                       url_fallback_for_bcdc = FALSE,
                                       default_buffer_m = NA_real_) {
  config <- load_layers_config(config_path = config_path, default_buffer_m = default_buffer_m)

  if (!dir.exists(search_dir)) {
    dir.create(search_dir, recursive = TRUE, showWarnings = FALSE)
  }

  status <- data.frame(
    layer_name = config$layer_name,
    status = rep("missing", nrow(config)),
    layer_path = rep(NA_character_, nrow(config)),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(config))) {
    layer_name <- as.character(config$layer_name[[i]])
    layer_display_name <- as.character(config$name[[i]] %||% layer_name)
    layer_identifier <- as.character(config$layer_identifier[[i]] %||% "")
    layer_geom <- as.character(config$geometry_type[[i]] %||% "polygon")
    cache_stem <- layer_cache_stem(layer_display_name, layer_identifier, fallback = paste0("layer_", i))

    resolved <- resolve_layer_path_for_config(
      layer_identifier = layer_identifier,
      layer_name = layer_display_name,
      geometry_type = layer_geom,
      search_dir = search_dir
    )
    if (is.na(resolved) || !file.exists(resolved)) {
      src_name <- as.character(config$source_data_name[[i]] %||% "")
      if (nzchar(src_name) && !grepl("^https?://", src_name)) {
        src_candidate <- resolve_local_layer_path(src_name, layer_display_name, search_dir = search_dir)
        if (!is.na(src_candidate) &&
          file.exists(src_candidate) &&
          is_readable_layer_file(src_candidate, geometry_type = layer_geom)) {
          resolved <- src_candidate
        }
      }
    }
    if (!is.na(resolved) && file.exists(resolved)) {
      if (isTRUE(verbose)) {
        message("[", i, "/", nrow(config), "] Reusing local layer: ", layer_name, " -> ", resolved)
      }
      config$layer_path[[i]] <- resolved
      status$status[[i]] <- "found_local"
      status$layer_path[[i]] <- resolved
      if (!isTRUE(overwrite_existing)) {
        next
      }
    }

    if (!download_missing) {
      if (isTRUE(verbose)) {
        message("[", i, "/", nrow(config), "] Missing layer and download disabled: ", layer_name)
      }
      next
    }

    downloaded_path <- NA_character_

    source_url <- as.character(config$source_data_name[[i]] %||% "")
    bcdc_id <- as.character(config$bcdc_id[[i]] %||% "")
    has_bcdc <- isTRUE(download_from_bcdc) && nzchar(trimws(bcdc_id))

    # Prefer BCDC directly for BCDC-backed layers to avoid repeatedly hitting
    # catalogue landing pages that are not direct spatial downloads.
    if (has_bcdc) {
      candidate <- layer_cache_path(
        layer_name = layer_display_name,
        layer_identifier = layer_identifier,
        geometry_type = layer_geom,
        search_dir = search_dir,
        extension = "gpkg"
      )

      if (!isTRUE(overwrite_existing) &&
        file.exists(candidate) &&
        is_readable_layer_file(candidate, geometry_type = layer_geom)) {
        if (isTRUE(verbose)) {
          message("[", i, "/", nrow(config), "] Reusing cached BCDC file: ", candidate)
        }
        downloaded_path <- candidate
      } else {
        bcdc_layer <- load_layer_from_bcdc(
          bcdc_id = bcdc_id,
          geometry_type = as.character(config$geometry_type[[i]])
        )
        if (!is.null(bcdc_layer)) {
          if (isTRUE(verbose)) {
            message("[", i, "/", nrow(config), "] Downloading from BCDC: ", layer_name)
          }
          write_ok <- tryCatch(
            {
              if (file.exists(candidate)) {
                try(unlink(candidate), silent = TRUE)
              }
              sf::st_write(bcdc_layer, candidate, delete_dsn = TRUE, quiet = TRUE)
              TRUE
            },
            error = function(e) FALSE
          )
          if (write_ok && file.exists(candidate)) {
            if (isTRUE(verbose)) {
              message("[", i, "/", nrow(config), "] Saved BCDC cache: ", candidate)
            }
            downloaded_path <- candidate
          }
        }
      }
    }

    # URL attempt for non-BCDC layers, or optional fallback for BCDC layers.
    allow_url_attempt <- !has_bcdc || isTRUE(url_fallback_for_bcdc)

    if ((is.na(downloaded_path) || !file.exists(downloaded_path)) &&
      allow_url_attempt &&
      grepl("^https?://", layer_identifier)) {
      downloaded_path <- download_layer_file(
        url = layer_identifier,
        dest_dir = search_dir,
        target_stem = cache_stem,
        geometry_type = layer_geom,
        overwrite_existing = overwrite_existing,
        verbose = verbose
      )
    }

    if ((is.na(downloaded_path) || !file.exists(downloaded_path)) &&
      allow_url_attempt &&
      grepl("^https?://", source_url)) {
      downloaded_path <- download_layer_file(
        url = source_url,
        dest_dir = search_dir,
        target_stem = cache_stem,
        geometry_type = layer_geom,
        overwrite_existing = overwrite_existing,
        verbose = verbose
      )
    }

    if (!is.na(downloaded_path) && file.exists(downloaded_path)) {
      config$layer_path[[i]] <- downloaded_path
      status$status[[i]] <- "downloaded"
      status$layer_path[[i]] <- downloaded_path
    }
  }

  missing_idx <- which(!(status$status %in% c("found_local", "downloaded")))
  missing_layers <- status$layer_name[missing_idx]
  downloaded_layers <- status$layer_name[which(status$status == "downloaded")]

  if (warn_missing && length(missing_layers) > 0) {
    warning(
      "Missing layers after initialisation: ",
      paste(missing_layers, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    config = config,
    status = status,
    downloaded_layers = downloaded_layers,
    missing_layers = missing_layers
  )
}
