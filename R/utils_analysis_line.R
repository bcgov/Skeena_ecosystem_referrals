# =============================================================================
# Line AOI Analysis Functions
# =============================================================================

attach_single_field_tables <- get0(
  "attach_single_field_tables",
  mode = "function",
  ifnotfound = function(grouped_result, raw_result, group_fields, summary_fun) grouped_result
)

utils::globalVariables(".data")

#' Compute overlap of a line AOI with a polygon reference layer
#'
#' Calculates the length of the line AOI that falls within each polygon feature.
#'
#' @param aoi An sf line object (the geomark AOI).
#' @param ref_layer An sf polygon object.
#' @param fields_to_extract Character vector of field names to include.
#' @return A data.frame with overlap length (km) and extracted fields.
line_polygon_overlap <- function(aoi, ref_layer, fields_to_extract = NULL) {
  aoi_total_len_km <- as.numeric(sf::st_length(sf::st_union(aoi))) / 1000
  intersection <- sf::st_intersection(sf::st_union(aoi), ref_layer)
  if (nrow(intersection) == 0) {
    return(NULL)
  }
  intersection$overlap_length_km <- as.numeric(sf::st_length(intersection)) / 1000
  unique_overlap_length_km <- as.numeric(sf::st_length(sf::st_union(sf::st_geometry(intersection)))) / 1000

  result <- sf::st_drop_geometry(intersection)
  group_fields <- character(0)
  if (!is.null(fields_to_extract)) {
    group_fields <- fields_to_extract[fields_to_extract %in% names(result)]
  }

  if (length(group_fields) > 0) {
    result <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_fields))) |>
      dplyr::summarise(overlap_length_km = sum(.data$overlap_length_km, na.rm = TRUE), .groups = "drop")
  } else {
    result <- data.frame(overlap_length_km = sum(result$overlap_length_km, na.rm = TRUE))
  }

  result <- attach_single_field_tables(
    result,
    sf::st_drop_geometry(intersection),
    group_fields,
    function(data, field) {
      data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(field))) |>
        dplyr::summarise(overlap_length_km = sum(.data$overlap_length_km, na.rm = TRUE), .groups = "drop")
    }
  )

  attr(result, "coverage_summary") <- data.frame(
    total_overlap_length_km = unique_overlap_length_km,
    total_pct_of_aoi_line = if (aoi_total_len_km > 0) (unique_overlap_length_km / aoi_total_len_km) * 100 else NA_real_
  )
  result
}

#' Compute overlap of a line AOI with a raster reference layer
#'
#' Extracts raster values along the line AOI and summarises the length of the
#' line falling on each raster class.
#'
#' @param aoi An sf line object.
#' @param raster_layer A SpatRaster object.
#' @return A data.frame with raster value and length of overlap.
line_raster_overlap <- function(aoi, raster_layer) {
  aoi_proj <- sf::st_transform(aoi, terra::crs(raster_layer))
  aoi_union <- sf::st_union(aoi_proj)
  total_len_km <- as.numeric(sf::st_length(aoi_union)) / 1000
  if (total_len_km == 0) {
    return(NULL)
  }

  aoi_vect <- terra::vect(aoi_union)
  if (!extents_overlap(terra::ext(raster_layer), terra::ext(aoi_vect))) {
    return(NULL)
  }

  cropped <- tryCatch(
    terra::crop(raster_layer, terra::ext(aoi_vect)),
    error = function(e) NULL
  )
  if (is.null(cropped)) {
    return(NULL)
  }

  masked <- terra::mask(cropped, aoi_vect)
  if (all(is.na(terra::values(masked)))) {
    return(NULL)
  }

  cell_polys <- terra::as.polygons(masked, values = TRUE, na.rm = TRUE)
  cell_sf <- sf::st_as_sf(cell_polys)
  value_col <- names(cell_sf)[1]

  seg <- sf::st_intersection(cell_sf, aoi_union)
  if (nrow(seg) == 0) {
    return(NULL)
  }

  seg$overlap_km <- as.numeric(sf::st_length(seg)) / 1000
  seg_df <- sf::st_drop_geometry(seg)
  names(seg_df)[names(seg_df) == value_col] <- "raster_value"
  summary_df <- stats::aggregate(overlap_km ~ raster_value, data = seg_df, FUN = sum)
  summary_df$overlap_length_km <- summary_df$overlap_km
  summary_df$pct_of_line <- (summary_df$overlap_length_km / total_len_km) * 100
  summary_df$overlap_km <- NULL
  summary_df
}

#' Find intersections between a line AOI and linear reference features
#'
#' Identifies linear features that intersect the AOI line and returns their IDs
#' and selected fields.
#'
#' @param aoi An sf line object.
#' @param line_layer An sf line object.
#' @param fields_to_extract Character vector of field names to include.
#' @return A data.frame of intersecting feature attributes.
line_line_intersection <- function(aoi, line_layer, fields_to_extract = NULL) {
  intersects_idx <- sf::st_intersects(line_layer, sf::st_union(aoi), sparse = TRUE)
  hits <- which(lengths(intersects_idx) > 0)
  if (length(hits) == 0) {
    return(NULL)
  }
  result <- sf::st_drop_geometry(line_layer[hits, ])
  group_fields <- character(0)
  if (!is.null(fields_to_extract)) {
    group_fields <- fields_to_extract[fields_to_extract %in% names(result)]
  }

  if (length(group_fields) > 0) {
    result <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_fields))) |>
      dplyr::summarise(intersection_count = dplyr::n(), .groups = "drop")
  } else {
    result <- data.frame(intersection_count = nrow(result))
  }

  result <- attach_single_field_tables(
    result,
    sf::st_drop_geometry(line_layer[hits, ]),
    group_fields,
    function(data, field) {
      data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(field))) |>
        dplyr::summarise(intersection_count = dplyr::n(), .groups = "drop")
    }
  )

  result
}
