# =============================================================================
# Polygon AOI Analysis Functions
# =============================================================================

attach_single_field_tables <- get0(
  "attach_single_field_tables",
  mode = "function",
  ifnotfound = function(grouped_result, raw_result, group_fields, summary_fun) grouped_result
)

utils::globalVariables(".data")

#' Compute polygon-on-polygon overlap
#'
#' Calculates the area and percent overlap between the AOI polygon and a
#' reference polygon layer.
#'
#' @param aoi An sf polygon object (the geomark AOI).
#' @param ref_layer An sf polygon object (the reference layer).
#' @param fields_to_extract Character vector of field names to include.
#' @return A data.frame with overlap area (ha), percent overlap, and extracted fields.
polygon_overlap <- function(aoi, ref_layer, fields_to_extract = NULL) {
  aoi_area <- as.numeric(sf::st_area(sf::st_union(aoi))) # m^2
  intersection <- sf::st_intersection(ref_layer, sf::st_union(aoi))
  if (nrow(intersection) == 0) {
    return(NULL)
  }
  intersection$overlap_area_m2 <- as.numeric(sf::st_area(intersection))
  unique_overlap_area_m2 <- as.numeric(sf::st_area(sf::st_union(sf::st_geometry(intersection))))

  result <- sf::st_drop_geometry(intersection)
  group_fields <- character(0)
  if (!is.null(fields_to_extract)) {
    group_fields <- fields_to_extract[fields_to_extract %in% names(result)]
  }

  if (length(group_fields) > 0) {
    grouped <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_fields))) |>
      dplyr::summarise(overlap_area_m2 = sum(.data$overlap_area_m2, na.rm = TRUE), .groups = "drop")
  } else {
    grouped <- data.frame(overlap_area_m2 = sum(result$overlap_area_m2, na.rm = TRUE))
  }

  grouped$overlap_area_ha <- grouped$overlap_area_m2 / 10000
  grouped$overlap_pct <- (grouped$overlap_area_m2 / aoi_area) * 100

  grouped <- attach_single_field_tables(
    grouped,
    result,
    group_fields,
    function(data, field) {
      split <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(field))) |>
        dplyr::summarise(overlap_area_m2 = sum(.data$overlap_area_m2, na.rm = TRUE), .groups = "drop")
      split$overlap_area_ha <- split$overlap_area_m2 / 10000
      split$overlap_pct <- (split$overlap_area_m2 / aoi_area) * 100
      split[, c(field, "overlap_area_ha", "overlap_pct"), drop = FALSE]
    }
  )

  cols_to_keep <- c(group_fields, "overlap_area_ha", "overlap_pct")
  output <- grouped[, cols_to_keep, drop = FALSE]
  attr(output, "coverage_summary") <- data.frame(
    total_overlap_area_ha = unique_overlap_area_m2 / 10000,
    total_overlap_pct = (unique_overlap_area_m2 / aoi_area) * 100
  )
  output
}

#' Compute raster overlap within an AOI polygon
#'
#' Extracts raster values within the AOI and summarises the percent of the AOI
#' covered by each raster class value.
#'
#' @param aoi An sf polygon object.
#' @param raster_layer A SpatRaster object.
#' @return A data.frame with raster value, count, and percent of AOI.
raster_overlap <- function(aoi, raster_layer) {
  aoi_vect <- terra::vect(sf::st_transform(aoi, terra::crs(raster_layer)))
  if (!extents_overlap(terra::ext(raster_layer), terra::ext(aoi_vect))) {
    return(NULL)
  }

  cropped <- tryCatch(
    terra::crop(raster_layer, aoi_vect),
    error = function(e) NULL
  )
  if (is.null(cropped)) {
    return(NULL)
  }

  masked <- terra::mask(cropped, aoi_vect)
  vals <- terra::freq(masked)
  if (is.null(vals) || nrow(vals) == 0) {
    return(NULL)
  }

  total_cells <- sum(vals$count)
  if (total_cells == 0) {
    return(NULL)
  }
  vals$pct_of_aoi <- (vals$count / total_cells) * 100
  vals
}

#' Compute linear feature density within an AOI polygon
#'
#' Clips linear features to the AOI and calculates total length and density
#' (km per km^2).
#'
#' @param aoi An sf polygon object.
#' @param line_layer An sf line object.
#' @param fields_to_extract Character vector of field names to include.
#' @return A data.frame with total length (km), density (km/km^2), and fields.
line_density_in_polygon <- function(aoi, line_layer, fields_to_extract = NULL) {
  clipped <- sf::st_intersection(line_layer, sf::st_union(aoi))
  if (nrow(clipped) == 0) {
    return(NULL)
  }
  aoi_area_km2 <- as.numeric(sf::st_area(sf::st_union(aoi))) / 1e6
  clipped$length_km <- as.numeric(sf::st_length(clipped)) / 1000

  result <- sf::st_drop_geometry(clipped)
  group_fields <- character(0)
  if (!is.null(fields_to_extract)) {
    group_fields <- fields_to_extract[fields_to_extract %in% names(result)]
  }

  if (length(group_fields) > 0) {
    result <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_fields))) |>
      dplyr::summarise(length_km = sum(.data$length_km, na.rm = TRUE), .groups = "drop")
  } else {
    result <- data.frame(length_km = sum(result$length_km, na.rm = TRUE))
  }

  result <- attach_single_field_tables(
    result,
    sf::st_drop_geometry(clipped),
    group_fields,
    function(data, field) {
      data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(field))) |>
        dplyr::summarise(length_km = sum(.data$length_km, na.rm = TRUE), .groups = "drop")
    }
  )

  summary_row <- data.frame(
    total_length_km = sum(result$length_km),
    density_km_per_km2 = sum(result$length_km) / aoi_area_km2
  )
  list(details = result, summary = summary_row)
}

#' Count and describe points within an AOI polygon
#'
#' Identifies points that fall within the AOI and extracts relevant fields.
#'
#' @param aoi An sf polygon object.
#' @param point_layer An sf point object.
#' @param fields_to_extract Character vector of field names to include.
#' @return A list with count and detail data.frame.
points_in_polygon <- function(aoi, point_layer, fields_to_extract = NULL) {
  within <- sf::st_filter(point_layer, aoi, .predicate = sf::st_within)
  if (nrow(within) == 0) {
    return(NULL)
  }
  result <- sf::st_drop_geometry(within)
  total_points <- nrow(result)
  group_fields <- character(0)
  if (!is.null(fields_to_extract)) {
    group_fields <- fields_to_extract[fields_to_extract %in% names(result)]
  }

  if (length(group_fields) > 0) {
    result <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_fields))) |>
      dplyr::summarise(point_count = dplyr::n(), .groups = "drop")
  } else {
    result <- data.frame(point_count = total_points)
  }

  result <- attach_single_field_tables(
    result,
    sf::st_drop_geometry(within),
    group_fields,
    function(data, field) {
      data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(field))) |>
        dplyr::summarise(point_count = dplyr::n(), .groups = "drop")
    }
  )

  list(count = total_points, details = result)
}
