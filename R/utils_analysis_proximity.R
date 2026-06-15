# =============================================================================
# Proximity Analysis Functions
# =============================================================================

attach_single_field_tables <- get0(
  "attach_single_field_tables",
  mode = "function",
  ifnotfound = function(grouped_result, raw_result, group_fields, summary_fun) grouped_result
)

#' Find features within a buffer distance of the AOI
#'
#' Buffers the AOI by the given distance and returns features from the
#' reference layer that fall within the buffer, along with their distance.
#'
#' @param aoi An sf object (polygon or line).
#' @param ref_layer An sf object (point or polygon).
#' @param buffer_dist_m Buffer distance in metres (default 1000).
#' @param fields_to_extract Character vector of field names to include.
#' @return A data.frame with distance (m) and extracted fields, or NULL.
features_within_buffer <- function(aoi, ref_layer, buffer_dist_m = 1000,
                                   fields_to_extract = NULL) {
  aoi_buffer <- sf::st_buffer(sf::st_union(aoi), dist = buffer_dist_m)
  aoi_union <- sf::st_union(aoi)
  nearby <- sf::st_filter(ref_layer, aoi_buffer, .predicate = sf::st_intersects)

  if (nrow(nearby) == 0) {
    return(NULL)
  }

  intersects_mat <- sf::st_intersects(nearby, aoi_union, sparse = FALSE)
  inside_mask <- rep(FALSE, nrow(nearby))
  if (is.matrix(intersects_mat) && ncol(intersects_mat) >= 1) {
    inside_mask <- as.logical(intersects_mat[, 1, drop = TRUE])
    inside_mask[is.na(inside_mask)] <- FALSE
  }
  inside_idx <- which(inside_mask)
  if (length(inside_idx) > 0) {
    nearby <- nearby[-inside_idx, ]
  }
  if (nrow(nearby) == 0) {
    return(NULL)
  }
  nearby$distance_m <- as.numeric(sf::st_distance(nearby, aoi_union)[, 1])
  result <- sf::st_drop_geometry(nearby)
  group_fields <- character(0)
  if (!is.null(fields_to_extract)) {
    group_fields <- fields_to_extract[fields_to_extract %in% names(result)]
  }

  if (length(group_fields) > 0) {
    result <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_fields))) |>
      dplyr::summarise(
        feature_count = dplyr::n(),
        min_distance_m = min(.data$distance_m, na.rm = TRUE),
        mean_distance_m = mean(.data$distance_m, na.rm = TRUE),
        max_distance_m = max(.data$distance_m, na.rm = TRUE),
        .groups = "drop"
      )
    result <- result[order(result$min_distance_m), , drop = FALSE]
  } else {
    result <- result[, "distance_m", drop = FALSE]
    result <- result[order(result$distance_m), , drop = FALSE]
  }

  result <- attach_single_field_tables(
    result,
    sf::st_drop_geometry(nearby),
    group_fields,
    function(data, field) {
      split <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(field))) |>
        dplyr::summarise(
          feature_count = dplyr::n(),
          min_distance_m = min(.data$distance_m, na.rm = TRUE),
          mean_distance_m = mean(.data$distance_m, na.rm = TRUE),
          max_distance_m = max(.data$distance_m, na.rm = TRUE),
          .groups = "drop"
        )
      split[order(split$min_distance_m), , drop = FALSE]
    }
  )

  result
}
