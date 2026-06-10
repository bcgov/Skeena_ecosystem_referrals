# # =============================================================================
# # Specialised Layer Analysis Functions
# # =============================================================================

# #' Summarise Core Ecosystem layer overlap with AOI
# #'
# #' For CE layers (which always overlap), reports percent of each Analysis Unit
# #' (AU) that overlaps and the CE concerns for each AU.
# #'
# #' @param aoi An sf polygon object.
# #' @param ce_layer An sf polygon object with AU_NAME and CONCERN columns.
# #' @param fields_to_extract Character vector of field names to include.
# #' @return A data.frame with AU-level overlap summaries.
# ce_overlap_summary <- function(aoi, ce_layer, fields_to_extract = NULL) {
#   intersection <- sf::st_intersection(ce_layer, sf::st_union(aoi))
#   if (nrow(intersection) == 0) {
#     return(NULL)
#   }
#   aoi_area <- as.numeric(sf::st_area(sf::st_union(aoi)))
#   intersection$overlap_area_m2 <- as.numeric(sf::st_area(intersection))
#   intersection$au_pct_of_aoi <- (intersection$overlap_area_m2 / aoi_area) * 100

#   result <- sf::st_drop_geometry(intersection)
#   cols_to_keep <- c("au_pct_of_aoi")
#   if (!is.null(fields_to_extract)) {
#     available <- fields_to_extract[fields_to_extract %in% names(result)]
#     cols_to_keep <- c(available, cols_to_keep)
#   }
#   result <- result[, cols_to_keep, drop = FALSE]
#   result
# }

# #' Summarise Wildlife Habitat Rating suitability classes within AOI
# #'
# #' Calculates the percent of each WHR class within the AOI polygon.
# #'
# #' @param aoi An sf polygon object.
# #' @param whr_layer An sf polygon object with a WHR_CLASS column.
# #' @param fields_to_extract Character vector of field names to include.
# #' @return A data.frame with WHR class and percent of AOI.
# whr_suitability_summary <- function(aoi, whr_layer, fields_to_extract = NULL) {
#   intersection <- sf::st_intersection(whr_layer, sf::st_union(aoi))
#   if (nrow(intersection) == 0) {
#     return(NULL)
#   }
#   aoi_area <- as.numeric(sf::st_area(sf::st_union(aoi)))
#   intersection$area_m2 <- as.numeric(sf::st_area(intersection))

#   if ("WHR_CLASS" %in% names(intersection)) {
#     summary_df <- intersection |>
#       sf::st_drop_geometry()
#     summary_df <- stats::aggregate(area_m2 ~ WHR_CLASS, data = summary_df, FUN = sum)
#     summary_df$total_area_ha <- summary_df$area_m2 / 10000
#     summary_df$pct_of_aoi <- summary_df$area_m2 / aoi_area * 100
#     summary_df$area_m2 <- NULL
#   } else {
#     summary_df <- sf::st_drop_geometry(intersection)
#     summary_df$pct_of_aoi <- (summary_df$area_m2 / aoi_area) * 100
#   }

#   if (!is.null(fields_to_extract)) {
#     available <- fields_to_extract[fields_to_extract %in% names(summary_df)]
#     extra <- c(available, "total_area_ha", "pct_of_aoi")
#     extra <- extra[extra %in% names(summary_df)]
#     summary_df <- summary_df[, extra, drop = FALSE]
#   }
#   summary_df
# }
