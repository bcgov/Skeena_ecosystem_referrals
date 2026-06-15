# =============================================================================
# Report Helper Functions
# =============================================================================

new_render_silent <- function() {
  structure(list(), class = "render_silent")
}

print.render_silent <- function(x, ...) {
  invisible(x)
}

#' Create a formatted analysis result
#'
#' Wraps analysis output in a standardised list structure for reporting.
#'
#' @param layer_name Name of the reference layer.
#' @param analysis_type Type of analysis performed.
#' @param result The analysis result (data.frame or list).
#' @param has_overlap Logical, whether any overlap was found.
#' @return A named list.
format_result <- function(layer_name, analysis_type, result, has_overlap) {
  list(
    layer_name = layer_name,
    analysis_type = analysis_type,
    has_overlap = has_overlap,
    result = result
  )
}

#' Render a result table for the report
#'
#' Produces a nicely formatted kable table from an analysis result.
#'
#' @param result A data.frame to render.
#' @param caption Table caption.
#' @param collapsible Logical; wrap the table in a details block.
#' @return A silent token so implicit print() emits nothing.
render_table <- function(result, caption = NULL, collapsible = FALSE) {
  if (is.null(result) || (is.data.frame(result) && nrow(result) == 0)) {
    return(new_render_silent())
  }

  total_rows <- if (is.data.frame(result)) nrow(result) else NA_integer_
  max_rows <- getOption("referral.max_table_rows", Inf)
  truncated <- FALSE
  if (is.data.frame(result) && is.finite(max_rows) && max_rows > 0 && nrow(result) > max_rows) {
    result <- utils::head(result, max_rows)
    truncated <- TRUE
  }

  format_column_name <- function(col_name) {
    lookup <- c(
      overlap_area_ha = "Overlap Area (ha)",
      overlap_pct = "Overlap (%)",
      overlap_length_km = "Overlap Length (km)",
      total_length_km = "Total Length (km)",
      density_km_per_km2 = "Density (km per km2)",
      point_count = "Point Count",
      intersection_count = "Intersection Count",
      feature_count = "Feature Count",
      min_distance_m = "Minimum Distance (m)",
      mean_distance_m = "Mean Distance (m)",
      max_distance_m = "Maximum Distance (m)",
      distance_m = "Distance (m)",
      pct_of_aoi = "Percent of AOI (%)",
      pct_of_line = "Percent of Line (%)",
      raster_value = "Raster Value",
      outside_distance_m = "Outside Distance (m)",
      direct_hit = "Direct Hit",
      nearby_hit = "Nearby Hit",
      spatial_request = "Requested Spatial Information"
    )

    if (col_name %in% names(lookup)) {
      return(unname(lookup[[col_name]]))
    }

    parts <- strsplit(gsub("[.]+", "_", col_name), "_", fixed = FALSE)[[1]]
    parts <- parts[nzchar(parts)]
    parts <- vapply(parts, function(p) {
      if (p %in% c("aoi", "id", "bcdc")) {
        toupper(p)
      } else {
        tools::toTitleCase(p)
      }
    }, character(1))
    paste(parts, collapse = " ")
  }

  num_cols <- sapply(result, is.numeric)
  result[num_cols] <- lapply(result[num_cols], round, digits = 2)
  names(result) <- vapply(names(result), format_column_name, character(1))

  kable_output <- knitr::kable(result, caption = caption, format = "pipe")
  table_markdown <- paste(as.character(kable_output), collapse = "\n")

  if (isTRUE(collapsible)) {
    summary_text <- sprintf("%s (%d rows)", caption %||% "Table", total_rows %||% nrow(result))
    table_markdown <- paste0(
      "<details><summary>", summary_text, "</summary>\n\n",
      table_markdown,
      "\n\n</details>\n\n"
    )
  } else {
    table_markdown <- paste0(table_markdown, "\n")
  }

  if (truncated) {
    table_markdown <- paste0(
      table_markdown,
      sprintf("\n*Showing first %d of %d rows.*\n", nrow(result), total_rows)
    )
  }

  cat(table_markdown)
  new_render_silent()
}

format_field_label <- function(field_name) {
  tools::toTitleCase(gsub("_", " ", field_name))
}

render_grouped_tables <- function(result, caption_prefix = NULL) {
  if (is.null(result) || (is.data.frame(result) && nrow(result) == 0)) {
    return(invisible(NULL))
  }

  split_tables <- attr(result, "split_tables", exact = TRUE)
  if (is.null(split_tables)) {
    render_table(result, caption = caption_prefix, collapsible = TRUE)
    return(invisible(NULL))
  }

  for (field in names(split_tables)) {
    render_table(
      split_tables[[field]],
      caption = paste(caption_prefix, sprintf("- By %s", format_field_label(field))),
      collapsible = TRUE
    )
  }

  invisible(NULL)
}

crop_to_aoi_context <- function(layer, aoi, pad_dist = 500) {
  if (is.null(layer) || nrow(layer) == 0) {
    return(NULL)
  }

  aoi_bbox <- sf::st_bbox(aoi)
  expanded_bbox <- aoi_bbox
  expanded_bbox[["xmin"]] <- expanded_bbox[["xmin"]] - pad_dist
  expanded_bbox[["xmax"]] <- expanded_bbox[["xmax"]] + pad_dist
  expanded_bbox[["ymin"]] <- expanded_bbox[["ymin"]] - pad_dist
  expanded_bbox[["ymax"]] <- expanded_bbox[["ymax"]] + pad_dist

  cropped <- suppressWarnings(sf::st_crop(layer, expanded_bbox))
  if (nrow(cropped) == 0) {
    return(layer)
  }
  cropped
}

prepare_raster_plot_data <- function(raster_layer, aoi, outside_distance = NA_real_) {
  aoi_proj <- sf::st_transform(aoi, terra::crs(raster_layer))
  aoi_bbox <- sf::st_bbox(aoi_proj)
  pad_dist <- if (is.na(outside_distance) || outside_distance <= 0) 500 else outside_distance
  plot_extent <- terra::ext(
    aoi_bbox[["xmin"]] - pad_dist,
    aoi_bbox[["xmax"]] + pad_dist,
    aoi_bbox[["ymin"]] - pad_dist,
    aoi_bbox[["ymax"]] + pad_dist
  )

  cropped <- tryCatch(
    terra::crop(raster_layer, plot_extent),
    error = function(e) NULL
  )
  if (is.null(cropped)) {
    return(NULL)
  }

  raster_values <- tryCatch(terra::values(cropped), error = function(e) NULL)
  if (is.null(raster_values) || all(is.na(raster_values))) {
    return(NULL)
  }

  list(raster = cropped, aoi = aoi_proj)
}

# =============================================================================
# Interactive Leaflet Map Helpers
# =============================================================================

#' Build hover-tooltip labels from sf object attributes
#'
#' Concatenates non-geometry column names and values as HTML for use as
#' leaflet pop-up / tooltip labels.
#'
#' @param sf_obj An sf object.
#' @return Character vector of HTML strings, one per row.
make_feature_labels <- function(sf_obj) {
  geom_col <- attr(sf_obj, "sf_column")
  col_names <- setdiff(names(sf_obj), geom_col)
  if (length(col_names) == 0) {
    return(rep("Feature", nrow(sf_obj)))
  }
  data_df <- sf::st_drop_geometry(sf_obj)[, col_names, drop = FALSE]
  apply(data_df, 1, function(row) {
    paste(paste0("<b>", col_names, ":</b> ", row), collapse = "<br>")
  })
}

render_leaflet_widget <- function(widget) {
  if (is.null(widget)) {
    return(invisible(NULL))
  }

  rendered <- knitr::knit_print(htmltools::tagList(widget))
  knit_meta <- attr(rendered, "knit_meta", exact = TRUE)
  if (!is.null(knit_meta)) {
    knitr::knit_meta_add(knit_meta)
  }
  cat(as.character(rendered))
  invisible(NULL)
}

#' Build an interactive AOI overview map with Skeena region context
#'
#' Returns a leaflet map initially fitted to the Skeena Natural Resource Region
#' with the AOI polygon overlaid.
#'
#' @param aoi An sf object (BC Albers EPSG:3005).
#' @return A leaflet htmlwidget.
build_aoi_leaflet <- function(aoi) {
  aoi_wgs84 <- sf::st_transform(aoi, 4326)

  # Skeena NR Region approximate extent (WGS84)
  skeena_bounds <- list(
    lat1 = 53.5, lat2 = 59.5,
    lng1 = -135.0, lng2 = -121.0
  )

  leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Basemap") |>
    leaflet::fitBounds(
      lng1 = skeena_bounds$lng1, lat1 = skeena_bounds$lat1,
      lng2 = skeena_bounds$lng2, lat2 = skeena_bounds$lat2
    ) |>
    leaflet::addPolygons(
      data = aoi_wgs84,
      fillColor = "steelblue",
      fillOpacity = 0.35,
      color = "navy",
      weight = 2,
      label = htmltools::HTML("<b>Area of Interest</b>"),
      highlightOptions = leaflet::highlightOptions(fillOpacity = 0.6, weight = 3),
      group = "AOI"
    ) |>
    leaflet::addLayersControl(
      overlayGroups = "AOI",
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addScaleBar(position = "bottomleft")
}

#' Build an interactive per-layer leaflet map with hover tooltips
#'
#' Renders the layer features and the AOI as a leaflet widget. Vector features
#' show hover tooltips with all extracted attribute fields. Raster layers are
#' rendered via the raster package if available.
#'
#' @param aoi sf object (BC Albers).
#' @param aoi_geom_type Character: "polygon" or "line".
#' @param layer_display Character: human-readable layer name for labels.
#' @param layer_geom Character: "polygon", "line", "point", or "raster".
#' @param plot_vector sf object with features to display (NULL = none).
#' @param plot_raster List with $raster (terra SpatRaster) and $aoi; from
#'   prepare_raster_plot_data(). NULL = none.
#' @return A leaflet htmlwidget, or NULL if nothing to show.
build_layer_leaflet <- function(aoi, aoi_geom_type, layer_display, layer_geom,
                                plot_vector = NULL, plot_raster = NULL) {
  aoi_wgs84 <- sf::st_transform(aoi, 4326)
  aoi_bbox <- sf::st_bbox(aoi_wgs84)

  has_features <- (!is.null(plot_raster)) ||
    (!is.null(plot_vector) && nrow(plot_vector) > 0)

  if (!has_features) {
    return(NULL)
  }

  lmap <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      data = aoi_wgs84,
      fillColor = if (aoi_geom_type == "polygon") "#2b8cbe" else "transparent",
      fillOpacity = if (aoi_geom_type == "polygon") 0.12 else 0,
      color = "navy",
      weight = 2.5,
      label = htmltools::HTML("<b>Area of Interest</b>"),
      highlightOptions = leaflet::highlightOptions(fillOpacity = 0.3, weight = 3),
      group = "AOI"
    )

  if (!is.null(plot_raster)) {
    tryCatch(
      {
        if (requireNamespace("raster", quietly = TRUE)) {
          r <- raster::raster(plot_raster$raster)
          r_vals <- raster::values(r)
          r_vals <- r_vals[!is.na(r_vals)]
          if (length(r_vals) > 0) {
            pal <- leaflet::colorNumeric(
              "viridis",
              domain = range(r_vals, na.rm = TRUE),
              na.color = "transparent"
            )
            lmap <- lmap |>
              leaflet::addRasterImage(
                raster::projectRaster(r, crs = raster::crs("+proj=longlat +datum=WGS84")),
                colors = pal,
                opacity = 0.7,
                group = layer_display
              ) |>
              leaflet::addLegend(
                pal = pal,
                values = r_vals,
                title = layer_display,
                position = "bottomright"
              )
          }
        }
      },
      error = function(e) NULL
    )
  } else if (!is.null(plot_vector) && nrow(plot_vector) > 0) {
    vec_wgs84 <- sf::st_transform(sf::st_make_valid(plot_vector), 4326)
    labels <- lapply(make_feature_labels(vec_wgs84), htmltools::HTML)
    hi_opts <- leaflet::highlightOptions(fillOpacity = 0.75, weight = 3, bringToFront = TRUE)

    if (layer_geom == "polygon") {
      lmap <- lmap |>
        leaflet::addPolygons(
          data = vec_wgs84,
          fillColor = "#d95f02",
          fillOpacity = 0.40,
          color = "#8c2d04",
          weight = 1.5,
          label = labels,
          highlightOptions = hi_opts,
          group = layer_display
        )
    } else if (layer_geom == "line") {
      lmap <- lmap |>
        leaflet::addPolylines(
          data = vec_wgs84,
          color = "#d95f02",
          weight = 2.5,
          opacity = 0.9,
          label = labels,
          highlightOptions = leaflet::highlightOptions(weight = 4.5, color = "#8c2d04"),
          group = layer_display
        )
    } else if (layer_geom == "point") {
      lmap <- lmap |>
        leaflet::addCircleMarkers(
          data = vec_wgs84,
          radius = 7,
          color = "#8c2d04",
          fillColor = "#d95f02",
          fillOpacity = 0.8,
          weight = 1.5,
          label = labels,
          group = layer_display
        )
    }
  }

  layer_groups <- if (!is.null(plot_raster)) c("AOI", layer_display) else c("AOI", layer_display)

  lmap |>
    leaflet::fitBounds(
      lng1 = as.numeric(aoi_bbox["xmin"]),
      lat1 = as.numeric(aoi_bbox["ymin"]),
      lng2 = as.numeric(aoi_bbox["xmax"]),
      lat2 = as.numeric(aoi_bbox["ymax"])
    ) |>
    leaflet::addLayersControl(
      overlayGroups = layer_groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addScaleBar(position = "bottomleft")
}

build_layer_plot <- function(aoi, aoi_geom_type, layer_display, layer_geom,
                             plot_vector = NULL, plot_raster = NULL) {
  if (!is.null(plot_raster)) {
    raster_df <- terra::as.data.frame(plot_raster$raster, xy = TRUE, na.rm = TRUE)
    if (nrow(raster_df) == 0) {
      return(NULL)
    }

    x <- y <- value <- NULL

    value_col <- names(raster_df)[3]
    names(raster_df)[3] <- "value"
    unique_values <- unique(raster_df$value)

    aoi_native <- sf::st_transform(plot_raster$aoi, sf::st_crs(plot_raster$raster))

    plot_obj <- ggplot2::ggplot()
    if (is.numeric(raster_df$value) && length(unique_values) > 20) {
      plot_obj <- plot_obj +
        ggplot2::geom_raster(
          data = raster_df,
          ggplot2::aes(x = x, y = y, fill = value)
        ) +
        ggplot2::scale_fill_viridis_c(name = value_col)
    } else {
      raster_df$value <- as.factor(raster_df$value)
      plot_obj <- plot_obj +
        ggplot2::geom_raster(
          data = raster_df,
          ggplot2::aes(x = x, y = y, fill = value)
        ) +
        ggplot2::scale_fill_brewer(palette = "Set3", name = value_col)
    }

    return(
      plot_obj +
        ggplot2::geom_sf(
          data = aoi_native,
          fill = if (aoi_geom_type == "polygon") NA else NULL,
          colour = "navy",
          linewidth = 0.9,
          inherit.aes = FALSE
        ) +
        ggplot2::coord_sf(expand = FALSE) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste(layer_display, "Analysis Map"),
          subtitle = "AOI over analysis layer"
        )
    )
  }

  if (is.null(plot_vector) || nrow(plot_vector) == 0) {
    return(NULL)
  }

  plot_vector <- sf::st_make_valid(plot_vector)
  plot_obj <- ggplot2::ggplot()
  if (layer_geom == "polygon") {
    plot_obj <- plot_obj +
      ggplot2::geom_sf(data = plot_vector, fill = "#d95f02", alpha = 0.45, colour = "#8c2d04")
  } else if (layer_geom == "line") {
    plot_obj <- plot_obj +
      ggplot2::geom_sf(data = plot_vector, colour = "#d95f02", linewidth = 0.8, alpha = 0.9)
  } else if (layer_geom == "point") {
    plot_obj <- plot_obj +
      ggplot2::geom_sf(data = plot_vector, colour = "#d95f02", size = 2.2, alpha = 0.9)
  }

  plot_obj +
    ggplot2::geom_sf(
      data = aoi,
      fill = if (aoi_geom_type == "polygon") "#2b8cbe" else NA,
      alpha = if (aoi_geom_type == "polygon") 0.2 else 1,
      colour = "navy",
      linewidth = 1
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste(layer_display, "Analysis Map"),
      subtitle = "AOI over analysis layer"
    )
}
