testthat::test_that("format_result returns standard structure", {
  out <- format_result("Layer 1", "polygon", data.frame(x = 1), TRUE)
  testthat::expect_named(out, c("layer_name", "analysis_type", "has_overlap", "result"))
  testthat::expect_equal(out$layer_name, "Layer 1")
  testthat::expect_true(out$has_overlap)
})

testthat::test_that("format_field_label title-cases underscore names", {
  testthat::expect_equal(format_field_label("outside_distance_m"), "Outside Distance m")
})

testthat::test_that("render_table returns silent object and emits markdown", {
  df <- data.frame(overlap_area_ha = 1.234, spatial_request = "Yes")
  output <- paste(capture.output(res <- render_table(df, caption = "Test Table", collapsible = TRUE)), collapse = "\n")

  testthat::expect_s3_class(res, "render_silent")
  testthat::expect_match(output, "<details><summary>Test Table \\(1 rows\\)</summary>")
  testthat::expect_match(output, "Overlap Area \\(ha\\)")
  testthat::expect_match(output, "1\\.23")
})

testthat::test_that("render_grouped_tables handles plain and split-table inputs", {
  plain <- data.frame(value = 1)
  out_plain <- capture.output(render_grouped_tables(plain, caption_prefix = "Plain"))
  testthat::expect_true(length(out_plain) > 0)

  grouped <- data.frame(value = c(1, 2))
  attr(grouped, "split_tables") <- list(
    field_one = data.frame(point_count = 2),
    field_two = data.frame(feature_count = 3)
  )
  out_split <- paste(capture.output(render_grouped_tables(grouped, caption_prefix = "Grouped")), collapse = "\n")
  testthat::expect_match(out_split, "By Field One")
  testthat::expect_match(out_split, "By Field Two")
})

testthat::test_that("make_feature_labels supports attributed and bare sf objects", {
  testthat::skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(name = "A", value = 2, x = 0, y = 0),
    coords = c("x", "y"),
    crs = 3005
  )
  labels <- make_feature_labels(pts)
  testthat::expect_length(labels, 1)
  testthat::expect_match(labels[[1]], "<b>name:</b> A")

  geom_only <- sf::st_sf(geometry = sf::st_geometry(pts))
  fallback <- make_feature_labels(geom_only)
  testthat::expect_equal(fallback, "Feature")
})

testthat::test_that("crop_to_aoi_context handles empty and non-overlap cases", {
  testthat::skip_if_not_installed("sf")

  testthat::expect_null(crop_to_aoi_context(NULL, NULL))

  aoi <- sf::st_as_sf(
    data.frame(id = 1, wkt = "POLYGON((0 0,0 10,10 10,10 0,0 0))"),
    wkt = "wkt",
    crs = 3005
  )
  far_layer <- sf::st_as_sf(
    data.frame(id = 1, wkt = "POLYGON((100 100,100 110,110 110,110 100,100 100))"),
    wkt = "wkt",
    crs = 3005
  )

  cropped <- crop_to_aoi_context(far_layer, aoi, pad_dist = 0)
  testthat::expect_equal(nrow(cropped), nrow(far_layer))
})

testthat::test_that("prepare_raster_plot_data returns cropped raster when values exist", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")

  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 5, ymin = 0, ymax = 5, crs = "EPSG:3005")
  terra::values(r) <- 1:25

  aoi <- sf::st_as_sf(
    data.frame(id = 1, wkt = "POLYGON((1 1,1 3,3 3,3 1,1 1))"),
    wkt = "wkt",
    crs = 3005
  )

  out <- prepare_raster_plot_data(r, aoi, outside_distance = 0)
  testthat::expect_type(out, "list")
  testthat::expect_true(all(c("raster", "aoi") %in% names(out)))

  r_na <- terra::rast(r)
  terra::values(r_na) <- NA_real_
  testthat::expect_null(prepare_raster_plot_data(r_na, aoi, outside_distance = 0))
})
