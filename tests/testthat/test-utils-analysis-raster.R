testthat::test_that("raster_overlap returns NULL when AOI and raster do not overlap", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")

  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10, crs = "EPSG:3005")
  terra::values(r) <- 1

  aoi_far <- sf::st_as_sf(
    data.frame(id = 1, wkt = "POLYGON((20 20,20 30,30 30,30 20,20 20))"),
    wkt = "wkt",
    crs = 3005
  )

  testthat::expect_null(raster_overlap(aoi_far, r))
})

testthat::test_that("raster_overlap computes class percentages for overlapping AOI", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")

  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10, crs = "EPSG:3005")
  terra::values(r) <- rep(c(1, 2), each = 50)

  aoi <- sf::st_as_sf(
    data.frame(id = 1, wkt = "POLYGON((2 2,2 8,8 8,8 2,2 2))"),
    wkt = "wkt",
    crs = 3005
  )

  out <- raster_overlap(aoi, r)
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(all(c("value", "count", "pct_of_aoi") %in% names(out)))
  testthat::expect_equal(sum(out$pct_of_aoi), 100, tolerance = 1e-8)
})

testthat::test_that("line_raster_overlap returns NULL when AOI line and raster do not overlap", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")

  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10, crs = "EPSG:3005")
  terra::values(r) <- 1

  line_far <- sf::st_as_sf(
    data.frame(id = 1, wkt = "LINESTRING(20 20, 30 30)"),
    wkt = "wkt",
    crs = 3005
  )

  testthat::expect_null(line_raster_overlap(line_far, r))
})

testthat::test_that("line_raster_overlap computes overlap summary for intersecting line", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")

  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10, crs = "EPSG:3005")
  terra::values(r) <- 3

  line_aoi <- sf::st_as_sf(
    data.frame(id = 1, wkt = "LINESTRING(1 1, 9 9)"),
    wkt = "wkt",
    crs = 3005
  )

  out <- line_raster_overlap(line_aoi, r)
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(all(c("raster_value", "overlap_length_km", "pct_of_line") %in% names(out)))
  testthat::expect_equal(sum(out$pct_of_line), 100, tolerance = 1e-6)
})

testthat::test_that("build_layer_plot builds raster plots without aesthetic leakage", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("ggplot2")

  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10, crs = "EPSG:3005")
  terra::values(r) <- matrix(seq_len(100), nrow = 10, ncol = 10)

  aoi <- sf::st_as_sf(
    data.frame(id = 1, wkt = "POLYGON((1 1,1 9,9 9,9 1,1 1))"),
    wkt = "wkt",
    crs = 3005
  )

  plot_data <- prepare_raster_plot_data(r, aoi, outside_distance = 0)

  testthat::expect_type(plot_data, "list")
  plot_obj <- build_layer_plot(
    aoi = aoi,
    aoi_geom_type = "polygon",
    layer_display = "Test Raster",
    layer_geom = "raster",
    plot_raster = plot_data
  )

  testthat::expect_s3_class(plot_obj, "ggplot")
  testthat::expect_no_error(ggplot2::ggplot_build(plot_obj))
})
