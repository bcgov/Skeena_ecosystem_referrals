testthat::test_that("quote_sql_ident escapes embedded quotes", {
  testthat::expect_equal(quote_sql_ident("abc"), '"abc"')
  testthat::expect_equal(quote_sql_ident('a"b'), '"a""b"')
})

testthat::test_that("layer cache helpers produce deterministic names and paths", {
  testthat::expect_equal(layer_cache_stem("My Layer (v1)"), "My_Layer_v1")
  testthat::expect_equal(layer_cache_stem("", layer_identifier = "id-layer"), "id-layer")
  testthat::expect_equal(layer_cache_stem("", layer_identifier = "", fallback = "fallback"), "fallback")

  testthat::expect_equal(normalise_layer_key("  My-Layer_Name "), "mylayername")
  testthat::expect_equal(cache_extension_for_geometry("raster"), "tif")
  testthat::expect_equal(cache_extension_for_geometry("polygon"), "gpkg")

  expected <- file.path("cache", "Layer_A.gpkg")
  actual <- layer_cache_path(
    layer_name = "Layer A",
    geometry_type = "polygon",
    search_dir = "cache"
  )
  testthat::expect_equal(actual, expected)
})

testthat::test_that("resolve_layer_path_for_config prefers deterministic cache files", {
  testthat::skip_if_not_installed("sf")

  td <- withr::local_tempdir()
  expected <- file.path(td, "Test_Layer.gpkg")
  layer <- sf::st_as_sf(
    data.frame(id = 1, x = 0, y = 0),
    coords = c("x", "y"),
    crs = 3005
  )
  suppressWarnings(sf::st_write(layer, expected, quiet = TRUE, delete_dsn = TRUE))

  resolved <- resolve_layer_path_for_config(
    layer_identifier = "id",
    layer_name = "Test Layer",
    geometry_type = "polygon",
    search_dir = td
  )
  testthat::expect_equal(resolved, expected)
})
