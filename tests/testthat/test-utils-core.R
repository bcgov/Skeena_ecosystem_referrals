testthat::test_that("extract_geomark_id parses supported inputs", {
  testthat::expect_equal(extract_geomark_id("gm-abc123"), "gm-abc123")
  testthat::expect_equal(
    extract_geomark_id("https://apps.gov.bc.ca/pub/geomark/geomarks/gm-a1b2c3"),
    "gm-a1b2c3"
  )
  testthat::expect_equal(extract_geomark_id("abc123"), "gm-abc123")
  testthat::expect_true(is.na(extract_geomark_id("")))
  testthat::expect_true(is.na(extract_geomark_id(NA_character_)))
})

testthat::test_that("build_geomark_feature_url builds expected endpoint", {
  testthat::expect_equal(
    build_geomark_feature_url("gm-xyz789"),
    "https://apps.gov.bc.ca/pub/geomark/geomarks/gm-xyz789/feature.geojson"
  )
  testthat::expect_true(is.na(build_geomark_feature_url("not a geomark url")))
})

testthat::test_that("normalise_geometry_type maps aliases", {
  testthat::expect_equal(normalise_geometry_type("MULTIPOLYGON"), "polygon")
  testthat::expect_equal(normalise_geometry_type("linestring"), "line")
  testthat::expect_equal(normalise_geometry_type("points"), "point")
  testthat::expect_equal(normalise_geometry_type("GeoTIFF"), "raster")
  testthat::expect_equal(normalise_geometry_type("weird"), "unknown")
})

testthat::test_that("parse_key_values keeps valid field tokens", {
  parsed <- parse_key_values("name, species; area_ha ; if overlap then show")
  testthat::expect_equal(parsed, c("name", "species", "area_ha"))
  testthat::expect_null(parse_key_values(""))
  testthat::expect_null(parse_key_values(NA_character_))
})

testthat::test_that("parse_distance_m handles km, m, bare numeric and default", {
  testthat::expect_equal(parse_distance_m("1 km"), 1000)
  testthat::expect_equal(parse_distance_m("250 m"), 250)
  testthat::expect_equal(parse_distance_m("42"), 42)
  testthat::expect_equal(parse_distance_m("bad", default_m = 99), 99)
  testthat::expect_equal(parse_distance_m("", default_m = 7), 7)
})

testthat::test_that("attach_single_field_tables skips split tables for large raw inputs", {
  grouped <- data.frame(group = "A", value = 1)
  raw <- data.frame(group = rep("A", 1501), other = rep("B", 1501), value = 1)

  out <- withr::with_options(
    list(referral.max_split_table_rows = 1000L),
    attach_single_field_tables(
      grouped_result = grouped,
      raw_result = raw,
      group_fields = c("group", "other"),
      summary_fun = function(data, field) data.frame(dummy = 1)
    )
  )

  testthat::expect_null(attr(out, "split_tables", exact = TRUE))
})

testthat::test_that("resolve_local_layer_path matches explicit and stem names", {
  td <- withr::local_tempdir()
  f1 <- file.path(td, "My_Layer.gpkg")
  f2 <- file.path(td, "Other.geojson")
  file.create(f1)
  file.create(f2)

  testthat::expect_equal(
    resolve_local_layer_path(f1, "ignored", search_dir = td),
    f1
  )

  testthat::expect_equal(
    resolve_local_layer_path("my_layer", "ignored", search_dir = td),
    f1
  )

  testthat::expect_true(is.na(resolve_local_layer_path("missing", "none", search_dir = td)))
})
