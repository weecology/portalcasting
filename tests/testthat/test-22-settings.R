context(desc = "settings functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "directory_settings produces a list", {

  expect_silent(ds <- directory_settings())
  expect_is(ds, "list")
  expect_equal(names(ds), c("files", "subdirectories", "resources", "repository", "confidence_level", "nsamples", "time", "save", "force", "overwrite", "unzip_pause", "download_timeout"))

  expect_silent(ps <- production_settings())
  expect_is(ps, "list")
  expect_equal(names(ps), c("files", "subdirectories", "resources", "repository", "confidence_level", "nsamples", "time", "save", "force", "overwrite", "unzip_pause", "download_timeout"))

  expect_silent(ss <- sandbox_settings())
  expect_is(ss, "list")
  expect_equal(names(ss), c("files", "subdirectories", "resources", "repository", "confidence_level", "nsamples", "time", "save", "force", "overwrite", "unzip_pause", "download_timeout"))

})

test_that(desc = "directory_resources produces a list", {

  expect_silent(dr <- directory_resources())
  expect_is(dr, "list")
  expect_equal(names(dr), c("PortalData", "portalPredictions", "climate_forecasts"))

})

test_that(desc = "directory_subdirectories produces a list", {

  expect_silent(ds <- directory_subdirectories())
  expect_is(ds, "list")
  expect_equal(sort(names(ds)), sort(c("forecasts", "fits", "models", "resources", "data", "www")))

})

test_that(desc = "time_settings produces a list", {

  expect_silent(ts <- time_settings())
  expect_is(ts, "list")
  expect_equal(names(ts), c("timeseries_start", "timeseries_start_lagged", "forecast_start", "forecast_end", "forecast_end_buffered", "origin", "forecast_date", "lead_time", "lead_time_buffer", "max_lag", "lag_buffer"))

})


