context("Test downloading functions")

main <- "./testing"

test_that("download works properly to get portal data", {
  skip_on_cran() # downloads take too long for cran checks
  expect_message(download("PortalData", "zenodo", main = main,
                          concept_rec_id = "1215988"))
})

test_that("download works properly to get archive", {
  skip_on_cran() # downloads take too long for cran checks
  expect_message(download("portalPredictions", "zenodo", main = main,
                          concept_rec_id = "833438"))
})

test_that("download_message handles messaging options", {
  expect_message(download_message("ok"))
  expect_message(download_message("ok", verbose = TRUE))
  expect_message(download_message("ok", type = "zenodo"))
  expect_message(download_message("ok", verbose = TRUE, type = "zenodo"))
})

test_that("download_url generates proper urls for downloading", {
  expect_equal(download_url(url = "https://www.12.com"), "https://www.12.com")
  expect_is(download_url(url = "ok", type = "zenodo", 
                         concept_rec_id = "1215988"), "character")
  expect_error(download_url(type = "ok"))
})

test_that("download_destin generates proper destinations", {
  expect_is(download_destin("ok", "http://test.com/123.csv"), "character")
  expect_is(download_destin("ok", "http://test.com/123"), "character")
})

test_that("zenodo_url retrieves the proper url from zenodo", {
  skip_on_cran() # has the potential to hang

  expect_is(zenodo_url("1215988", rec_version = "1.89.0"), "character")
  expect_warning(zenodo_url("1215988", rec_id = "1217163"))
  expect_is(zenodo_url(concept_rec_id = NULL, rec_id = "1217163", 
                       rec_version = NULL), "character")
  expect_error(zenodo_url("1215988", "12345678v1237"))
})

test_that("name_from_url parses a url appropriately", {
  url <- "https://zenodo.org/api/files/4f/weecology/PortalData-2.9.0.zip"
  expect_null(name_from_url(url, TRUE))
  expect_equal(name_from_url(url), "PortalData")
})

test_that("zenodo_downloads creates a proper download list", {
  expect_is(zenodo_downloads("1215988"), "list")
  expect_is(zenodo_downloads(c("1215988", "833438")), "list")
  expect_is(zenodo_downloads(rec_id = "12345"), "list")
})


test_that("NMME_urls constructs proper urls or errors accordingly", {
  expect_is(NMME_urls(), "character")
  expect_error(NMME_urls(start = rep(Sys.Date(), 2)))
  expect_error(NMME_urls(lat = rep(32, 2)))
  expect_error(NMME_urls(model = rep("ENSMEAN", 2)))
  expect_error(NMME_urls(freq = rep("daily", 2)))
  expect_error(NMME_urls(model = "ok"))
  expect_error(NMME_urls(freq = "ok"))
  expect_error(NMME_urls(data = "ok"))
})

