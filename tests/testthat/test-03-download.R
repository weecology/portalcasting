context("Test downloading functions")



test_that("download", {
  skip_on_cran() # downloads take too long for cran checks
  expect_message(download("PortalData", "zenodo", main = "./testing",
                          concept_rec_id = "1215988"))
})

test_that("download_message", {
  expect_message(download_message("ok"))
})

test_that("download_url", {
  expect_equal(download_url(url="ok"), "ok")
  expect_error(download_url(type="ok"))
})

test_that("download_destin", {
  expect_is(download_destin("ok", "http://test.com/123.csv"), "character")
})

test_that("zenodo_url", {
  skip_on_cran() # could hang?
  expect_warning(zenodo_url("1215988", rec_id = "1217163"))
  expect_is(zenodo_url(concept_rec_id = NULL, rec_id = "1217163", 
                       rec_version = NULL), "character")
  expect_error(zenodo_url("1215988", "12345"))
})

test_that("name_from_url", {
  skip_on_cran() # downloads take too long for cran checks
  source_url <- zenodo_url(concept_rec_id = "1215988")
  expect_null(name_from_url(source_url, TRUE))
  expect_equal(name_from_url(source_url), "PortalData")
})

test_that("zenodo_downloads", {
  expect_is(zenodo_downloads(c("1215988", "833438")), "list")
  expect_is(zenodo_downloads(rec_id = "12345"), "list")
})

test_that("verify_raw_data", {
  skip_on_cran() # downloads take too long for cran checks
  expect_equal(verify_raw_data(main = "./testing"), TRUE)
})

test_that("NMME_urls", {
  expect_is(NMME_urls(), "character")
  expect_error(NMME_urls(start = rep(Sys.Date(), 2)))
  expect_error(NMME_urls(lat = rep(32, 2)))
  expect_error(NMME_urls(model = rep("ENSMEAN", 2)))
  expect_error(NMME_urls(freq = rep("daily", 2)))
  expect_error(NMME_urls(model = "ok"))
  expect_error(NMME_urls(freq = "ok"))
  expect_error(NMME_urls(data = "ok"))
})

