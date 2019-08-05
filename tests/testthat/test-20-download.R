context("Test downloading functions")

create_dir(main = "testing")

test_that("download", {
  expect_message(download("PortalData", "zenodo", main = "testing",
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
  expect_warning(zenodo_url("1215988", rec_id = "1217163"))
})

test_that("record_name_from_url", {
  source_url <- zenodo_url(concept_rec_id = "1215988")
  expect_null(record_name_from_url(source_url, TRUE))
  expect_equal(record_name_from_url(source_url), "PortalData")
})

unlink(main_path(main = "testing"), recursive = TRUE, force = TRUE)
