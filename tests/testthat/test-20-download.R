context("Test downloading functions")

create_dir(main = "./testing")

test_that("download", {
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
  expect_warning(zenodo_url("1215988", rec_id = "1217163"))
})

test_that("record_name_from_url", {
  source_url <- zenodo_url(concept_rec_id = "1215988")
  expect_null(record_name_from_url(source_url, TRUE))
  expect_equal(record_name_from_url(source_url), "PortalData")
})

test_that("zenodo_downloads", {
  expect_is(zenodo_downloads(c("1215988", "833438")), "list")
  expect_is(zenodo_downloads(rec_id = "12345"), "list")
})
raw_path_data = "PortalData"
 main = "./testing"

  lpath <- paste0("raw/", raw_path_data)
  full <- file_paths(main, lpath) 
print(full)


print(verify_raw_data(main = "./testing"))
test_that("verify_raw_data", {
  expect_equal(verify_raw_data(main = "./testing"), TRUE)
})

unlink(main_path(main = "./testing"), recursive = TRUE, force = TRUE)
