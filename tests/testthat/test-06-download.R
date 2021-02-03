context("content downloading functions")

# given the directory was created in test-03

main <- "./testing"

test_that("download works properly to get portal data", {

  # downloads take too long for cran checks

    skip_on_cran() 

  expect_message(download("PortalData", "zenodo", main = main,
                          concept_rec_id = "1215988"))
  expect_equal(list.files(raw_path(main)), "PortalData")
})


test_that("download_message handles messaging options", {

  #im having difficult with making the final flexible one match the 
  # message expector when it should and visually appears to, still doesnt tho

  expect_message(download_message("ok"), " -ok")
  expect_message(download_message("ok", verbose = TRUE), " -ok from")
  expect_message(download_message("ok", type = "zenodo"), " -ok")
  expect_message(download_message("ok", verbose = TRUE, type = "zenodo"))

})

test_that("download_url generates proper urls for downloading", {

  expect_equal(download_url(url = "https://www.12.com"), "https://www.12.com")
  expect_is(download_url(url = "ok", type = "zenodo", 
                         concept_rec_id = "1215988"), "character")
  expect_error(download_url(type = "ok"))

})

test_that("download_destin generates destinations", {

  expect_is(download_destin("ok", "http://test.com/123.csv"), "character")
  expect_is(download_destin("ok", "http://test.com/123"), "character")

})


test_that("zenodo_url retrieves the urls from zenodo", {

  # has the potential to hang

    skip_on_cran() 

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

  dl1 <- zenodo_downloads("1215988")
  expect_is(dl1, "list")
  expect_equal(length(dl1), 1)
  expect_equal(dl1[[1]]$concept_rec_id, "1215988")

  dl2 <- zenodo_downloads(c("1215988", "833438"))
  expect_is(dl2, "list")
  expect_equal(length(dl2), 2)
  expect_equal(dl2[[1]]$concept_rec_id, "1215988")
  expect_equal(dl2[[2]]$concept_rec_id, "833438")

  dl3 <- zenodo_downloads()
  expect_equal(dl3, NULL)

  dl4 <- zenodo_downloads(rec_id = "12345")
  expect_is(dl4, "list")
  expect_equal(length(dl4), 1)
  expect_equal(dl4[[1]]$rec_id, "12345")

})

test_that("NMME_urls constructs proper urls or errors accordingly", {

  uu <- NMME_urls()
  expect_is(uu, "character")
  expect_equal(length(uu), 4)
  expect_equal(sum(grepl("https", uu)), 4)

  expect_error(NMME_urls(start = rep(Sys.Date(), 2)))
  expect_error(NMME_urls(lat = rep(32, 2)))
  expect_error(NMME_urls(model = rep("ENSMEAN", 2)))
  expect_error(NMME_urls(freq = rep("daily", 2)))
  expect_error(NMME_urls(model = "ok"))
  expect_error(NMME_urls(freq = "ok"))
  expect_error(NMME_urls(data = "ok"))

})


