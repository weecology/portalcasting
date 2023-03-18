context(desc = "path generating and manipulating functions")


test_that(desc = "file_ext extracts the file extension",
          code = {

  expect_equal(file_ext("home/folders.with.dots/stuff/ok.csv"), "csv")
  expect_equal(file_ext(NMME_urls()[[1]]), "")
  expect_equal(file_ext(NMME_urls()[[1]], "="), "csv")

})
