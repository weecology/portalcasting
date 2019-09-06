context("Test filling functions")



test_that("fill_raw", {
  skip_on_cran() # downloads take too long for cran checks
  PP <- list(type = "zenodo", concept_rec_id = "833438")
  expect_equal(fill_raw(PP, main = "./testing"), NULL)
})

test_that("fill_data", {
  skip_on_cran() # downloads take too long for cran checks

# remove raw to ensure that it can do it itself 

unlink(list.files(sub_path(main = "./testing", "raw"), full.names = TRUE))
  expect_equal(fill_data(main = "./testing"), NULL)

})

test_that("fill_casts", {
  skip_on_cran() # downloads take too long for cran checks
  expect_equal(fill_casts("./testing"), NULL)
  expect_message(fill_casts("./testing", verbose = TRUE))
})

test_that("fill_casts_message", {
  expect_message(fill_casts_message("x", FALSE, verbose = TRUE))
})


test_that("fill_models", {
  expect_message(fill_models(main = "./testing"))
})

