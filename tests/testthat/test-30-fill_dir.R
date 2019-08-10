context("Test filling functions")



test_that("fill_raw", {
  skip_on_cran() # downloads take too long for cran checks
  PP <- list(type = "zenodo", concept_rec_id = "833438")
  expect_equal(fill_raw(PP, main = "./testing"), NULL)
})

test_that("fill_predictions", {
  skip_on_cran() # downloads take too long for cran checks
  expect_equal(fill_predictions("portalPredictions/predictions", "./testing"),
               NULL)
  expect_message(fill_predictions("portalPredictions/predictions",
                 "./testing", verbose = TRUE))
})

test_that("fill_models", {
  expect_message(fill_models(main = "./testing"))
})

