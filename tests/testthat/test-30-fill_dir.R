context("Test filling functions")



test_that("fill_raw", {
  PP <- list(type = "zenodo", concept_rec_id = "833438")
  expect_equal(fill_raw(PP, main = "./testing"), NULL)
})

test_that("fill_predictions", {
  expect_equal(fill_predictions("portalPredictions/predictions", "./testing"),
               NULL)
  expect_message(fill_predictions("portalPredictions/predictions",
                 "./testing", verbose = TRUE))
})

test_that("fill_models", {
  expect_message(fill_models(main = "./testing"))
})

