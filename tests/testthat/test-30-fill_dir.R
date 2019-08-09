context("Test filling functions")

unlink(main_path(main = "./testing"), recursive = TRUE, force = TRUE)
create_dir(main = "./testing")

test_that("fill_predictions", {
  skip_on_cran()
  PP <- list(type = "zenodo", concept_rec_id = "833438")
  expect_equal(fill_raw(PP, main = "./testing"), NULL)
  expect_equal(fill_predictions("portalPredictions/predictions", "./testing"),
               NULL)
  expect_message(fill_predictions("portalPredictions/predictions",
                 "./testing", verbose = TRUE))
})

test_that("fill_models", {
  expect_message(fill_models(main = "./testing"))
})

unlink(main_path(main = "./testing"), recursive = TRUE, force = TRUE)
