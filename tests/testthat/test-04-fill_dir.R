context("Test filling functions")



test_that("fill_raw", {
  skip_on_cran() # downloads take too long for cran checks
  PP <- list(type = "zenodo", concept_rec_id = "833438")
  expect_equal(fill_raw(PP, main = "./testing"), NULL)
})

test_that("fill_data", {
  skip_on_cran() # downloads take too long for cran checks

# remove raw to ensure that it can do it itself 

unlink(list.files(sub_paths(main = "./testing", "raw"), full.names = TRUE))
  expect_equal(fill_data(main = "./testing"), NULL)

})

test_that("fill_predictions", {
  skip_on_cran() # downloads take too long for cran checks
  expect_equal(fill_predictions("./testing", "portalPredictions/predictions"),
               NULL)
  expect_message(fill_predictions("./testing", 
                   "portalPredictions/predictions",
                   verbose = TRUE))
})

test_that("fill_predictions_message", {
  expect_is(fill_predictions_message("x", FALSE, verbose = TRUE), "character")
})


test_that("fill_models", {
  expect_message(fill_models(main = "./testing"))
})

