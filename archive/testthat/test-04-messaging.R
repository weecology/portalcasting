context("messaging functions")

# need to add in the unit components, but i think they are getting coverage
# elsewhere

test_that("model_done_message", {

  expect_message(xx <- model_done_message("xx", NA))
  expect_is(xx, "NULL")

})