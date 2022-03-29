context("messaging functions")

# need to add in the unit components, but i think they are getting coverage
# elsewhere

test_that("messageq quiets", {

  expect_message(messageq(123, quiet = FALSE))
  expect_silent(messageq(123, quiet = TRUE))

})


test_that("message_break writes", {

  expect_is(message_break(), "character")

})

