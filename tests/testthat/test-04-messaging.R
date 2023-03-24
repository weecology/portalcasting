context(desc = "messaging functions")

# need to add in the unit components, but i think they are getting coverage
# elsewhere

test_that(desc = "messageq quiets",
          code = {

  expect_message(messageq(123, quiet = FALSE))
  expect_silent(messageq(123, quiet = TRUE))

})


test_that(desc = "break_line writes",
          code = {

  expect_is(break_line(), "character")

})

