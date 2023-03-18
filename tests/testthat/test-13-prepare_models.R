context(desc = "model preparation functions")

main <- "./testing"



test_that(desc = "read_model_controls does", {

  mc <- read_model_controls(main = main)
  expect_is(mc, "list")

})



test_that(desc = "model_controls reads in a list", {

  mc <- model_controls(main = main)
  expect_is(mc, "list")

})



test_that(desc = "write_model_controls returns a list", {

  mc <- write_model_controls(main = main)
  expect_is(mc, "list")

})

