context(desc = "webapp server functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "server functions work off of global list", {

  skip_on_cran() 

  expect_silent(gl <- global_list(main = main3))
  expect_is(gl, "list")

  expect_silent(irv <- initial_reactive_values(global = gl))
  expect_is(irv, "reactivevalues")

  expect_silent(io <- initial_output(main = main3, rv = irv, output = list()))
  expect_is(io, "list")

})