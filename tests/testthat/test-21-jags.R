context("jags functions")

test_that("run_jags_control makes a list", {

  expect_is(runjags_control(), "list")

})

