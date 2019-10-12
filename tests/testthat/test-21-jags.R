context("Test jags functions")

test_that("run_jags_control", {
  expect_is(runjags_control(), "list")
})

