context(desc = "jags functions")

test_that(desc = "run_jags_control makes a list", {

  expect_is(runjags_control(), "list")

})

