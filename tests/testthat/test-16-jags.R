context(desc = "jags functions")

test_that(desc = "run_jags_controls makes a list", {

  expect_is(runjags_controls(), "list")

})

