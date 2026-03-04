context(desc = "moons prepping functions")

test_that(desc = "prep_moons pulls in moons or throws error", {

  skip_on_cran()

  moons <- prepare_newmoons(main = main2)
  expect_is(moons, "data.frame")

})

test_that(desc = "add_forecast_newmoons skips for 0", {

  skip_on_cran()

  newmoons  <- prepare_newmoons(main = main2)
  newmoons2 <- add_forecast_newmoons(main     = main2,
                                     newmoons = newmoons)

  expect_is(newmoons2, "data.frame")

})