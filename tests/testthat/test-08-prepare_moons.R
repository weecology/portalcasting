context(desc = "moons prepping functions")

main <- "./testing"

test_that(desc = "prep_moons pulls in moons or throws error", {

  # downloads take too long for cran checks

    skip_on_cran() 

  moons <- prepare_newmoons(main = main)
  expect_is(moons, "data.frame")

})

test_that(desc = "add_future_moons skips for 0", {

  # downloads take too long for cran checks

    skip_on_cran() 

  newmoons  <- prepare_newmoons(main = main)
  newmoons2 <- add_forecast_newmoons(main     = main, 
                                     newmoons = newmoons,
                                     settings = directory_settings(time = time_settings(lead_time = 0)))
                                 
  expect_equal(newmoons, newmoons2)

})
