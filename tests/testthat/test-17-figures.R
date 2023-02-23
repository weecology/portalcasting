context(desc = "Figure functions")

main <- "./testing"



test_that(desc = "plot_cast_ts", {

  # download is held back on cran

    skip_on_cran() 

  portalcast(main = main, models = c("AutoArima", "NaiveArima"))
  expect_silent(plot_cast_ts(main = main, species = "DM", 
                             model = "AutoArima"))
  expect_silent(plot_cast_ts(main = main, species = "DM"))
  expect_error(plot_cast_ts(main = main, species = "DM", cast_id = 1e10))

})


test_that(desc = "plot_cast_point", {

  # download is held back on cran

    skip_on_cran() 


  moons <- read_moons(main = main)
  

  last_census_date <- max(moons$censusdate, na.rm = TRUE)
  which_last_census_moon <- which(moons$censusdate == last_census_date)
  last_census_moon <- moons$newmoonnumber[which_last_census_moon]

  expect_silent(plot_cast_point(main = main))
  expect_silent(plot_cast_point(main = main, highlight_sp = "DM"))
  expect_silent(plot_cast_point(main = main, model = "AutoArima"))
  expect_silent(plot_cast_point(main = main, model = "AutoArima", moon = last_census_moon, with_census = TRUE))
  expect_error(plot_cast_point(main = main, cast_id = 1e10))


})




test_that(desc = "plot_casts_err_lead", {

  # download is held back on cran

    skip_on_cran() 

  expect_silent(plot_casts_err_lead(main = main))
  expect_silent(plot_casts_err_lead(main = main, models = "AutoArima", 
                                    ensemble = FALSE, species = "total", 
                                    dataset = "all"))
  expect_silent(plot_casts_err_lead(main = main, models = "AutoArima", 
                                    ensemble = FALSE,
                                   species = "BA", dataset = "all"))
  cast_tab <- read_cast_tabs(main = main)
  expect_error(plot_casts_err_lead(main = main, cast_tab = cast_tab,
                                   cast_id = 1e10))
  expect_error(plot_casts_err_lead(main = main, cast_id = 1e10))

})



test_that(desc = "plot_casts_cov_RMSE", {

  # download is held back on cran

    skip_on_cran() 

  expect_silent(plot_casts_cov_RMSE(main = main, species = "DM"))
  cast_tab <- read_cast_tabs(main = main)
  expect_error(plot_casts_cov_RMSE(main = main, cast_tab = cast_tab,
                                   cast_id = 1e10))
  expect_error(plot_casts_cov_RMSE(main = main, cast_id = 1e10))

})