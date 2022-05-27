context(desc = "Figure functions")

main <- "./testing"

test_that(desc = "plot_cast_point", {

  # download is held back on cran

    skip_on_cran() 


  fill_data(main = main)
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

