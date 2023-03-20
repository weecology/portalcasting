context(desc = "Figure functions")

main <- "./testing"


test_that(desc = "plot_cast_ts", {

  # download is held back on cran

    skip_on_cran() 

  expect_silent(plot_cast_ts(main = main, species = "DM", 
                             model = "AutoArima"))
  expect_silent(plot_cast_ts(main = main, species = "DM"))
  expect_error(plot_cast_ts(main = main, species = "DM", cast_id = 1e10))

})

test_that(desc = "plot_cast_point", {

  # download is held back on cran

    skip_on_cran() 

  expect_silent(plot_cast_point(main = main))
  expect_silent(plot_cast_point(main = main, highlight_sp = "DM"))
  expect_silent(plot_cast_point(main = main, model = "AutoArima"))
  expect_silent(plot_cast_point(main = main, model = "AutoArima", with_census = TRUE))
  expect_error(plot_cast_point(main = main, cast_id = 1e10))


})





test_that(desc = "plot_casts_err_lead", {

  # download is held back on cran

    skip_on_cran() 


  expect_silent(plot_casts_err_lead(main = main, models = c("AutoArima"), species = c("DM")))
  expect_silent(plot_casts_err_lead(main = main, models = c("AutoArima", "ESSS"), species = c("DM", "BA")))
  expect_error(plot_casts_err_lead(main = main, cast_id = 1e10))

})



test_that(desc = "plot_casts_cov_RMSE", {

  # download is held back on cran

    skip_on_cran() 

  expect_silent(plot_casts_cov_RMSE(main = main, models = c("AutoArima"), species = c("DM")))
  expect_silent(plot_casts_cov_RMSE(main = main, models = c("AutoArima", "ESSS"), species = c("DM", "BA")))
  expect_error(plot_casts_cov_RMSE(main = main, cast_id = 1e10))


})