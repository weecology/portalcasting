context("Figures")

main <- "./testing"

test_that("plot_cast_point", {
  skip_on_cran() # downloads and casting take too long to run on cran
  portalcast(main = main, models = c("AutoArima", "NaiveArima"),
             end_moon = last_moon(main) - 5)
print(main)
  expect_silent(plot_cast_point(main = main))
  expect_error(plot_cast_point(main = main, cast_id = 1e10))

})

test_that("plot_cast_ts", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_silent(plot_cast_ts(main = main))
  expect_silent(plot_cast_ts(main = main, species = "DM"))
  expect_error(plot_cast_ts(main = main, cast_id = 1e10))

})