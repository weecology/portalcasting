context("Test process_casts functions")

main <- "./testing"

test_that("read_cast_tab", {
  skip_on_cran() # downloads take too long for cran checks
  expect_error(read_cast_tab(main = main, cast_id = 1e10))
})


test_that("read_cast_tab", {
  skip_on_cran() # downloads take too long for cran checks
   cast_tab <- read_cast_tab(main = main, cast_id = 1)
   expect_is(add_lead_to_cast_tab(main = main, cast_tab = cast_tab),
             "data.frame")
   expect_is(add_obs_to_cast_tab(main = main, cast_tab = cast_tab), 
             "data.frame")
   expect_is(add_err_to_cast_tab(main = main, cast_tab = cast_tab), 
             "data.frame")
   expect_is(add_covered_to_cast_tab(main = main, cast_tab = cast_tab), 
             "data.frame")
})
