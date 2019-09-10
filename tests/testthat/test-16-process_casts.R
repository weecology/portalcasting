context("Test process_casts functions")

main <- "./testing"

test_that("read_cast_tab", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(read_cast_tab(main = main, cast_id = NULL), "data.frame")
  expect_error(read_cast_tab(main = main, cast_id = 1e10))
})

test_that("read_cast_tabs", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(read_cast_tabs(main = main, cast_ids = NULL), "data.frame")
  expect_is(read_cast_tabs(main = main, cast_ids = 1:2), "data.frame")
})

test_that("read_cast_metadata", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(read_cast_metadata(main = main, cast_id = NULL), "list")
  expect_error(read_cast_metadata(main = main, cast_id = 1e10))
})

test_that("add_to_cast_tab", {
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
