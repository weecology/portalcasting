context("Test process_casts functions")

main <- "./testing"

test_that("read_cast_tab", {
  skip_on_cran() # downloads take too long for cran checks
  expect_error(read_cast_tab(main = main, cast_id = 1e10))
})
