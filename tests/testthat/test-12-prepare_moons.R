context("moons prepping functions")

main <- "./testing"

test_that("prep_moons pulls in moons or throws error", {

  # downloads take too long for cran checks

    skip_on_cran() 

  moons <- prep_moons(main = main)
  expect_is(moons, "data.frame")
  expect_error(prep_moons("ok"))

})

test_that("add_future_moons skips for 0 or adds specifically extra more", {

  # downloads take too long for cran checks

    skip_on_cran() 

  moons <- prep_moons(main = main)
  moons$newmoondate <- moons$moondate

  moons2 <- add_future_moons(main = main, moons, lead_time = 0)
  expect_is(moons2, "data.frame")
  expect_equal(moons, moons2)
 
  raw_path <- raw_path(main = main)
  traps_in <- load_trapping_data(path = raw_path, download_if_missing = FALSE,
                                 clean = FALSE, quiet = TRUE)
  moons_in <- traps_in[["newmoons_table"]]
  moons3 <- add_future_moons(main = main, moons = moons_in, 
                             lead_time = 15, cast_date = Sys.Date())
  moons3 <- format_moons(moons3)
  expect_is(moons3, "data.frame")
  expect_equal(NROW(moons3) > NROW(moons2), TRUE)

})


test_that("trim_moons trims off the end of the data frame", {

  # downloads take too long for cran checks

    skip_on_cran() 

  moons <- prep_moons(main = main)
  moons2 <- trim_moons(moons, 300:310)
  expect_is(moons2, "data.frame")
  expect_equal(NROW(moons2) < NROW(moons), TRUE)

})

test_that("add_moons_from_date adds moons on to weather", {

  # downloads take too long for cran checks

    skip_on_cran() 

  raw_path <- sub_path(main = main, subs = "raw")
  weather <- portalr::weather("daily", fill = TRUE, path = raw_path)
  moons <- prep_moons(main = main)
  weather2 <- add_moons_from_date(weather, moons)
  expect_is(weather2, "data.frame") 
  expect_equal(NCOL(weather) < NCOL(weather2), TRUE)

})

test_that("target_moons produces integer output", {

  # downloads take too long for cran checks

    skip_on_cran() 

  tms <- target_moons(main = main)
  expect_is(tms, "integer")

})
