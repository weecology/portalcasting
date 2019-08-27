context("Test prepare_moons functions")

test_that("prep_moons", {
  skip_on_cran() # downloads take too long for cran checks
  moons <- prep_moons(main = "./testing")
  expect_is(moons, "data.frame")
})

test_that("add_future_moons", {
  skip_on_cran() # downloads take too long for cran checks
  moons <- prep_moons(main = "./testing")
  expect_is(add_future_moons(moons, lead_time = 0), "data.frame")
})

test_that("add_extra_future_moons", {
  skip_on_cran() # downloads take too long for cran checks
  moons <- prep_moons(main = "./testing")
  max_moon <- max(moons$moondate) 
  max_plus <- max_moon + 100
  expect_is(add_extra_future_moons(moons, cast_date = max_plus), "data.frame")
})

test_that("trim_moons", {
  skip_on_cran() # downloads take too long for cran checks
  moons <- prep_moons(main = "./testing")
  expect_is(trim_moons(moons, 300:310), "data.frame")
})

test_that("add_moons_from_date", {
  skip_on_cran() # downloads take too long for cran checks
  raw_path <- sub_paths(main = "./testing", specific_subs = "raw")
  weather <- portalr::weather("daily", TRUE, raw_path)
  moons <- prep_moons(main = "./testing")
  expect_is(add_moons_from_date(weather, moons), "data.frame") 
})

test_that("target_moons", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(target_moons(main = "./testing"), "integer")
})
