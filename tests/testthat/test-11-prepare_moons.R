context("Test prepare_moons functions")

moons_opts <- all_options(main = "testing_casting")$options_data$moons
moons <- prep_moons(moons_opts)

test_that("prep_moons", {
  expect_error(prep_moons(1), "`options_moons` is not")
  expect_message(moons <- prep_moons(moons_opts))
  expect_is(moons, "moons")
})

test_that("append_past_moons_to_raw", {
  expect_error(append_past_moons_to_raw(moons, 1), "`options_moons` is not")
  expect_error(append_past_moons_to_raw(1, moons_opts), "`moons` is not")
  expect_is(append_past_moons_to_raw(moons, moons_opts), "moons")
})

test_that("add_future_moons", {
  expect_error(add_future_moons(moons, 1), "`options_moons` is not")
  expect_error(add_future_moons(1, moons_opts), "`moons` is not")

  moons <- file_path(moons_opts$tree, "PortalData/Rodents/moon_dates.csv") %>%
            read.csv(stringsAsFactors = FALSE) %>% 
            classy(c("data.frame", "moons"))
  expect_is(add_future_moons(moons, moons_opts), "moons")
  moons_opts2 <- moons_opts
  moons_opts2$n_future_moons <- 0
  expect_is(add_future_moons(moons, moons_opts2), "moons")
})

test_that("add_addl_future_moons", {
  future_moons <- get_future_moons(moons, moons_opts$n_future_moons) %>%
                  classy(c("data.frame", "moons"))
  cast_date <- moons_opts$cast_date
  expect_error(add_addl_future_moons(future_moons, 1), "`cast_date` is not")
  expect_error(add_addl_future_moons(1, cast_date), "`future_moons` is not")
  expect_error(add_addl_future_moons(future_moons, c(cast_date, cast_date)), 
               "`cast_date` can only be")
  expect_is(add_addl_future_moons(future_moons, cast_date), "moons")
  cast_date2 <- as.Date("2100-01-01")
  expect_is(add_addl_future_moons(future_moons, cast_date2), "moons")
})

test_that("format_moons", {
  expect_error(format_moons(1), "`moons` is not")
  expect_is(format_moons(moons), "moons")
  expect_is(format_moons(append_past_moons_to_raw(moons, moons_opts)), 
            "moons")
})