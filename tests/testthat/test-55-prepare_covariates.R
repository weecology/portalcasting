context("Test prepare_covariates functions")



test_that("prep_covariates", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(prep_covariates(main = "./testing"), "data.frame")
})


test_that("prep_covariates", {
  skip_on_cran() # downloads take too long for cran checks
  raw_path <- sub_paths(main = "./testing", specific_subs = "raw")
  moons <- prep_moons(main = "./testing")
  weather("daily", TRUE, raw_path) %>% 
  add_date_from_components() %>%
  select(-c(year, month, day, battery_low, locally_measured))  %>%
  add_newmoons_from_date(moons) %>%
  summarize_daily_weather_by_newmoon() -> x
  expect_is(x, "data.frame")
})
