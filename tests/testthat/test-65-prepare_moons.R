context("Test prepare_moons functions")



test_that("prep_moons", {
  moons <- prep_moons(main = "./testing")
  expect_is(moons, "data.frame")
})

test_that("add_future_moons", {
  moons <- prep_moons(main = "./testing")
  expect_is(add_future_moons(moons, lead_time = 0), "data.frame")

})

unlink(main_path(main = "./testing"), recursive = TRUE, force = TRUE)