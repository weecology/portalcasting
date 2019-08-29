context("Test arg functions")

test_that("check_args", {
  expect_error(check_args("a"))
  expect_error(check_args(c(TRUE, TRUE)))

})

test_that("check_arg", {

  expect_message(check_arg("x", 12))

  expect_is(check_arg("cast_date", 1), "character")
  expect_is(check_arg("cast_date", NA), "character")
  expect_is(check_arg("moons", 1), "character")
  expect_is(check_arg("cast", 1), "character")
  expect_is(check_arg("dfl", 1), "character")
  expect_is(check_arg("dfv", matrix(123)), "character")
  expect_is(check_arg("end_moon", -1), "character")
  expect_is(check_arg("lead_time", -1), "character")
  expect_is(check_arg("lev", 0.2), "character")
  expect_is(check_arg("confidence_level", 1.1), "character")
  expect_is(check_arg("extension", 1), "character")
  expect_is(check_arg("extension", "ab"), "character")
})

