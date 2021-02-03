context("basic arg checking utilities")

test_that("check_args throws needed errors",{
  expect_error(check_args("a"))
  expect_error(check_args(c(TRUE, TRUE)))
})

test_that("check_args returns if false", {
  expect_equal(check_args(FALSE), NULL)
})


test_that("check_arg messages about new args", {
  expect_message(val <- check_arg("xyz123", TRUE))
  expect_equal(val, NULL)
})


test_that("check_arg handles the range of input possibilities", {

  expect_message(check_arg("x", 12))

  # date

    expect_equal(check_arg("cast_date", 1), 
                 "`cast_date` must be a Date or date-conformable")
    expect_equal(check_arg("cast_date", NA), 
                 "`cast_date` cannot be NA")

  # df: data frame

    expect_equal(check_arg("moons", 1),  
                 "`moons` must be a data.frame")

  # cast: data frame or list

    expect_equal(check_arg("cast", 1), 
                 "`cast` must be a data.frame or list")

  # dfv: data frame or vector

    expect_equal(check_arg("dfv", matrix(123)), 
                 "`dfv` must be a data.frame or vector")

  # listfun: list or function

    expect_equal(check_arg("mutate", 1),  
                 "`mutate` must be a list, or function")

  # inits: character vector list or function

    expect_equal(check_arg("inits", 1),  
                 "`inits` must be a character vector, list, or function")

  # dfl: data frame or list

    expect_equal(check_arg("dfl", 1), 
                 "`dfl` must be a data.frame or list")

  # intnum: integer

    expect_equal(check_arg("lev", 0.2),  
                 "`lev` must be an integer-conformable number")

  # nonnegintnum: non-negative integer

    expect_equal(check_arg("lead_time", -1), 
            "`lead_time` must be a non-negative integer-conformable number")

  # positnum: positive integer

    expect_equal(check_arg("end_moon", -1), 
                 "`end_moon` must be a positive integer-conformable number")

  # zeroone: 0 - 1

    expect_equal(check_arg("confidence_level", 1.1), 
                 "`confidence_level` must be a number between 0 and 1")


  # extension: character and extension

    expect_equal(check_arg("extension", 1), 
                  "`extension` must be a character")

    expect_equal(check_arg("extension", "ab"),  
                 "`extension` must be an extension")

  # logical

    expect_equal(check_arg("cleanup", "ok"),  
                 "`cleanup` must be a logical")

  # character

    expect_equal(check_arg("colname", 1),  
                 "`colname` must be a character")

  # numeric

    expect_equal(check_arg("lat", "1"),  
                 "`lat` must be a numeric")


  # from options

    expect_equal(check_arg("sub", "abc"),
                 "`sub` must be one of casts, models, raw, data, fits, tmp")


})

