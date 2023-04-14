context("Testing Testing 123")

test_that("test", {

  expect_equal(1, 1)
  
})

message(runjags::testjags() )

test_that("test", {

  expect_equal(1, 1)
  
})