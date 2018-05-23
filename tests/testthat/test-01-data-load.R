context("Dummy Test")

test_that("dummy test", {
  hash_val <- digest::digest("HI HOW ARE YOU")
  expect_equal(hash_val, "8e5bbb3966948b6e0627c5cd041e8051")
})