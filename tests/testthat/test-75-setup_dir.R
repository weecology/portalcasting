context("Test directory setting up functions")

test_that("setup_dir", {
  skip_on_cran() # downloads take too long for cran checks
  #expect_message(setup_dir(main = "./testing/dir")) excessive for the time
  #expect_message(setup_production(main = "./testing/prod")) same ^
  expect_message(setup_sandbox(main = "./sand"))
})

test_that("sandbox_welcome", {
  expect_message(sandbox_welcome())
})


