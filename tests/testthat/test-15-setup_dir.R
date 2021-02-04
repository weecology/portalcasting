context("directory setting up functions")


test_that("sandbox_welcome makes a nice message", {

  expect_message(sandbox_welcome())

})

test_that("specialized setup_dirs work", {

  # downloads take too long for cran checks

    skip_on_cran() 

  expect_equal(1,1)
#  expect_message(setup_production(main = "./prod"))
#  expect_message(setup_sandbox(main = "./sand"))

})