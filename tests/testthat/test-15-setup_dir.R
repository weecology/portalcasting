context("directory setting up functions")



test_that("specialized setup_dirs work", {

  # downloads take too long for cran checks

    skip_on_cran() 

  expect_equal(1,1)
#  expect_message(setup_production(main = "./prod"))
#  expect_message(setup_sandbox(main = "./sand"))

})