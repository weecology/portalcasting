context(desc = "directory setting up functions")

main1 <- "./testing1"
main2 <- "./testing2"

test_that(desc = "setup_production creates a production directory",
          code = {
  
  expect_message(setup_production(main = main1, verbose = TRUE))


})



test_that(desc = "setup_sandbox creates a sandbox directory",
          code = {
  
  expect_message(setup_sandbox(main = main2, verbose = TRUE))


})