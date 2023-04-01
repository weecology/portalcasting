context(desc = "directory creating functions")

main <- "./testing"
main2 <- "./testing2"


test_that(desc = "create_dir creates the folder structure",
          code = {
  
  expect_message(create_dir(main = main, verbose = TRUE))
  expect_silent(create_dir(main = main, verbose = TRUE))


})


test_that(desc = "write_directory_config creates the config file",
          code = {


  config <- write_directory_configuration(main     = main)
  expect_equal(!is.null(config), TRUE)
  expect_is(config, "list")

})



test_that(desc = "read_directory_config catches if its missing and throws error",
          code = {

  expect_is(read_directory_configuration(main = main), "list")
  expect_error(read_directory_configuration())

})



test_that(desc = "setup_sandbox creates a sandbox directory",
          code = {
  
  expect_message(setup_sandbox(main = main2, verbose = TRUE))


})