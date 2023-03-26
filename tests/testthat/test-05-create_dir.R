context(desc = "directory creating functions")

main <- "./testing"


test_that(desc = "create_dir creates the folder structure",
          code = {
  
  main2 <- "./testing2"
  expect_message(create_dir(main = main2, verbose = TRUE))
  expect_silent(create_dir(main = main2, verbose = TRUE))


})


test_that(desc = "write_directory_config creates the config file",
          code = {


  config <- write_directory_configuration(main     = main)
  expect_equal(!is.null(config), TRUE)
  expect_is(config, "list")

})



test_that(desc = "read_directory_config catches if its missing and throws error",
          code = {


  expect_error(read_directory_configuration())

})


# we can test the other set_up functions once we have the directory squared away with the new format