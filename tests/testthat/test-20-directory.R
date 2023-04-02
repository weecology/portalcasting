context(desc = "directory creating functions")

main1 <- normalizePath(file.path(tempdir(), "testing1"))
main2 <- normalizePath(file.path(tempdir(), "testing2"))
main3 <- normalizePath(file.path(tempdir(), "testing3"))


test_that(desc = "create_dir creates the folder structure",
          code = {
  
  expect_message(create_dir(main = main1, verbose = TRUE))
  expect_silent(create_dir(main = main1, verbose = TRUE))


})


test_that(desc = "write_directory_config creates the config file",
          code = {


  config <- write_directory_configuration(main = main1)
  expect_equal(!is.null(config), TRUE)
  expect_is(config, "list")

})



test_that(desc = "read_directory_config catches if its missing and throws error",
          code = {

  expect_is(read_directory_configuration(main = main1), "list")
  expect_error(read_directory_configuration(main = "xyz"))

})


test_that(desc = "setup_sandbox does",
          code = {

  skip_on_cran() 

  expect_message(out <- setup_sandbox(main = main2, verbose = TRUE))
  expect_is(out, "list")

})

test_that(desc = "setup_production does",
          code = {

  skip_on_cran() 

  expect_message(out <- setup_production(main = main3, verbose = TRUE))
  expect_is(out, "list")

})