context(desc = "directory creating functions")

main <- "./testing"


test_that(desc = "create_dir fully creates the folder structure",
          code = {

  expect_message(create_dir(main = main))
  expect_is(read_directory_config(main), "list")

})


