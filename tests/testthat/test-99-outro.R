context(desc = "cleaning house")

test_that(desc = "removal main1",
          code = {

  expect_equal((unlink(file.path(main1), recursive = TRUE, force = TRUE)), 0)

})

test_that(desc = "removal main2 and main3",
          code = {

  if (nzchar(Sys.getenv("PORTALCASTING_TEST_CACHE"))) {
    skip("Skipping cache cleanup when PORTALCASTING_TEST_CACHE is set")
  }
  expect_equal((unlink(file.path(main2), recursive = TRUE, force = TRUE)), 0)

})


