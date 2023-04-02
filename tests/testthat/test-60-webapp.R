context(desc = "webapp functions")

main1 <- normalizePath(file.path(tempdir(), "testing1"))
main2 <- normalizePath(file.path(tempdir(), "testing2"))
main3 <- normalizePath(file.path(tempdir(), "testing3"))

test_that(desc = "lists functions build lists", {

  expect_silent(ft <- historic_end_newmoonnumber_list(main = main3))
  expect_is(ft, "integer")

  expect_silent(ft <- model_list())
  expect_is(ft, "character")

  expect_silent(ft <- species_list())
  expect_is(ft, "character")

})