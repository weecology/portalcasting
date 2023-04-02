context(desc = "prefab model functions")

main1 <- normalizePath(file.path(tempdir(), "testing1"))
main2 <- normalizePath(file.path(tempdir(), "testing2"))
main3 <- normalizePath(file.path(tempdir(), "testing3"))


test_that(desc = "prefab_models creates a vector of model names", {

  pfm <- prefab_models()
  expect_equal(length(pfm), 15)
  expect_is(pfm, "character")

})
