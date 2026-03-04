context(desc = "prefab model functions")

test_that(desc = "prefab_models creates a vector of model names", {

  pfm <- prefab_models()
  expect_equal(length(pfm), 15)
  expect_is(pfm, "character")

})
