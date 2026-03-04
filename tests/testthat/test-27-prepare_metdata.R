context(desc = "metadata prepping functions")

test_that(desc = "prepare_metadata", {

  skip_on_cran()

  prepare_rodents(main = main2)

  md <- prepare_metadata(main = main2)
  expect_is(md, "list")

})