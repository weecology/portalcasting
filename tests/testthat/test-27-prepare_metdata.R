context(desc = "metadata prepping functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "prepare_metadata", {

  skip_on_cran()

  prepare_rodents(main = main2)

  md <- prepare_metadata(main = main2)
  expect_is(md, "list")

})