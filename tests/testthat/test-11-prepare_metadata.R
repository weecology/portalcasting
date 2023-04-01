context(desc = "metadata prepping functions")

main <- "./testing"

test_that(desc = "prepare_metadata", {

  # downloads take too long for cran checks

    skip_on_cran() 

  prepare_rodents(main = main)

  md <- prepare_metadata(main = main)
  expect_is(md, "list")

})

