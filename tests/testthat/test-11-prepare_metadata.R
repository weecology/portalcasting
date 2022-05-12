context(desc = "metadata prepping functions")

main <- "./testing"

test_that(desc = "prep_metadata", {

  # downloads take too long for cran checks

    skip_on_cran() 


  md <- prep_metadata(main = main)
  expect_is(md, "list")

})

