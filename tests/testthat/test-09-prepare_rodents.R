context(desc = "rodent prep functions")

main <- "./testing"


test_that(desc = "prepare_abundance preps a vector with interpolation if requested", {

  # downloads take too long for cran checks

    skip_on_cran() 

  r1 <- prepare_abundance(main = main, dataset = "all", species = "DM", model = "AutoArima")
  r2 <- prepare_abundance(main = main, dataset = "all", species = "DM", model = "ESSS")

  expect_is(r1, "integer")
  expect_is(r2, "numeric")
  expect_equal(length(r1), length(r2))
  expect_equal(sum(is.na(r1)) > 0, TRUE)  
  expect_equal(sum(is.na(r2)) > 0, FALSE)  

})



test_that(desc = "prepare_rodents preps all the rodents", {

  # downloads take too long for cran checks

    skip_on_cran() 

  r1 <- prepare_rodents(main = main)
  r2 <- prepare_rodents(main = main, datasets = "all")

  expect_is(r1, "list")
  expect_equal(length(r1), length(prefab_datasets()))

  expect_is(r2, "list")
  expect_equal(length(r2), length(prefab_datasets()))

})



test_that(desc = "prefab_datasets names the proper sets", {

  pds1 <- prefab_datasets()

  expect_is(pds1, "character")
  expect_equal(sort(pds1), c("all", "controls", "exclosures"))

})