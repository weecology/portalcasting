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



test_that(desc = "rodents_species creates a vector of IDs as needed", {

  x1 <- rodent_species()
  x2 <- rodent_species(set = "all")
  x3 <- rodent_species(set = "base")
  x4 <- all_species(type = "abbreviation")
  x5 <- all_species(type = "Latin")
  x6 <- all_species(type = "table")
  x7 <- all_species()
  x8 <- all_species(nadot = TRUE)
  x9 <- all_species(total = TRUE)
  x0 <- base_species()
  xa <- eval_species()


  expect_equal(x1, NULL)
  expect_is(x2, "character")
  expect_is(x3, "character")
  expect_is(x4, "character")
  expect_is(x5, "character")
  expect_is(x6, "data.frame")
  expect_is(x7, "character")
  expect_is(x8, "character")
  expect_is(x9, "character")
  expect_is(x0, "character")
  expect_is(xa, "character")

  expect_error(rodent_species(type = "xxx", set = "all"))
  expect_error(rodent_species(set = "xxx"))

})



test_that(desc = "prefab_datasets names the proper sets", {

  pds1 <- prefab_datasets()

  expect_is(pds1, "character")
  expect_equal(sort(pds1), c("all", "controls", "exclosures"))

})