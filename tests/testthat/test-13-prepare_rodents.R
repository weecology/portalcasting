context(desc = "rodent prep functions")

main <- "./testing"


test_that(desc = "prep_rodents preps all the rodents", {

  # downloads take too long for cran checks

    skip_on_cran() 

  r1 <- prep_rodents(main = main)
  r2 <- prep_rodents(main = main, datasets = "all")

  expect_is(r1, "list")
  expect_equal(length(r1), length(prefab_datasets()))

  expect_is(r2, "list")
  expect_equal(length(r2), 1)

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


  expect_equal(x1, NULL)
  expect_is(x2, "character")
  expect_is(x3, "character")
  expect_is(x4, "character")
  expect_is(x5, "character")
  expect_is(x6, "data.frame")
  expect_is(x7, "character")
  expect_is(x8, "character")
  expect_is(x9, "character")

  expect_error(rodent_species(type = "xxx", set = "all"))
  expect_error(rodent_species(set = "xxx"))

})



test_that(desc = "prefab_datasets names the proper sets", {

  pds1 <- prefab_datasets()
  pds2 <- prefab_datasets(interpolate = TRUE)
  pds3 <- prefab_datasets(interpolate = FALSE)

  expect_is(pds1, "character")
  expect_is(pds2, "character")
  expect_is(pds3, "character")

  expect_equal(length(pds2) < length(pds1), TRUE)
  expect_equal(length(pds3) < length(pds1), TRUE)

})