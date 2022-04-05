context(desc = "rodent prep functions")

main <- "./testing"


test_that(desc = "prep_rodents preps all the rodents", {

  # downloads take too long for cran checks

    skip_on_cran() 

  r1 <- prep_rodents(main = main)
  r2 <- prep_rodents(main = main, rodent_datasets = "all")

  expect_is(r1, "list")
  expect_equal(length(r1), length(prefab_rodent_datasets()))

  expect_is(r2, "list")
  expect_equal(length(r2), 1)

})


test_that(desc = "rodents_species creates a vector of IDs as needed", {

  x1 <- rodent_species()
  x2 <- rodent_species(set = "all")
  x3 <- rodent_species("BA")
  x4 <- rodent_species("BA", set = "all")
  x5 <- all_species()
  x6 <- all_species(nadot = TRUE)
  x7 <- all_species(total = TRUE)
  x8 <- base_species()
  x9 <- evalplot_species()

  expect_equal(x1, NULL)
  expect_is(x2, "character")
  expect_is(x3, "character")
  expect_is(x4, "character")
  expect_is(x5, "character")
  expect_is(x6, "character")
  expect_is(x7, "character")
  expect_is(x8, "character")
  expect_is(x9, "character")

  expect_equal(length(x2), 21)
  expect_equal(length(x3), 1)
  expect_equal(length(x4), 21)
  expect_equal(length(x5), 21)
  expect_equal(length(x6), 21)
  expect_equal(length(x7), 22)
  expect_equal(length(x8), 20)
  expect_equal(length(x9), 7)

})

test_that(desc = "most_abundant_species finds them", {

  # downloads take too long for cran checks

    skip_on_cran() 

  topx <- 3
  mas <- most_abundant_species(main = main, topx = topx)
  expect_is(mas, "character")
  expect_equal(length(mas), topx)
  expect_equal(all(mas %in% rodent_species(set = "all")), TRUE)

})


test_that(desc = "prefab_rodent_datasets names the proper sets", {

  pds1 <- prefab_rodent_datasets()
  pds2 <- prefab_rodent_datasets(interpolate = TRUE)
  pds3 <- prefab_rodent_datasets(interpolate = FALSE)

  expect_is(pds1, "character")
  expect_is(pds2, "character")
  expect_is(pds3, "character")

  expect_equal(length(pds2) < length(pds1), TRUE)
  expect_equal(length(pds3) < length(pds1), TRUE)

})