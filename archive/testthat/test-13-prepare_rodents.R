context("rodent prep functions")

main <- "./testing"

test_that("rodents_control creates a proper list", {

  rc <- rodents_control("all_PPonly", species = "PP")
  expect_is(rc, "list")

})

test_that("prep_rodents_table makes the table for moons or dates", {

  # downloads take too long for cran checks

    skip_on_cran() 

  r1 <- prep_rodents_table(main = main)
  r2 <- prep_rodents_table(main = main, time = "date")

  expect_is(r1, "data.frame")
  expect_equal(sum(colnames(r1) == "moon"), 1)
  expect_equal(sum(colnames(r1) == "censusdate"), 0)

  expect_is(r2, "data.frame")
  expect_equal(sum(colnames(r2) == "moon"), 0)
  expect_equal(sum(colnames(r2) == "censusdate"), 1)
  
})

test_that("prep_rodents preps all the rodents", {

  # downloads take too long for cran checks

    skip_on_cran() 

  r1 <- prep_rodents(main = main)
  r2 <- prep_rodents(main = main, data_sets = "all")

  expect_is(r1, "list")
  expect_equal(length(r1), length(prefab_data_sets()))

  expect_is(r2, "list")
  expect_equal(length(r2), 1)

})

test_that("last_census finds it", {

  # downloads take too long for cran checks

    skip_on_cran() 

  expect_is(last_census(main = main), "Date")

})

test_that("rodents_controls creates lists or throws errors",{

  expect_is(rodents_controls(c("all", "controls")), "list")
  controls_r <- rodents_controls("all")[[1]]
  controls_r$name <- "allX"
  expect_is(rodents_controls("allX", controls_rodents = controls_r),
                             "list")
  controls_r$name <- "all"
  expect_error(rodents_controls("all", controls_rodents = controls_r))
  expect_error(rodents_controls("allX", arg_checks = FALSE))

})

test_that("rodents_species creates a vector of IDs as needed", {

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

test_that("most_abundant_species finds them", {

  # downloads take too long for cran checks

    skip_on_cran() 

  topx <- 3
  mas <- most_abundant_species(main = main, topx = topx)
  expect_is(mas, "character")
  expect_equal(length(mas), topx)
  expect_equal(all(mas %in% rodent_species(set = "all")), TRUE)

})


test_that("prefab_data_sets names the proper sets", {

  pds1 <- prefab_data_sets()
  pds2 <- prefab_data_sets(interpolate = TRUE)
  pds3 <- prefab_data_sets(interpolate = FALSE)

  expect_is(pds1, "character")
  expect_is(pds2, "character")
  expect_is(pds3, "character")

  expect_equal(length(pds2) < length(pds1), TRUE)
  expect_equal(length(pds3) < length(pds1), TRUE)

})