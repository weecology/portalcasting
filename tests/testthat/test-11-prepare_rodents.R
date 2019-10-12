context("Test prepare_rodents functions")
main <- "./testing"
test_that("rodents_control", {
  expect_is(rodents_control("all_PPonly", species = "PP"), "list")
})

test_that("prep_rodents_table", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(prep_rodents_table(main = "./testing"), "data.frame")
  expect_is(prep_rodents_table(main = "./testing", time = "date"), 
            "data.frame")
})

test_that("prep_rodents", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(prep_rodents(main = "./testing"), "list")
  expect_is(prep_rodents(main = "./testing", 
                              controls = rodents_controls()), "list")
})

test_that("last_census", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(last_census(main = "./testing"), "Date")
})

test_that("rodents_controls",{
  expect_is(rodents_controls(c("all", "controls")), "list")
  controls_r <- rodents_controls("all")[[1]]
  controls_r$name <- "allX"
  expect_is(rodents_controls("allX", controls_rodents = controls_r),
                             "list")
  controls_r$name <- "all"
  expect_error(rodents_controls("all", controls_rodents = controls_r))
  expect_error(rodents_controls("allX", arg_checks = FALSE))
})

test_that("rodents_species", {
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

test_that("most_abundant_species", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(most_abundant_species(main = main), "character")
})


test_that("prefab_data_sets", {
  expect_is(prefab_data_sets(), "character")
  expect_is(prefab_data_sets(TRUE), "character")
  expect_is(prefab_data_sets(FALSE), "character")
})