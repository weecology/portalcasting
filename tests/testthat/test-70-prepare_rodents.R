context("Test prepare_rodents functions")

test_that("prep_rodents_table", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(prep_rodents_table(main = "./testing"), "data.frame")
})


test_that("trim_treatment", {
  skip_on_cran() # downloads take too long for cran checks
  raw_path <- sub_paths(main = "./testing", specific_subs = "raw")
  dat1 <- summarize_rodent_data(path = raw_path, clean = FALSE,
                                level = "Treatment")
  dat2 <- trim_species(dat1)
  dat3 <- add_total(dat2)
  expect_is(trim_treatment(dat3, "Treatment", "control"), "data.frame")
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

unlink(main_path(main = "./testing"), recursive = TRUE, force = TRUE)