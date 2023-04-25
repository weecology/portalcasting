context(desc = "rodent prep functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")


test_that(desc = "prepare_abundance preps a vector with interpolation if requested", {

  skip_on_cran() 

  r1 <- prepare_abundance(main = main2, dataset = "all", species = "DM", model = "AutoArima")
  r2 <- prepare_abundance(main = main2, dataset = "all", species = "DM", model = "ESSS")

  expect_is(r1, "integer")
  expect_is(r2, "numeric")
  expect_equal(length(r1), length(r2))
  expect_equal(sum(is.na(r1)) > 0, TRUE)  
  expect_equal(sum(is.na(r2)) > 0, FALSE)  

})



test_that(desc = "prepare_rodents preps all the rodents", {

  skip_on_cran() 

  r1 <- prepare_rodents(main = main2)
  r2 <- prepare_rodents(main = main2, datasets = "all")

  expect_is(r1, "list")
  expect_equal(length(r1), length(prefab_datasets()))

  expect_is(r2, "list")
  expect_equal(length(r2), 1)

})




test_that(desc = "prefab_datasets names the proper sets", {

  pds1 <- prefab_datasets( )

  expect_is(pds1, "character")
  expect_equal(sort(pds1), c("all", "controls", "exclosures"))

})

test_that(desc = "prefab_rodents names the proper species", {

  ps1 <- prefab_species(main = main2)

  expect_is(ps1, "character")
  expect_equal(sort(ps1), c("BA", "DM", "DO", "DS", "NA", "OL", "OT", "PB", "PE", "PF", "PH", "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO", "total"))

})






test_that(desc = "add_new_dataset makes a list", {

  dc <- add_new_dataset(main = main2, models = "AutoArima", new_dataset_controls = new_dataset_controls(metadata = new_dataset_metadata(name = "newone")))
  expect_is(dc, "list")

})



test_that(desc = "dataset_controls_template reads in the list", {

  dc <- dataset_controls_template( )
  expect_is(dc, "list")
  expect_equal(length(dc), 3)

})




test_that(desc = "new_dataset_<> functions update the list", {

  dc <- new_dataset_controls(extra_field = 1234)
  expect_is(dc, "list")
  expect_equal(names(dc)[length(dc)], "extra_field")
  expect_equal(dc[[length(dc)]], 1234)

  dc <- new_dataset_metadata(name = "xyz")
  expect_is(dc, "list")
  expect_equal(dc$name, "xyz")

  dc <- new_dataset_fun(fun = "newfun")
  expect_is(dc, "character")

  dc <- new_dataset_args()
  expect_is(dc, "list")

})



