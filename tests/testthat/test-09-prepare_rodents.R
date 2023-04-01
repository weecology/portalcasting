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
  expect_equal(length(r2), 1)

})




test_that(desc = "prefab_datasets names the proper sets", {

  pds1 <- prefab_datasets()

  expect_is(pds1, "character")
  expect_equal(sort(pds1), c("all", "controls", "exclosures"))

})






test_that(desc = "add_new_dataset makes a list", {

  dc <- add_new_dataset(main = main, new_dataset_controls = new_dataset_controls(metadata = new_dataset_metadata(name = "newone")))
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



