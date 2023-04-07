context(desc = "ensembling functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")

test_that(desc = "ensemble_casts ensembles accordingly", {

  skip_on_cran() 

  cast_ids <- select_casts(main3)$cast_id
  cast_id <- cast_ids[length(cast_ids)]

  expect_is(ensemble_casts(main     = main3, 
                           cast_ids = cast_id), "data.frame")
  expect_error(ensemble_casts(main = main3, cast_id = 1e10))

  expect_error(ensemble_casts(main = main3, historic_end_newmoonnumber = 1e10))

})