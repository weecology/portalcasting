context(desc = "evaluate functions")

main1 <- file.path(tempdir(), "testing1")
main2 <- file.path(tempdir(), "testing2")
main3 <- file.path(tempdir(), "testing3")


test_that(desc = "evaluate_casts evaluates casts", {

  cast_ids <- select_casts(main = main3, species = c("DM", "PP", "total"), models = c("AutoArima", "ESSS", "pevGARCH", "nbGARCH"), datasets = c("all", "controls"))$cast_id
  nids <- length(cast_ids)
  nsample_ids <- 1000
  cast_ids <- cast_ids[round(seq(1, nids, length.out = nsample_ids))]
  expect_message(ec <- evaluate_casts(main = main3, cast_ids = cast_ids))
  expect_is(ec, "data.frame")
  expect_error(evaluate_casts(main = main3, cast_ids = c(1e100, 2e100)))

})


test_that(desc = "evaluate_cast evaluates cast", {

  cast_ids <- select_casts(main = main3)$cast_id
  cast_id <- cast_ids[length(cast_ids)]

  expect_is(evaluate_cast(main = main3, cast_id = cast_id), "data.frame")
  expect_error(evaluate_cast(main = main3, cast_id = 1e100))

})
