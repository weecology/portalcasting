context(desc = "evaluate functions")

main <- "./testing"

test_that(desc = "evaluate_casts evaluates casts", {

  cast_ids <- select_casts(main)$cast_id
  nids <- length(cast_ids)
  nsample_ids <- 100
  cast_ids <- cast_ids[round(seq(1, nids, length.out = nsample_ids))]
  expect_is(evaluate_casts(main = main, cast_ids = cast_ids), "data.frame")
  expect_error(evaluate_casts(main = main, cast_ids = c(1e100, 2e100)))

})


test_that(desc = "evaluate_cast evaluates cast", {

  cast_ids <- select_casts(main)$cast_id
  cast_id <- cast_ids[length(cast_ids)]

  expect_is(evaluate_cast(main = main, cast_id = cast_id), "data.frame")
  expect_error(evaluate_cast(main = main, cast_id = 1e100))

})

