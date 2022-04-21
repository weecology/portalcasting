context(desc = "evaluate functions")

main <- "./testing"

test_that(desc = "evaluate_casts evaluates casts", {

  expect_is(evaluate_casts(main = main, cast_ids = 1:2), "list")

})


test_that(desc = "evaluate_cast evaluates cast", {

  expect_null(evaluate_cast(main = main))
  expect_is(evaluate_cast(main = main, cast_id = 1), "data.frame")

})

