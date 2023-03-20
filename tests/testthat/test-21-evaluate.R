context(desc = "evaluate functions")

main <- "./testing"

test_that(desc = "evaluate_casts evaluates casts", {

  expect_is(evaluate_casts(main = main, cast_ids = c("1-1", "1-2")), "list")

})


test_that(desc = "evaluate_cast evaluates cast", {

  expect_is(evaluate_cast(main = main, cast_id = "1-1"), "data.frame")

})

test_that(desc = "placeholder", {

  expect_equal(1, 1)

})