context(desc = "evaluate functions")

main <- "./testing"

test_that(desc = "evaluate_casts evaluates casts", {

  expect_null(evaluate_casts(main = main))

})


test_that(desc = "evaluate_cast evaluates cast", {

  expect_null(evaluate_cast(main = main))

})

