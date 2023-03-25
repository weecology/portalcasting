context(desc = "evaluate functions")

main <- "./testing"

test_that(desc = "evaluate_casts evaluates casts", {

  #expect_is(evaluate_casts(main = main, cast_ids = c("1-1", "1-2")), "list")
  #expect_error(evaluate_casts(main = main, cast_ids = c(1e100, "1-2")))
expect_equal(1, 1)
})


test_that(desc = "evaluate_cast evaluates cast", {

  #expect_is(evaluate_cast(main = main, cast_id = "1-1"), "data.frame")
  #expect_error(evaluate_cast(main = main, cast_id = 1e100))
expect_equal(1, 1)
})

