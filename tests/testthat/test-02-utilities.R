context(desc = "general utilities")


test_that(desc = "round_na.interp does its job",
          code = {

  expect_is(round_na.interp(x = c(-1, 2, 3, NA, NA, 170), min_val = 1), "numeric")
  expect_equal(min(round_na.interp(x = c(-1, 2, 3, NA, NA, 170), min_val = 1)), 1)

  expect_is(round_na.interp(x = c(-1, 2, 3, NA, NA, 170)), "numeric")
  expect_equal(min(round_na.interp(x = c(-1, 2, 3, NA, NA, 170))), 0)

  expect_equal(any(is.na(round_na.interp(x = c(-1, 2, 3, NA, NA, 170)))), FALSE)
  expect_equal(all(round_na.interp(x = c(-1, 2, 3, NA, NA, 170)) %% 1 == 0), TRUE)
})



test_that(desc = "named_null_list produces a proper list",
          code = {

  nnl <- named_null_list(c("a", "b", "c"))

  # proper class, length, names, content
 
    expect_equal(class(nnl), "list")
    expect_equal(length(nnl), 3)
    expect_equal(names(nnl), c("a", "b", "c"))
    expect_equal(all(unlist(lapply(nnl, is.null))), TRUE)

})


test_that(desc = "update_list updates lists appropriately",
          code = {

  orig_list <- list(a = 1, b = 3, c = 4)
  orig_list_same <- update_list(orig_list)
  new_list <- update_list(orig_list, a = 10, b = "b")

  # verify class and content 

    expect_is(orig_list_same, "list")
    expect_is(new_list, "list")
    expect_equal(orig_list, orig_list_same)
    expect_equal(identical(new_list, orig_list), FALSE)

  # throw error if not a list

    expect_error(update_list("a"))
})


test_that(desc = "foy calculates the fraction of the year ",
          code = {

  expect_equal(foy("2020-01-01"), 0.003)
  expect_equal(foy("2020-07-30"), 0.579)
  expect_equal(foy("2020-12-31"), 1)

})



test_that(desc = "return_if_null properly returns for a new function",
          code = {
  ff <- function(x = 1, null_return = "hello"){
    return_if_null(x, null_return)
    x
  }
  expect_equal(ff(), 1)
  expect_equal(ff(NULL), "hello")
})

test_that(desc = "ifnull toggles based on input",
          code = {
  expect_equal(ifnull(NULL, 123), 123)
  expect_equal(ifnull(TRUE, 123), TRUE)
})
