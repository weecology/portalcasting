context("general utilities")


test_that("match.call.defaults operates within functions", {

  fun <- function(arg1 = "ok", ...) {
    match.call.defaults()
  }

  # simple default

    expect_is(fun(), "call")

  # elipsis input

    expect_is(fun(arg2 = "hi"), "call")
})


test_that("named_null_list produces a proper list", {

  nnl <- named_null_list(c("a", "b", "c"))

  # proper class, length, names, content
 
    expect_equal(class(nnl), "list")
    expect_equal(length(nnl), 3)
    expect_equal(names(nnl), c("a", "b", "c"))
    expect_equal(all(unlist(lapply(nnl, is.null))), TRUE)

})


test_that("error_if_deep catches deep but not shallow", {

  expect_error(error_if_deep(-1e4))
  expect_equal(error_if_deep(0), NULL)

})


test_that("update_list updates lists appropriately", {

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


test_that("na_conformer makes NA into `NA` in vectors and data frames", {

  # work on vectors

    xx <- c("a", "b", NA, "c")
    expect_equal(na_conformer(xx)[3], "NA")

  # works on dfs

    xx <- data.frame(w = "a", n = as.character(c("d", NA, "a", "b", "c")))
    expect_is(na_conformer(xx, "n"), "data.frame")
    expect_equal(na_conformer(xx, "n")[2,2], "NA")
})


test_that("append_csv properly appends and saves a csv, even if absent", {

  # nothing present yet

    df <- data.frame(x = 1:10, y = 11:20)
    expect_silent(append_csv(df, "ok.csv"))

    df_in <- read.csv("ok.csv") 
    nrow1 <- NROW(df_in)

  # with it present

    dfa <- data.frame(x = 100:102, y = 200:202)
    expect_silent(append_csv(dfa, "ok.csv"))
    dfap <- read.csv("ok.csv") 
    nrow2 <- NROW(dfap)

    expect_equal(nrow2 > nrow1, TRUE)

    unlink("ok.csv")
})


test_that("foy calculates the fraction of the year ", {

  expect_equal(foy("2020-01-01"), 0.003)
  expect_equal(foy("2020-07-30"), 0.579)
  expect_equal(foy("2020-12-31"), 1)

})



test_that("clear_tmp clears out the temp directory", {

  # this is weird because the nuances are in the second message reported, not
  # the first one, and i can't quite figure out how to check that right

  dir.create(file.path("tmp"))

  df <- data.frame(x = 1:10, y = 11:20)
  expect_silent(append_csv(df, file.path("tmp", "ok.csv")))  

  # allow for short circuit

    expect_equal(clear_tmp(cleanup = FALSE), NULL)

  # clear, with messaging verbose and double clearing

    expect_message(clear_tmp(), 
                   "Clearing tmp subdirectory")
    expect_silent(append_csv(df, file.path("tmp", "ok.csv")))  
    expect_message(clear_tmp(verbose = TRUE), 
                     "Clearing tmp subdirectory")
    expect_message(clear_tmp(verbose = TRUE), 
                     "Clearing tmp subdirectory")

  # expect message when not present

    unlink(file.path("tmp"), recursive = TRUE, force = TRUE)
    expect_message(clear_tmp(verbose = TRUE), 
                     "Clearing tmp subdirectory")

})

test_that("combine_hist_and_cast correctly combines overlapping dataframes", {

  hist_tab <- data.frame(date = seq(Sys.Date(), Sys.Date() + 5, 1), x = 1:6)
  cast_tab <- data.frame(date = seq(Sys.Date() + 5, Sys.Date() + 10, 1),
                         x = 101:106)

  # hist wins then cast wins

    df <- combine_hist_and_cast(hist_tab, cast_tab, "hist")
    expect_is(df, "data.frame")
    expect_equal(df$x[6], 6)

    df <- combine_hist_and_cast(hist_tab, cast_tab, "cast")
    expect_is(df, "data.frame")
    expect_equal(df$x[6], 101)

  # errors properly with incorrect winner

    expect_error(combine_hist_and_cast(hist_tab, cast_tab, "c123")) 
})

test_that("add_date_from_components combines parts into date", {
  df <- data.frame(year = 2010, month = 2, day = 1:10)
  df2 <- add_date_from_components(df)
  expect_is(df2, "data.frame")
  expect_equal(df2$date, 
               as.Date(c("2010-02-01", "2010-02-02", "2010-02-03", 
                         "2010-02-04", "2010-02-05", "2010-02-06", 
                         "2010-02-07", "2010-02-08", "2010-02-09",
                         "2010-02-10")))
})


test_that("remove_incompletes does remove them", {
  df <- data.frame(c1 = c(1:9, NA), c2 = 11:20)
  df2 <- remove_incompletes(df, "c1")
  expect_is(df2, "data.frame")
  expect_equal(NROW(df2), 9)
})


test_that("list_depth properly measures the depth of a list", {
  expect_equal(list_depth("a"), 0)
  expect_equal(list_depth(list("a")), 1)
  expect_equal(list_depth(list()), 0)
  expect_equal(list_depth(list(list("a"))), 2)
})

test_that("return_if_null properly returns for a new function", {
  ff <- function(x = 1, null_return = "hello"){
    return_if_null(x, null_return)
    x
  }
  expect_equal(ff(), 1)
  expect_equal(ff(NULL), "hello")
})

test_that("ifnull toggles based on input", {
  expect_equal(ifnull(NULL, 123), 123)
  expect_equal(ifnull(TRUE, 123), TRUE)
})

test_that("ifna toggles based on input", {
  expect_equal(ifna(NA, 123), 123)
  expect_equal(ifna(FALSE, 123), FALSE)
  expect_equal(ifna(NA, NA), NA)
})
