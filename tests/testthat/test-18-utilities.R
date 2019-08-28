context("Test utility functions")

test_that("named_null_list", {
  expect_is(named_null_list(c("a", "b", "c")), "list")
  expect_equal(length(named_null_list(c("a", "b", "c"))), 3)

})



test_that("clear_tmp", {
  expect_message(clear_tmp(""))
})

test_that("na_conformer", {
  expect_equal(na_conformer(c("a", "b", NA, "c"))[3], "NA")
  xx <- data.frame(w = "a", n = as.character(c("d", NA, "a", "b", "c")),
                   stringsAsFactors = FALSE)
  expect_is(na_conformer(xx, "n"), "data.frame")
  expect_equal(na_conformer(xx, "n")[2,2], "NA")
})

test_that("cast0", {
  expect_is(cast0(12), "list")
})

test_that("foy", {
  expect_is(foy(Sys.Date()), "numeric")
})

test_that("append_csv",{
  df <- data.frame(x = 1:10)
  fpath <- file_path(main = "./testing", NULL, "xx.csv")
  expect_equal(append_csv(df, fpath), NULL)
})

test_that("cast_window", {
  skip_on_cran() # downloads take too long for cran checks
  wind <- cast_window(main = "./testing")
  expect_is(wind, "list")
  expect_equal(names(wind), c("start", "end"))
})


test_that("combine_hist_and_cast", {
  hist_tab <- data.frame(date = Sys.Date(), x = 1:10)
  cast_tab <- data.frame(date = Sys.Date(), x = 101:110)
  expect_is(combine_hist_and_cast(hist_tab, cast_tab, "hist"), "data.frame")
  expect_is(combine_hist_and_cast(hist_tab, cast_tab, "cast"), "data.frame")
  expect_error(combine_hist_and_cast(hist_tab, cast_tab, "c123")) 
})

test_that("add_date_from_components", {
  df <- data.frame(year = 2010, month = 2, day = 1:10)
  expect_is(add_date_from_components(df), "data.frame")
})

test_that("error_if_deep", {

  expect_error(error_if_deep(-1e4))
  expect_equal(error_if_deep(0), NULL)
})

test_that("update_list", {
  orig_list <- list(a = 1, b = 3, c = 4)
  expect_is(update_list(orig_list), "list")
  expect_is(update_list(orig_list, a = "a"), "list")
  expect_is(update_list(orig_list, a = 10, b = NULL), "list")
  expect_error(update_list("a"))
})

test_that("remove_incompletes", {
  df <- data.frame(c1 = c(1:9, NA), c2 = 11:20)
  expect_is(remove_incompletes(df, "c1"), "data.frame")
})

test_that("data_out", {
  skip_on_cran()
  expect_silent(prep_moons(main = "./testing"))
  expect_silent(prep_moons(main = "./testing", overwrite = FALSE))
})

test_that("ifnull", {
  expect_equal(ifnull(NULL, 123), 123)
  expect_equal(ifnull(TRUE, 123), TRUE)
})

test_that("ifna", {
  expect_equal(ifna(NA, 123), 123)
  expect_equal(ifna(FALSE, 123), FALSE)
  expect_equal(ifna(NA, NA), NA)
})
test_that("messageq", {
  expect_silent(messageq("ok", TRUE))
  expect_message(messageq("ok"))
})

test_that("list_depth", {
  expect_equal(list_depth("a"), 0)
  expect_equal(list_depth(list("a")), 1)
  expect_equal(list_depth(list()), 0)
  expect_equal(list_depth(list(list("a"))), 2)
})

test_that("return_if_null", {
  ff <- function(x = 1, null_return = "hello"){
    return_if_null(x, null_return)
    x
  }
  expect_equal(ff(), 1)
  expect_equal(ff(NULL), "hello")
})


test_that("data_out", {
 expect_error(data_out(list(1), filename = "123.rtrr"))
})
