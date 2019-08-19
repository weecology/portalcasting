context("Test utility functions")

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
  fpath <- file_paths(main = "./testing", "xx.csv")
  expect_equal(append_csv(df, fpath), NULL)
})

test_that("cast_window", {
  skip_on_cran() # downloads take too long for cran checks
  create_dir(main = "./testing")
  #fill_raw(main = "./testing")
  cast_window(main = "./testing")
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

test_that("pass_and_call", {

  yy <- function(n = 1, z = sqrt(w), w = 40){
           pass_and_call(rnorm, mean = z, sd  = w)
         }

  yyy <- function(d = 43){
            pass_and_call(yy, n = d)
          }
  expect_is(yy(), "numeric")
  expect_is(yy(w = 30), "numeric")
  expect_is(yy(z = 0, w = 1), "numeric")
  expect_equal(length(yyy()), 43)
  expect_equal(length(yyy(d = 2)), 2)

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
  expect_message(prep_moons(main = "./testing"))
  expect_message(prep_moons(main = "./testing", overwrite = FALSE))
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
unlink(main_path(main = "./testing"), recursive = TRUE, force = TRUE)