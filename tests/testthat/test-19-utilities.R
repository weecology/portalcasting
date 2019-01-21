context("Test utilities functions")

test_that("dataout", {
  df <- data.frame(x = 1:10, y = 11:20)
  expect_error(dataout(1, moons_options()))
  expect_equal(dataout(df, moons_options(save = FALSE)), df)

  fp <- file_path(dirtree(main = ""), "ok.csv")
  fp1 <- gsub("ok.csv", "", fp)
  fp2 <- paste0(fp1, "data")
  dir.create(fp2)
  options_list <- moons_options(tree = dirtree(main = ""), 
                                filename = "ok.csv")
  expect_equal(dataout(df, options_list), df)
  unlink(fp2, recursive = TRUE, force = TRUE)
})

test_that("append_csv", {
  df <- data.frame(x = 1:10, y = 11:20)
  expect_error(append_csv(10, "ok.csv"))
  expect_error(append_csv(df, c("ok", "ok")))
  expect_error(append_csv(df, 10))
  expect_silent(append_csv(df, "ok.csv"))
  unlink("ok.csv")
})

test_that("fcast0", {
  expect_is(fcast0(1), "list")
  expect_equal(length(fcast0(1)), 2)
  expect_equal(length(fcast0(1)[[1]]), 1)
  expect_equal(length(fcast0(10)[[1]]), 10)
  expect_equal(nrow(fcast0(1)[[2]]), 1)
  expect_equal(nrow(fcast0(10)[[2]]), 10)
  expect_error(fcast0(1.5))
  expect_error(fcast0(1:10))
  expect_error(fcast0(1, c("ok", "ok")))
  expect_error(fcast0("ok"))
  expect_error(fcast0(1, 1))
})

test_that("today", {
  expect_equal(today(), Sys.Date())
})