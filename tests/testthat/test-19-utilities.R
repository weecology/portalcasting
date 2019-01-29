context("Test utilities functions")

test_that("dataout", {
  df <- data.frame(x = 1:10, y = 11:20)
  dfo <- dataout(df, moons_options(save = FALSE))
  fp <- file_path(dirtree(main = ""), "ok.csv")
  fp1 <- gsub("ok.csv", "", fp)
  fp2 <- paste0(fp1, "data")
  dir.create(fp2)
  options_list <- moons_options(tree = dirtree(main = ""), 
                                filename = "ok.csv")
  dfo2 <- dataout(df, options_list)
  df <- classy(df, c("moons", "data.frame"))
  expect_error(dataout(1, moons_options()))
  expect_equal(dfo, df)
  expect_equal(dfo2, df)
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
  expect_equal(today(time = TRUE), Sys.time())
})

test_that("classy", {
  expect_error(classy(1, 1))
  expect_is(classy(1, "character"), "character")
  expect_is(classy(1, c("ok", "character")), c("character"))
  expect_is(classy(1, c("ok", "character")), c("ok"))
})

test_that("remove_incompletes", {
  df <- data.frame(ok = 1:10, not_ok = 11:20)
  expect_error(remove_incompletes(1, "ok"))
  expect_error(remove_incompletes(df, 1))
  expect_error(remove_incompletes(df, "ok2"))
  expect_error(remove_incompletes(df, c("ok", "ok")))
  expect_is(remove_incompletes(df, "ok"), "data.frame")
})