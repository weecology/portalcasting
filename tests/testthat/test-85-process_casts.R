context("Test process_casts functions")

bad_cast1 <- read.csv("bad_cast1.csv", stringsAsFactors = FALSE)
bad_cast2 <- read.csv("bad_cast2.csv", stringsAsFactors = FALSE)

test_that("verify_casts", {
  expect_error(verify_cast(bad_cast1))
  expect_error(verify_cast(na_conformer(bad_cast2)))
})

test_that("cast_is_valid", {
  expect_equal(cast_is_valid(bad_cast1), FALSE)
  expect_message(expect_equal(cast_is_valid(bad_cast1, TRUE), FALSE))
  expect_equal(cast_is_valid(bad_cast2), FALSE)
  expect_message(expect_equal(cast_is_valid(bad_cast2, TRUE), FALSE))
})

test_that("add_ensemble", {
  skip_on_cran() # downloads take too long for cran checks
  verify_models(main = "./testing", models = c("ESSS", "AutoArima"))
  min_lag <- extract_min_lag(models = c("ESSS", "AutoArima"))
  last_moon <- last_newmoon(main = "./testing")
  end_moon <- 520
  clear_tmp(main = "./testing") # not sure why this throws an error...
  prep_data(main = "./testing", min_lag = min_lag, end_moon = end_moon)
  models_scripts <- models_to_cast(main = "./testing", 
                                   models = c("ESSS", "AutoArima"))
  sapply(models_scripts, source)
  expect_is(combine_casts(main = "./testing", end_moon = end_moon), 
            "list")
  expect_is(add_ensemble(main = "./testing", end_moon = end_moon), 
            "data.frame")
})

test_that("make_ensemble", {
  skip_on_cran() # downloads take too long for cran checks
  end_moon <- 520
  temp_dir <- sub_paths(main = "./testing", "tmp")
  pred_dir <- sub_paths(main = "./testing", "predictions")
  filename_suffix <- "hindcast"
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)
  cclass <- c("Date", "integer", "integer", "integer", "character", 
              "character", "character", "character", "numeric",
              "numeric", "numeric", "integer", "integer", "integer")
  all_casts <- do.call(rbind, 
               lapply(files, read.csv, na.strings = "", colClasses  = cclass))
  expect_is(make_ensemble(all_casts = all_casts, main = "./testing") , 
            "data.frame")
})

test_that("compile_aic_weights", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(compile_aic_weights(main = "./testing"), "data.frame")
})

test_that("save_cast_output", {
  skip_on_cran() # downloads take too long for cran checks
  f_a <- AutoArima(tmnt_type = "All", main = "./testing")
  f_c <- AutoArima(tmnt_type = "Controls", main = "./testing")
  expect_silent(save_cast_output(f_a, f_c, "AutoArima", main = "./testing"))
})

test_that("select_most_ab_spp", {
  skip_on_cran() # downloads take too long for cran checks
  expect_is(select_most_ab_spp(main = "~/testing", model = "AutoArima"), 
             "character")
})


test_that("append_observed_to_cast", {
  skip_on_cran() # downloads take too long for cran checks
  casts <- read_casts(main = "./testing")
  casts_a <- select_casts(casts, tmnt_types = "All")
  casts_a <- append_observed_to_cast(main = "./testing", casts_a) 
  expect_is(casts_a, "data.frame")
})

test_that("measure_cast_error", {
  skip_on_cran() # downloads take too long for cran checks
  casts <- read_casts(main = "./testing") %>% 
           select_casts(tmnt_types = "Controls") %>%
           append_observed_to_cast(main = "./testing")
  expect_silent(casttab <- measure_cast_error(casts)) 
})

test_that("read_cast", {
  skip_on_cran() # downloads take too long for cran checks
  casts <- read_cast(main = "./testing") 
  expect_silent(casttab <- measure_cast_error(casts)) 
})

test_that("column conformer", {
  df <- data.frame(xforecastx = 1:10, fore = 2:11)
  expect_is(column_conformer(df), "data.frame")
})


test_that("most_recent_cast", {
  skip_on_cran() # downloads take too long for cran checks
   expect_is(most_recent_cast("./testing"), "Date")
})