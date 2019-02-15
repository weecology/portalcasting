context("Test process_forecasts functions")

fill_data(data_options(main = "testing_casting"))
tree <- dirtree(main = "testing_casting");
all <- read_data(tree, "all");
controls <-read_data(tree, "controls");
metadata <- read_data(tree, "metadata");
f_a <- AutoArima(all, metadata, quiet = FALSE);
f_c <- AutoArima(controls, metadata, level = "Controls", quiet = FALSE);
f_a2 <- ESSS(all, metadata, quiet = FALSE);
f_c2 <- ESSS(controls, metadata, level = "Controls", quiet = FALSE);
f_ax <- f_a
names(f_ax)[1] <- "ok"
f_cx <- f_c
names(f_cx)[1] <- "ok"

test_that("save_forecast_output", {
  expect_error(save_forecast_output(1, f_c, "AutoArima", metadata, 
                                     sub_path(tree, "tmp")))
  expect_error(save_forecast_output(f_a, 1, "AutoArima", metadata, 
                                     sub_path(tree, "tmp")))
  expect_error(save_forecast_output(f_a, f_c, 1, metadata, 
                                     sub_path(tree, "tmp")))
  expect_error(save_forecast_output(f_a, f_c, rep("AutoArima", 2), metadata, 
                                     sub_path(tree, "tmp")))
  expect_error(save_forecast_output(f_a, f_c, "AutoArima", 1, 
                                     sub_path(tree, "tmp")))
  expect_error(save_forecast_output(f_a, f_c, "AutoArima", metadata, 1)) 
  expect_error(save_forecast_output(f_a, f_c, "AutoArima", metadata, 
                                    c("a", "b"))) 
  expect_error(save_forecast_output(f_ax, f_c, "AutoArima", metadata, 
                                     sub_path(tree, "tmp")))
  expect_error(save_forecast_output(f_a, f_cx, "AutoArima", metadata, 
                                     sub_path(tree, "tmp")))

  expect_silent(save_forecast_output(f_a, f_c, "AutoArima", metadata, 
                                     sub_path(tree, "tmp")))
  expect_silent(save_forecast_output(f_a2, f_c2, "ESSS", metadata, 
                                     sub_path(tree, "tmp")))
})

cast_opts1 <- cast_options(main = "testing_casting")
cast_opts2 <- cast_options(main = "testing_casting", quiet = TRUE)

test_that("combine_forecasts", {
  expect_error(combine_forecasts(1), "`cast_options` not of class")
  expect_message(comb_fcast1 <- combine_forecasts(cast_opts1))
  expect_silent(comb_fcast2 <- combine_forecasts(cast_opts2))
  expect_equal(comb_fcast1, comb_fcast2)
  expect_is(comb_fcast1, "list")
  expect_equal(names(comb_fcast1), c("forecasts", "all_model_aic"))
})

test_that("add_ensemble", {
  expect_error(add_ensemble(1), "`cast_options` not of class")
  expect_message(ensmb1 <- add_ensemble(cast_opts1))
  expect_silent(ensmb2 <- add_ensemble(cast_opts2))
  expect_equal(ensmb1, ensmb2)
  expect_is(ensmb1, "data.frame")
  cast_opts3 <- cast_opts1
  cast_opts3$ensemble <- FALSE
  expect_silent(ensmb3 <- add_ensemble(cast_opts3))
  expect_equal(ensmb3, NULL)
})

sub1 <- sub_path(cast_opts1$tree, "predictions")
sub2 <- sub_path(cast_opts1$tree, "PortalData")

test_that("compile_aic_weights", {
  expect_error(compile_aic_weights(1), "`pred_dir` is")
  expect_error(compile_aic_weights(c(sub1, sub1)), "`pred_dir` can")
  expect_error(compile_aic_weights(sub2))
  expect_silent(weights <- compile_aic_weights(sub1))
  expect_is(weights, "data.frame")
})

test_that("make_ensemble", {
  temp_dir <- sub_path(cast_opts1$tree, "tmp")
  pred_dir <- sub_path(cast_opts1$tree, "predictions")
  forecast_date <- cast_opts1$fdate
  filename_suffix <- cast_opts1$cast_type
  file_ptn <- paste(filename_suffix, ".csv", sep = "")
  files <- list.files(temp_dir, pattern = file_ptn, full.names = TRUE)
  cclass <- c("Date", "integer", "integer", "integer", "character", 
              "character", "character", "character", "numeric",
              "numeric", "numeric", "integer", "integer", "integer")
  fcasts <- do.call(rbind, 
            lapply(files, read.csv, na.strings = "", colClasses  = cclass))
  expect_silent(ensemble <- make_ensemble(fcasts, sub1, 0.9))
  expect_is(ensemble, "data.frame")  
  expect_error(make_ensemble(fcasts, 1), "`pred_dir` is")
  expect_error(make_ensemble(fcasts, c(sub1, sub1)), "`pred_dir` can")
  expect_error(make_ensemble(1, sub1), "`all_forecasts` is")
  expect_error(make_ensemble(fcasts, sub1, "ok"), "`CI_level` is")
  expect_error(make_ensemble(fcasts, sub1, 1:2), "`CI_level` can")
  expect_error(make_ensemble(fcasts, sub1, -1), "`CI_level` is")
})

test_that("read_casts", {
  expect_silent(casts <- read_casts(cast_opts1$tree))
  expect_is(casts, "casts")
})

test_that("select_casts", {
  casts <- read_casts(cast_opts1$tree)
  expect_silent(cast <- select_casts(casts))
  expect_is(cast, "casts")
  expect_silent(cast <- select_casts(casts, species = "total"))
  expect_is(cast, "casts")
  expect_silent(cast <- select_casts(casts, level = "All"))
  expect_is(cast, "casts")
  expect_silent(cast <- select_casts(casts, model = "Ensemble"))
  expect_is(cast, "casts")

  metadata <- read_data(cast_opts1$tree, "metadata")
  nmn <- metadata$rodent_forecast_newmoons
  expect_silent(cast <- select_casts(casts, newmoonnumber = nmn))
  expect_is(cast, "casts")
})
