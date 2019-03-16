context("Test process_forecasts functions")

fill_data(data_options(main = "testing_casting"))
tree <- dirtree(main = "testing_casting");
all <- read_data(tree, "all");
controls <-read_data(tree, "controls");
metadata <- read_data(tree, "metadata");
f_a <- AutoArima(tree, level = "All", quiet = FALSE);
f_c <- AutoArima(tree, level = "Controls", quiet = FALSE);
f_a2 <- ESSS(tree, level = "All", quiet = FALSE);
f_c2 <- ESSS(tree, level = "Controls", quiet = FALSE);
f_ax <- f_a
names(f_ax)[1] <- "ok"
f_cx <- f_c
names(f_cx)[1] <- "ok"
cast_date <- as.Date(metadata$forecast_date)

test_that("save_forecast_output", {
  expect_error(save_forecast_output(1, f_c, "AutoArima", tree))
  expect_error(save_forecast_output(f_a, 1, "AutoArima", tree))
  expect_error(save_forecast_output(f_a, f_c, 1, tree))
  expect_error(save_forecast_output(f_a, f_c, rep("AutoArima", 2), tree))
  expect_error(save_forecast_output(f_a, f_c, "AutoArima", 1))
  expect_error(save_forecast_output(f_ax, f_c, "AutoArima", tree))
  expect_error(save_forecast_output(f_a, f_cx, "AutoArima", tree))

  expect_silent(save_forecast_output(f_a, f_c, "AutoArima", tree))
  expect_silent(save_forecast_output(f_a2, f_c2, "ESSS", tree))
})

cast_opts1 <- cast_options(main = "testing_casting")

test_that("combine_forecasts", {
  expect_error(combine_forecasts(1))
  expect_message(comb_fcast1 <- combine_forecasts(cast_opts1))
  expect_is(comb_fcast1, "list")
  expect_equal(names(comb_fcast1), c("forecasts", "all_model_aic"))
})

test_that("add_ensemble", {
  expect_error(add_ensemble(1))
  expect_message(ensmb1 <- add_ensemble(cast_opts1))
  expect_is(ensmb1, "data.frame")
  cast_opts3 <- cast_opts1
  cast_opts3$ensemble <- FALSE
  expect_silent(ensmb3 <- add_ensemble(cast_opts3))
  expect_equal(ensmb3, NULL)
})

sub1 <- sub_paths(cast_opts1$tree, "predictions")
sub2 <- sub_paths(cast_opts1$tree, "PortalData")

test_that("compile_aic_weights", {
  expect_error(compile_aic_weights(1))
  expect_error(compile_aic_weights(c(sub1, sub1)))
  expect_error(compile_aic_weights(sub2))
  expect_silent(weights <- compile_aic_weights(sub1))
  expect_is(weights, "data.frame")
})

test_that("make_ensemble", {
  temp_dir <- sub_paths(cast_opts1$tree, "tmp")
  pred_dir <- sub_paths(cast_opts1$tree, "predictions")
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
  expect_error(make_ensemble(fcasts, 1))
  expect_error(make_ensemble(fcasts, c(sub1, sub1)))
  expect_error(make_ensemble(1, sub1))
  expect_error(make_ensemble(fcasts, sub1, "ok"))
  expect_error(make_ensemble(fcasts, sub1, 1:2))
  expect_error(make_ensemble(fcasts, sub1, -1))
})

test_that("read_cast", {
  expect_silent(casts <- read_cast(cast_opts1$tree))
  expect_is(casts, "casts")
  expect_silent(casts2 <- read_cast(cast_opts1$tree, cast_date = cast_date))
  expect_is(casts2, "casts")
  expect_equal(casts, casts2)
  expect_error(read_cast(1))
  expect_error(read_cast(cast_opts1$tree, cast_type = 1))
  expect_error(read_cast(cast_opts1$tree, cast_type = rep("forecasts", 2)))
  expect_error(read_cast(cast_opts1$tree, cast_type = "ok"))
  expect_error(read_cast(cast_opts1$tree, cast_date = rep(today(), 2)))
  expect_error(read_cast(cast_opts1$tree, cast_date = "ok"))
  expect_error(read_cast(cast_opts1$tree, cast_date = "2020-01-01"))
})

test_that("select_casts", {
  casts <- read_cast(cast_opts1$tree)
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

  expect_error(select_casts(1))
  expect_error(select_casts(casts, species = 1))
  expect_error(select_casts(casts, species = "ok"))
  expect_error(select_casts(casts, level = 1))
  expect_error(select_casts(casts, level = "ok"))
  expect_error(select_casts(casts, model = 1))
  expect_error(select_casts(casts, newmoonnumber = -1))
  expect_error(select_casts(casts, newmoonnumber = 300.5))
  expect_error(select_casts(casts, newmoonnumber = "ok"))
})

test_that("most_recent_cast", {
  expect_silent(cdat <- most_recent_cast(cast_opts1$tree))
  expect_is(cdat, "Date")
  expect_error(most_recent_cast(1))
  expect_error(most_recent_cast(cast_opts1$tree, cast_type = 1))
  expect_error(most_recent_cast(cast_opts1$tree, 
                                cast_type = rep("forecasts", 2)))
  expect_error(most_recent_cast(cast_opts1$tree, cast_type = "ok"))
  expect_error(most_recent_cast(cast_opts1$tree, with_census = "ok"))
  expect_error(most_recent_cast(cast_opts1$tree, with_census = rep(TRUE, 2)))
  expect_error(most_recent_cast())
})

test_that("read_casts", {
  expect_silent(casts <- read_casts(cast_opts1$tree))
  expect_is(casts, "casts")
  expect_silent(casts2 <- read_casts(cast_opts1$tree, cast_dates = cast_date))
  expect_is(casts2, "casts")
  expect_error(read_casts(1))
  expect_error(read_casts(cast_opts1$tree, cast_type = 1))
  expect_error(read_casts(cast_opts1$tree, cast_type = rep("forecasts", 2)))
  expect_error(read_casts(cast_opts1$tree, cast_type = "ok"))
  expect_error(read_casts(cast_opts1$tree, cast_dates = "ok"))
})

bad_cast1 <- read.csv("bad_cast1.csv", stringsAsFactors = FALSE)
bad_cast2 <- read.csv("bad_cast2.csv", stringsAsFactors = FALSE)

test_that("verify_casts", {
  expect_error(verify_cast(bad_cast1))
  expect_output(expect_error(verify_cast(bad_cast1, TRUE)))
  expect_error(verify_cast(na_conformer(bad_cast2)))
})

test_that("cast_is_valid", {
  expect_equal(cast_is_valid(bad_cast1), FALSE)
  expect_output(expect_equal(cast_is_valid(bad_cast1, TRUE), FALSE))
  expect_equal(cast_is_valid(bad_cast2), FALSE)
  expect_output(expect_equal(cast_is_valid(bad_cast2, TRUE), FALSE))
})


test_that("append_observed_to_cast", {
  casts <- read_cast(cast_opts1$tree)
  casts1 <- select_casts(casts, level = "Controls")
  expect_silent(append_observed_to_cast(casts1, cast_opts1$tree))
  expect_error(append_observed_to_cast(casts, cast_opts1$tree))
  expect_error(append_observed_to_cast(casts, 1))
  expect_error(append_observed_to_cast(1, cast_opts1$tree))
  expect_error(append_observed_to_cast(casts, cast_opts1$tree, 
                                       add_error = 1))
  expect_error(append_observed_to_cast(casts, cast_opts1$tree, 
                                       add_error = rep(T,2)))
  expect_error(append_observed_to_cast(casts, cast_opts1$tree, 
                                       add_in_window = 1))
  expect_error(append_observed_to_cast(casts, cast_opts1$tree, 
                                       add_in_window = rep(T,2)))
  expect_error(append_observed_to_cast(casts, cast_opts1$tree, 
                                       add_lead = 1))
  expect_error(append_observed_to_cast(casts, cast_opts1$tree, 
                                       add_lead = rep(T,2)))
})

test_that("measure_cast_error", {
  casts <- read_cast(cast_opts1$tree) %>% 
           select_casts(level = "Controls") %>%
           append_observed_to_cast(cast_opts1$tree)
  expect_silent(casttab <- measure_cast_error(casts)) 
  expect_error(measure_cast_error(1))
  expect_error(measure_cast_error(casts, 1.5))
  expect_error(measure_cast_error(casts, 1:2))
  expect_error(measure_cast_error(casts, "ok"))
})



test_that("select_most_ab_spp", {
  expect_silent(spp <- select_most_ab_spp(tree = tree, cast_date = cast_date))
  expect_is(spp, "character")
  expect_equal(length(spp), 3)
  expect_silent(spp2 <- select_most_ab_spp(tree = tree, cast_date = NULL))
  expect_is(spp2, "character")
  expect_equal(length(spp2), 3)
  expect_equal(spp, spp2)
  expect_error(select_most_ab_spp(topx = "ok", 
      tree = tree, cast_date = cast_date))
  expect_error(select_most_ab_spp(topx = 1:2, 
      tree = tree, cast_date = cast_date))
  expect_error(select_most_ab_spp(topx = 1.2, 
      tree = tree, cast_date = cast_date))
  expect_error(select_most_ab_spp(tree = 1))
  expect_error(select_most_ab_spp(tree = tree, species = 1))
  expect_error(select_most_ab_spp(tree = tree, species = "ok"))
  expect_error(select_most_ab_spp(tree = tree, level = 1))
  expect_error(select_most_ab_spp(tree = tree, level = "ok" ))
  expect_error(select_most_ab_spp(tree = tree, level = rep("All", 2)))
  expect_error(select_most_ab_spp(tree = tree, cast_type = 1))
  expect_error(select_most_ab_spp(tree = tree, cast_type = "ok"))
  expect_error(
     select_most_ab_spp(tree = tree, cast_type = rep("forecasts", 2)))
  expect_error(select_most_ab_spp(tree = tree, cast_date = 1))
  expect_error(select_most_ab_spp(tree = tree, cast_date = rep(cast_date, 2)))
  expect_error(select_most_ab_spp(tree = tree, lead = 0))
  expect_error(select_most_ab_spp(tree = tree, lead = 1:2))
  expect_error(select_most_ab_spp(tree = tree, lead = 1.5))
  expect_error(select_most_ab_spp(tree = tree, from_date = 1))
  expect_error(select_most_ab_spp(tree = tree, from_date = rep(cast_date, 2)))

})

