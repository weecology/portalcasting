context("Test portalcast functions")

options_all1 <- all_options(main = "testing_casting")
options_all2 <- all_options(main = "testing_casting", quiet = TRUE)
options_all3 <- all_options(main = "testing_casting", 
                            model = models(NULL, "AutoArima"))
options_all4 <- all_options(main = "testing_casting", 
                            model = models(NULL, "ok"))
options_all5 <- all_options(main = "testing_casting", 
                            model = models(NULL, "all"))
options_all6 <- all_options(main = "testing_casting", 
                            model = models(NULL, "AutoArima"),
                            cast_type = "hindcasts", end = 490)
options_all7 <- all_options(main = "testing_casting", 
                            model = models(NULL, "AutoArima"), quiet = TRUE)
options_all8 <- all_options(main = "testing_casting", 
                            model = models(NULL, "AutoArima"),
                            cast_type = "hindcasts", end = 496)
options_all9 <- all_options(main = "testing_casting", 
                            model = models(NULL, "AutoArima"),
                            cast_type = "hindcasts", end = 490:493)
options_all10 <- all_options(main = "testing_casting", 
                             model = models(NULL, "AutoArima"),
                             cast_type = "hindcasts", end = 476)
options_all11 <- all_options(main = "testing_casting", 
                             model = models(NULL, "AutoArima"),
                             cast_type = "hindcasts", end = 490:493, 
                             quiet = TRUE)

test_that("portalcast", {
  expect_error(portalcast(1), "`all_options`")
  expect_message(portalcast(options_all3))
})

test_that("verify_models", {
  expect_error(verify_models(1))
  expect_error(verify_models())
  expect_message(verify_models(options_all1$options_cast))
  expect_message(verify_models(options_all2$options_cast))
  expect_error(verify_models(options_all4$options_cast))
})

test_that("cast_models", {
  expect_error(cast_models(1))
  expect_message(cast_models(options_all3$options_cast))
  expect_message(cast_models(options_all6$options_cast))
})

test_that("models_to_cast", {
  expect_error(models_to_cast(1))
  mtc3 <- models_to_cast(options_all3$options_cast)
  expect_is(mtc3, "character")
  expect_equal(length(mtc3), 1)
  mtc5 <- models_to_cast(options_all5$options_cast)
  expect_is(mtc5, "character")
  expect_equal(length(mtc5), length(models()))
})

test_that("create_tmp", {
  expect_error(create_tmp(1))
  expect_silent(create_tmp(dirtree(main = "testing_casting")))
})

test_that("clear_tmp", {
  expect_error(clear_tmp(1))
  expect_silent(clear_tmp(dirtree(main = "testing_casting")))
  unlink(sub_path(dirtree(main = "testing_casting"), "tmp"), recursive = TRUE, 
         force = TRUE)
  unlink(sub_path(dirtree(main = "testing_casting"), "tmp"), recursive = TRUE, 
         force = TRUE)
  expect_message(clear_tmp(dirtree(main = "testing_casting")))
})

test_that("prep_data", {
  expect_error(prep_data(1))
  expect_output(prep_data(options_all1$options_data))
  unlink(file_path(dirtree(main = "testing_casting"), "data/metadata.yaml"))
  expect_output(prep_data(options_all1$options_data))

  metadata_path <- file_path(dirtree(main = "testing_casting"), 
                             "data/metadata.yaml")
  metadata <- yaml.load_file(metadata_path)    
  metadata$forecast_date <- "1970-01-01"
  writeLines(as.yaml(metadata), con = metadata_path)
  expect_output(prep_data(options_all1$options_data))
  expect_output(prep_data(options_all6$options_data))
})

test_that("casts", {
  expect_error(casts(1))
  unlink(sub_path(dirtree(main = "testing_casting"), "data"), 
         recursive = TRUE)
  create_sub_dir(sub_path(dirtree(main = "testing_casting"), "data"))
  prep_data(options_all3$options_data)
  expect_output(casts(options_all3))
  unlink(sub_path(dirtree(main = "testing_casting"), "data"), 
         recursive = TRUE)
  unlink(sub_path(dirtree(main = "testing_casting"), "models"), 
         recursive = TRUE)
  create_sub_dir(sub_path(dirtree(main = "testing_casting"), "data"))
  prep_data(options_all7$options_data)
  create_sub_dir(sub_path(dirtree(main = "testing_casting"), "models"))
  fill_models(options_all7$options_models)
  expect_silent(casts(options_all7))
})


test_that("cast", {
  expect_error(cast(1))
  unlink(sub_path(dirtree(main = "testing_casting"), "data"), 
         recursive = TRUE)
  create_sub_dir(sub_path(dirtree(main = "testing_casting"), "data"))
  prep_data(options_all3$options_data)
  expect_output(cast(options_all3$options_cast))

  unlink(sub_path(dirtree(main = "testing_casting"), "data"), 
         recursive = TRUE)
  unlink(sub_path(dirtree(main = "testing_casting"), "models"), 
         recursive = TRUE)
  create_sub_dir(sub_path(dirtree(main = "testing_casting"), "data"))
  prep_data(options_all7$options_data)
  create_sub_dir(sub_path(dirtree(main = "testing_casting"), "models"))
  fill_models(options_all7$options_models)
  expect_silent(cast(options_all7$options_cast))

  unlink(sub_path(dirtree(main = "testing_casting"), "data"), 
         recursive = TRUE)
  create_sub_dir(sub_path(dirtree(main = "testing_casting"), "data"))
  prep_data(options_all8$options_data)
  expect_message(cast(options_all8$options_cast))
  expect_equal(cast(options_all8$options_cast), NULL)
})


test_that("step_casts", {
  expect_error(step_casts(1))
  cast(options_all9$options_cast)
  expect_message(step_casts(options_all9))
})

test_that("step_hind_forward", {
  expect_error(step_hind_forward(1))
  expect_is(step_hind_forward(options_all6), "all_options")
})

test_that("check_to_skip", {
  expect_error(check_to_skip(1))
  expect_equal(check_to_skip(options_all8$options_cast), TRUE)
  expect_equal(check_to_skip(options_all10$options_cast), TRUE)
  expect_equal(check_to_skip(options_all6$options_cast), FALSE)
})

test_that("update_data", {
  expect_error(update_data(1))
  expect_message(update_data(options_all9$options_data))
  expect_silent(update_data(options_all11$options_data))
})

test_that("update_covariates", {
  moons <- prep_moons(options_all9$options_data$moons)
  expect_error(update_covariates(1, options_all9$options_data$covariates))
  expect_error(update_covariates(moons, 1))
  updcov <- update_covariates(moons, options_all9$options_data$covariates)
  expect_is(updcov, "covariates")
})

test_that("update_rodents", {
  expect_error(update_rodents(1))
  updrod <- update_rodents(options_all9$options_data$rodents)
  expect_is(updrod, "rodents_list")
  expect_is(updrod[[1]], "rodents")
  expect_is(updrod[[2]], "rodents")
})
