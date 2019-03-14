context("Test directory functions")

test_location <- "travis"

test_that("setup_dir", {
  expect_error(setup_dir(1), "`options_all`")
  expect_message(setup_dir(all_options(main = "testing_casting")))
})

test_that("create_dir", {
  expect_error(create_dir(1), "`options_dir`")
})

test_that("create_main_dir", {
  expect_error(create_main_dir(1), "`options_dir`")
  expect_message(create_main_dir(dir_options(main = "ok")))
})

test_that("create_sub_dirs", {
  expect_error(create_sub_dirs(1), "`options_dir`")
})

test_that("create_sub_dir", {
  expect_error(create_sub_dir(1), "`sub_path`")
  expect_equal(create_sub_dir(), NULL)
  expect_message(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1]))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
  create_main_dir(dir_options(main = "ok"))
  expect_silent(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1],
                               quiet = TRUE))
  expect_silent(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1]))
})

test_that("fill_dir", {
  expect_error(fill_dir(1), "`options_all`")
})

test_that("cleanup_dir", {
  expect_error(cleanup_dir(1), "`options_all`")
  opts1 <- all_options(main = "testing_casting", to_cleanup = NULL)
  opts2 <- all_options(main = "testing_casting", to_cleanup = NULL, 
                       quiet = TRUE)
  opts3 <- all_options(main = "testing_casting")
  opts4 <- all_options(main = "testing_casting", quiet = TRUE)
  expect_message(cleanup_dir(opts1))
  expect_silent(cleanup_dir(opts2))
  expect_message(cleanup_dir(opts3))
  expect_silent(cleanup_dir(opts4))
})

test_that("fill_PortalData", {
  expect_error(fill_PortalData(1), "`options_PortalData`")
  pd_opts <- PortalData_options(main = "testing_casting", quiet = TRUE)
  expect_silent(fill_PortalData(pd_opts))
})

test_that("fill_data", {
  expect_error(fill_data(1), "`options_data`")
})

test_that("fill_predictions", {
  expect_error(fill_predictions(1), "`options_predictions`")
  if(test_location == "local"){
    opts <- all_options(main = "testing_casting", 
                        download_existing_predictions = TRUE)
    fill_predictions(opts$options_predictions)
  }
})

test_that("fill_models", {
  expect_error(fill_models(1), "`options_models`")
})

test_that("verify_PortalData", {
  expect_error(verify_PortalData(1))
  expect_error(verify_PortalData(dirtree(main = "testing_casting"), 1))
  cleanup_dir(all_options(main = "testing_casting"))
  expect_message(verify_PortalData(dirtree(main = "testing_casting")))
})


unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)