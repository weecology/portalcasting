context("Test directory functions")

test_location <- "travis"

test_that("setup_dir", {
  expect_error(setup_dir(1))
  expect_message(setup_dir(all_options(main = "testing_casting")))
})

test_that("create_dir", {
  expect_error(create_dir(1))
})

test_that("create_main_dir", {
  expect_error(create_main_dir(1))
  expect_message(create_main_dir(dir_options(main = "ok")))
})

test_that("create_sub_dirs", {
  expect_error(create_sub_dirs(1))
})

test_that("create_sub_dir", {
  expect_error(create_sub_dir(1))
  expect_equal(create_sub_dir(), NULL)
  expect_message(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1]))
  expect_silent(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1]))
})

test_that("fill_dir", {
  expect_error(fill_dir(1))
})

test_that("cleanup_dir", {
  expect_error(cleanup_dir(1))
  opts1 <- all_options(main = "testing_casting", to_cleanup = NULL)
  opts3 <- all_options(main = "testing_casting")
  expect_message(cleanup_dir(opts1))
  expect_message(cleanup_dir(opts3))
})

test_that("fill_PortalData", {
  expect_error(fill_PortalData(1))
  pd_opts <- PortalData_options(main = "testing_casting", quiet = TRUE)
  expect_silent(fill_PortalData(pd_opts))
})

test_that("fill_data", {
  expect_error(fill_data(1))
})

test_that("fill_predictions", {
  expect_error(fill_predictions(1))
  if(test_location == "local"){
    opts <- all_options(main = "testing_casting", 
                        download_existing_predictions = TRUE)
    fill_predictions(opts$options_predictions)
  }
})

test_that("fill_models", {
  expect_error(fill_models(1))
})

test_that("verify_PortalData", {
  expect_error(verify_PortalData(1))
  expect_error(verify_PortalData(dirtree(main = "testing_casting"), 1))
  cleanup_dir(all_options(main = "testing_casting"))
  expect_message(verify_PortalData(dirtree(main = "testing_casting")))
})


unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)