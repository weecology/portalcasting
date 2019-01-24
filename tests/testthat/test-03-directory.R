context("Test directory functions")

test_location <- "travis"

test_that("setup_dir", {
  expect_error(setup_dir(1))
  expect_silent(setup_dir(all_options(main = "ok", quiet = TRUE)))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
  expect_message(setup_dir(all_options(main = "ok")))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
})

test_that("create_dir", {
  expect_error(create_dir(1))
})

test_that("create_main_dir", {
  expect_error(create_main_dir(1))
  expect_message(create_main_dir(dir_options(main = "ok")))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
})

test_that("create_sub_dirs", {
  expect_error(create_sub_dirs(1))
})

test_that("create_sub_dir", {
  expect_error(create_sub_dir(1))
  expect_equal(create_sub_dir(), NULL)
  create_main_dir(dir_options(main = "ok", quiet = TRUE))
  expect_message(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1]))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
  create_main_dir(dir_options(main = "ok", quiet = TRUE))
  expect_silent(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1],
                               quiet = TRUE))
  expect_silent(create_sub_dir(sub_paths(tree = dirtree(main = "ok"))[1]))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
})


test_that("fill_dir", {
  expect_error(fill_dir(1))
})

test_that("fill_PortalData", {
  expect_error(fill_PortalData(1))
})

test_that("fill_data", {
  expect_error(fill_data(1))
})

test_that("fill_predictions", {
  expect_error(fill_predictions(1))
  if(test_location == "local"){
    opts <- all_options(main = "ok", download_existing_predictions = TRUE)
    create_dir(opts$options_dir)
    fill_predictions(opts$options_predictions)
    unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
  }
})

test_that("fill_models", {
  expect_error(fill_models(1))
})

test_that("verify_PortalData", {
  expect_error(verify_PortalData(1))
  expect_error(verify_PortalData(filename = 1))
  expect_message(verify_PortalData(dirtree(main = "ok")))
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
  unlink(dirtree(main = "ok"), recursive = TRUE, force = TRUE)
})

test_that("cleanup_dir", {
  expect_error(cleanup_dir(1))
})
