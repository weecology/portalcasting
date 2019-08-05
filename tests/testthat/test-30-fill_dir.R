context("Test filling functions")

unlink(main_path(main = "testing"), recursive = TRUE, force = TRUE)
create_dir(main = "testing")

test_that("download", {
  expect_message(download("PortalData", "zenodo", main = "testing",
                          concept_rec_id = "1215988"))
})


unlink(main_path(main = "testing"), recursive = TRUE, force = TRUE)
