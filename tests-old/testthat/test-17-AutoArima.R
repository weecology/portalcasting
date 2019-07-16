context("Test AutoArima functions")

tree <- dirtree(main = "testing_casting");

# this reduces the abundances to a single species for all model testing
#  for the sake of speed
# we also dummy up two species, one with all 0s and one with all but 1 0
#  to facilitate edge case testing (only need to do that in one level tho)

test_location <- "travis"

if(test_location != "local"){
  keepers <- c("newmoonnumber", "BA", "DM", "DO")
  all <- read_all(tree)
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  all2$BA <- 0
  all2$DO <- c(rep(0, nrow(all2) - 1), 1)
  write.csv(all2, file_paths(tree, "data/all.csv"), row.names = FALSE)

  keepers <- c("newmoonnumber", "DM")
  controls <- read_controls(tree)
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_paths(tree, "data/controls.csv"), 
            row.names = FALSE)
}

test_that("AutoArima", {
  expect_message(f_a <- AutoArima(tree, level = "All", quiet = FALSE))
  expect_message(f_c <- AutoArima(tree, level = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("forecast", "aic"))
  expect_equal(names(f_c), c("forecast", "aic"))
  expect_error(AutoArima(1, level = "Controls", quiet = FALSE))
  expect_error(AutoArima(tree, level = 1, quiet = FALSE))
  expect_error(AutoArima(tree, level = c("All", "Controls"), quiet = FALSE))
  expect_error(AutoArima(tree, level = "ok", quiet = FALSE))
  expect_error(AutoArima(tree, level = "Controls", quiet = 1))
})

