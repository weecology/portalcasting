context("Test prefab model functions")

# forecast

test_that("AutoArima", {
  skip_on_cran() # downloads and casting take too long to run on cran
  fill_data(main = "./testing")
  keepers <- c("newmoonnumber", "BA", "DM", "DO")
  all <- read_rodents_table(main = "./testing")
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  all2$BA <- 0
  all2$DO <- c(rep(0, nrow(all2) - 1), 1)
  write.csv(all2, file_paths(main = "./testing", "data/rodents_all.csv"), 
             row.names = FALSE)

  keepers <- c("newmoonnumber", "DM")
  controls <- read_rodents_table(main = "./testing", "controls")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_paths(main = "./testing", 
                                 "data/rodents_controls.csv"), 
            row.names = FALSE)
  expect_message(f_a <- AutoArima(main = "./testing", 
                                  tmnt_type = "All", quiet = FALSE))
  expect_message(f_c <- AutoArima(main = "./testing", 
                                  tmnt_type = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("cast", "aic"))
  expect_equal(names(f_c), c("cast", "aic"))
})

test_that("ESSS", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(f_a <- ESSS(main = "./testing", 
                                  tmnt_type = "All", quiet = FALSE))
  expect_message(f_c <- ESSS(main = "./testing",  
                                  tmnt_type = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("cast", "aic"))
  expect_equal(names(f_c), c("cast", "aic"))
})

test_that("nbGARCH", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(f_a <- nbGARCH(main = "./testing", 
                                  tmnt_type = "All", quiet = FALSE))
  expect_message(f_c <- nbGARCH(main = "./testing",  
                                  tmnt_type = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("cast", "aic"))
  expect_equal(names(f_c), c("cast", "aic"))
})

test_that("nbsGARCH", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(f_a <- nbsGARCH(main = "./testing", 
                                  tmnt_type = "All", quiet = FALSE))
  expect_message(f_c <- nbsGARCH(main = "./testing",  
                                  tmnt_type = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("cast", "aic"))
  expect_equal(names(f_c), c("cast", "aic"))
})

test_that("pevGARCH", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(f_a <- pevGARCH(main = "./testing", lag = 6,
                                  tmnt_type = "All", quiet = FALSE))
  expect_message(f_c <- pevGARCH(main = "./testing", lag = 6, 
                                  tmnt_type = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("cast", "aic"))
  expect_equal(names(f_c), c("cast", "aic"))
})

# hindcast (only needed for model functions that have a distinciton)

test_that("pevGARCH", {
  skip_on_cran() # downloads and casting take too long to run on cran
  fill_data(main = "./testing", end_moon = 520)
  keepers <- c("newmoonnumber", "DM", "DO")
  all <- read_rodents_table(main = "./testing")
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  all2$BA <- 0
  all2$DO <- c(rep(0, nrow(all2) - 1), 1)
  write.csv(all2, file_paths(main = "./testing", "data/rodents_all.csv"), 
             row.names = FALSE)

  keepers <- c("newmoonnumber", "DM")
  controls <- read_rodents_table(main = "./testing", "controls")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_paths(main = "./testing", 
                                 "data/rodents_controls.csv"), 
            row.names = FALSE)


  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(f_a <- pevGARCH(main = "./testing", lag = 6,
                                  tmnt_type = "All", quiet = FALSE))
  expect_message(f_c <- pevGARCH(main = "./testing", lag = 6, 
                                  tmnt_type = "Controls", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_equal(names(f_a), c("cast", "aic"))
  expect_equal(names(f_c), c("cast", "aic"))
})

