context("Test prefab model functions")

# forecast

main = "./testing"

test_that("AutoArima", {
  skip_on_cran() # downloads and casting take too long to run on cran
  fill_data(main = main)
  keepers <- c("moon", "BA", "DM", "DO")
  all <- read_rodents_table(main = main)
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  all2$BA <- 0
  all2$DO <- c(rep(0, nrow(all2) - 1), 1)
  write.csv(all2, file_path(main = main, "data", "rodents_all.csv"), 
             row.names = FALSE)

  keepers <- c("moon", "DM")
  controls <- read_rodents_table(main = main, "controls")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_path(main = main, 
                                 "data", "rodents_controls.csv"), 
            row.names = FALSE)
  
  keepers <- c("moon", "DM")
  controls <- read_rodents_table(main = main, "exclosures")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls3 <- controls[, rest_cols]
  write.csv(controls3, file_path(main = main, 
                                 "data", "rodents_exclosures.csv"), 
            row.names = FALSE)
  
  expect_message(f_a <- AutoArima(main = main, 
                                  data_set = "All", quiet = FALSE))
  expect_message(f_c <- AutoArima(main = main, 
                                  data_set = "Controls", quiet = FALSE))
  expect_message(f_e <- AutoArima(main = main, 
                                  data_set = "Exclosures", quiet = FALSE)) 
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")
})

test_that("NaiveArima", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(f_c <- NaiveArima(main = "./testing",  
                                   data_set = "Controls", quiet = FALSE))
  expect_message(f_a <- NaiveArima(main = "./testing", 
                                   data_set = "All", quiet = FALSE))
  expect_message(f_e <- NaiveArima(main = "./testing", 
                                   data_set = "Exclosures", quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")
})



test_that("ESSS", {
  skip_on_cran() # downloads and casting take too long to run on cran

  keepers <- c("moon", "BA", "DM", "DO")
  all <- read_rodents_table(main = main, "all_interp")
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  all2$BA <- 0
  all2$DO <- c(rep(0, nrow(all2) - 1), 1)
  write.csv(all2, file_path(main = main, "data", "rodents_all_interp.csv"), 
             row.names = FALSE)

  keepers <- c("moon", "DM")
  controls <- read_rodents_table(main = main, "controls_interp")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_path(main = main, 
                                 "data", "rodents_controls_interp.csv"), 
            row.names = FALSE)

  keepers <- c("moon", "DM")
  controls <- read_rodents_table(main = main, "exclosures_interp")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls3 <- controls[, rest_cols]
  write.csv(controls3, file_path(main = main, 
                                 "data", "rodents_exclosures_interp.csv"), 
            row.names = FALSE)

  expect_message(f_c <- ESSS(main = "./testing",  
                             data_set = "Controls_interp", quiet = FALSE))
  expect_message(f_a <- ESSS(main = "./testing", 
                             data_set = "All_interp", quiet = FALSE))
  expect_message(f_e <- ESSS(main = "./testing", 
                             data_set = "Exclosures_interp", quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")
})



test_that("nbGARCH", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(f_c <- nbGARCH(main = "./testing",  
                                data_set = "Controls_interp", quiet = FALSE))
  expect_message(f_a <- nbGARCH(main = "./testing", 
                                data_set = "All_interp", quiet = FALSE))
  expect_message(f_e <- nbGARCH(main = "./testing", 
                                data_set = "Exclosures_interp", quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")
})

test_that("nbsGARCH", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(f_c <- nbsGARCH(main = "./testing",  
                                 data_set = "Controls_interp", quiet = FALSE))
  expect_message(f_a <- nbsGARCH(main = "./testing", 
                                 data_set = "All_interp", quiet = FALSE))
  expect_message(f_e <- nbsGARCH(main = "./testing", 
                                 data_set = "Exclosures_interp", quiet = FALSE))
  
  
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")
})

test_that("pevGARCH", {
  skip_on_cran() # downloads and casting take too long to run on cran
  expect_message(f_c <- pevGARCH(main = "./testing",  
                                 data_set = "Controls_interp", quiet = FALSE))
  expect_message(f_a <- pevGARCH(main = "./testing", 
                                 data_set = "All_interp", quiet = FALSE))
  expect_message(f_e <- pevGARCH(main = "./testing", 
                                 data_set = "Exclosures_interp", quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")
})

test_that("pevGARCH", {
  skip_on_cran() # downloads and casting take too long to run on cran
  fill_data(main = "./testing", end_moon = 520)

  keepers <- c("moon", "BA", "DM", "DO")
  all <- read_rodents_table(main = main, "all_interp")
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  all2$BA <- 0
  all2$DO <- c(rep(0, nrow(all2) - 1), 1)
  write.csv(all2, file_path(main = main, "data", "rodents_all_interp.csv"), 
             row.names = FALSE)

  keepers <- c("moon", "DM")
  controls <- read_rodents_table(main = main, "controls_interp")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_path(main = main, 
                                 "data", "rodents_controls_interp.csv"), 
            row.names = FALSE)
  
  keepers <- c("moon", "DM")
  controls <- read_rodents_table(main = main, "exclosures_interp")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls3 <- controls[, rest_cols]
  write.csv(controls3, file_path(main = main, 
                                 "data", "rodents_exclosures_interp.csv"), 
            row.names = FALSE)
  

  expect_message(f_a <- pevGARCH(main = "./testing", lag = 6,
                                  data_set = "all_interp", quiet = FALSE))
  expect_message(f_c <- pevGARCH(main = "./testing", lag = 6, 
                              data_set = "controls_interp", quiet = FALSE))
  expect_message(f_e <- pevGARCH(main = "./testing", lag = 6, 
                                 data_set = "exclosures_interp", quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")
})

test_that("simplexEDM", {
  skip_on_cran() # downloads and casting take too long to run on cran
  fill_data(main = "./testing", end_moon = 520)
  
  keepers <- c("moon", "BA", "DM", "DO")
  all <- read_rodents_table(main = main, "all_interp")
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  all2$BA <- 0
  all2$DO <- c(rep(0, nrow(all2) - 1), 1)
  write.csv(all2, file_path(main = main, "data", "rodents_all_interp.csv"), 
            row.names = FALSE)
  
  keepers <- c("moon", "DM")
  controls <- read_rodents_table(main = main, "controls_interp")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_path(main = main, 
                                 "data", "rodents_controls_interp.csv"), 
            row.names = FALSE)
  
  keepers <- c("moon", "DM")
  controls <- read_rodents_table(main = main, "controls_interp")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls3 <- controls[, rest_cols]
  write.csv(controls3, file_path(main = main, 
                                 "data", "rodents_exclosures_interp.csv"), 
            row.names = FALSE)
  
  expect_message(f_a <- simplexEDM(main = "./testing", max_E = 7,
                                 data_set = "all_interp", quiet = FALSE))
  expect_message(f_c <- simplexEDM(main = "./testing", max_E = 7,
                                 data_set = "controls_interp", quiet = FALSE))
  expect_message(f_e <- simplexEDM(main = "./testing", max_E = 7,
                                   data_set = "exclosures_interp", quiet = FALSE))
  
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")
})

test_that("GPEDM", {
  skip_on_cran() # downloads and casting take too long to run on cran
  fill_data(main = "./testing", end_moon = 520)
  
  keepers <- c("moon", "BA", "DM", "DO")
  all <- read_rodents_table(main = main, "all_interp")
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  all2$BA <- 0
  all2$DO <- c(rep(0, nrow(all2) - 1), 1)
  write.csv(all2, file_path(main = main, "data", "rodents_all_interp.csv"), 
            row.names = FALSE)
  
  keepers <- c("moon", "DM")
  controls <- read_rodents_table(main = main, "controls_interp")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_path(main = main, 
                                 "data", "rodents_controls_interp.csv"), 
            row.names = FALSE)
  
  expect_message(f_a <- GPEDM(main = "./testing", max_E = 3,
                                   data_set = "all_interp", quiet = FALSE))
  expect_message(f_c <- GPEDM(main = "./testing", max_E = 3,
                                   data_set = "controls_interp", quiet = FALSE))
  
  expect_is(f_a, "list")
  expect_is(f_c, "list")
})

check_jags <- function()
{
  result <- capture_condition(runjags::findjags())
  if (any(c("warning", "error") %in% attributes(result)$class))
  {
    skip(result$message)
  }
}

test_that("jags_RW", {
  skip_on_cran() # downloads and casting take too long to run on cran
  check_jags() # check for JAGS installation on system
  fill_data(main = "./testing", end_moon = 520)

  keepers <- c("moon", "BA", "ntraps")
  all <- read_rodents_table(main = main, "all")
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  write.csv(all2, file_path(main = main, "data", "rodents_all.csv"), 
             row.names = FALSE)

  keepers <- c("moon", "DM", "ntraps")
  controls <- read_rodents_table(main = main, "controls")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_path(main = main, 
                                 "data", "rodents_controls.csv"), 
            row.names = FALSE)
  
  keepers <- c("moon", "DM", "ntraps")
  controls <- read_rodents_table(main = main, "exclosures")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls3 <- controls[, rest_cols]
  write.csv(controls3, file_path(main = main, 
                                 "data", "rodents_exclosures.csv"), 
            row.names = FALSE)

  expect_message(f_a <- jags_RW(main = "./testing", 
                                control_runjags = runjags_control(adapt = 1e3,
                                       burnin = 1e3, sample = 1e3),
                                data_set = "all", quiet = FALSE))
  expect_message(f_c <- jags_RW(main = "./testing",  
                                control_runjags = runjags_control(adapt = 1e3,
                                       burnin = 1e3, sample = 1e3),
                                data_set = "controls", quiet = FALSE))
  expect_message(f_e <- jags_RW(main = "./testing",  
                                control_runjags = runjags_control(adapt = 1e3,
                                                                  burnin = 1e3, 
                                                                  sample = 1e3),
                                data_set = "exclosures", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")
})
