context("prefab model functions")

main <- "./testing"

  # set up a mini version of data

# i'm not sure which model (maybe pevgarch?) needs this. check in covr
#   all2$BA <- 0
#   all2$DO <- c(rep(0, nrow(all2) - 1), 1)
# also not sure if that's how it should be now


  fill_data(main = main, end_moon = 520)
  keepers <- c("moon", "DM", "ntraps")


  all <- read_rodents_table(main = main, "all")
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  write.csv(all2, file_path(main = main, "data", "rodents_all.csv"), 
             row.names = FALSE)

  controls <- read_rodents_table(main = main, "controls")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_path(main = main, 
                                 "data", "rodents_controls.csv"), 
            row.names = FALSE)
  
  controls <- read_rodents_table(main = main, "exclosures")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls3 <- controls[, rest_cols]
  write.csv(controls3, file_path(main = main, 
                                 "data", "rodents_exclosures.csv"), 
            row.names = FALSE)

  all <- read_rodents_table(main = main, "all_interp")
  rest_cols <- which(colnames(all) %in% keepers)
  all2 <- all[, rest_cols]
  write.csv(all2, file_path(main = main, "data", "rodents_all_interp.csv"), 
             row.names = FALSE)

  all2$BA <- 0
  all2$DO <- c(rep(0, nrow(all2) - 1), 1)
  write.csv(all2, file_path(main = main, "data", "rodents_all_interp2.csv"), 
             row.names = FALSE)

  controls <- read_rodents_table(main = main, "controls_interp")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls2 <- controls[, rest_cols]
  write.csv(controls2, file_path(main = main, 
                                 "data", "rodents_controls_interp.csv"), 
            row.names = FALSE)
  
  controls <- read_rodents_table(main = main, "exclosures_interp")
  rest_cols <- which(colnames(controls) %in% keepers)
  controls3 <- controls[, rest_cols]
  write.csv(controls3, file_path(main = main, 
                                 "data", "rodents_exclosures_interp.csv"), 
            row.names = FALSE)
  


test_that("AutoArima model", {

  # download is held back on cran

    skip_on_cran() 

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

test_that("NaiveArima model", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(f_c <- NaiveArima(main = main,  
                                   data_set = "Controls", quiet = FALSE))
  expect_message(f_a <- NaiveArima(main = main, 
                                   data_set = "All", quiet = FALSE))
  expect_message(f_e <- NaiveArima(main = main, 
                                   data_set = "Exclosures", quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})



test_that("ESSS model", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(f_c <- ESSS(main = main,  
                             data_set = "Controls_interp", quiet = FALSE))
  expect_message(f_a <- ESSS(main = main, 
                             data_set = "All_interp", quiet = FALSE))
  expect_message(f_e <- ESSS(main = main, 
                             data_set = "Exclosures_interp", quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})



test_that("nbGARCH model", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(f_c <- nbGARCH(main = main,  
                                data_set = "Controls_interp", quiet = FALSE))
  expect_message(f_a <- nbGARCH(main = main, 
                                data_set = "All_interp", quiet = FALSE))
  expect_message(f_e <- nbGARCH(main = main, 
                                data_set = "Exclosures_interp", 
                                quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})

test_that("nbsGARCH model", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(f_c <- nbsGARCH(main = main,  
                                 data_set = "Controls_interp", quiet = FALSE))
  expect_message(f_a <- nbsGARCH(main = main, 
                                 data_set = "All_interp", quiet = FALSE))
  expect_message(f_e <- nbsGARCH(main = main, 
                                 data_set = "Exclosures_interp", 
                                 quiet = FALSE))
  
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})


test_that("pevGARCH model", {

  # download is held back on cran

    skip_on_cran() 


  expect_message(f_a <- pevGARCH(main = main, lag = 6, 
                                  data_set = "All_interp", quiet = FALSE))
  expect_message(f_c <- pevGARCH(main = main, lag = 6, 
                              data_set = "Controls_interp", quiet = FALSE))
  expect_message(f_e <- pevGARCH(main = main, lag = 6, 
                                 data_set = "Exclosures_interp",  
                                 quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})

#test_that("simplexEDM model", {

  # download is held back on cran

    skip_on_cran() 

 
 # expect_message(f_a <- simplexEDM(main = main, max_E = 7,
  #                               data_set = "all_interp", quiet = FALSE))
#  expect_message(f_c <- simplexEDM(main = main, max_E = 7,
 #                                data_set = "controls_interp", quiet = FALSE))
  #expect_message(f_e <- simplexEDM(main = main, max_E = 7,
   #                                data_set = "exclosures_interp",  
    #                               quiet = FALSE))
  
#  expect_is(f_a, "list")
 # expect_is(f_c, "list")
  #expect_is(f_e, "list")

#})

#test_that("GPEDM model", {

  # download is held back on cran

 #   skip_on_cran() 


  #expect_message(f_a <- GPEDM(main = main, max_E = 3,
  #                            data_set = "all_interp", quiet = FALSE))
  #expect_message(f_c <- GPEDM(main = main, max_E = 3,
   #                           data_set = "controls_interp", quiet = FALSE))
  #expect_message(f_e <- GPEDM(main = main, max_E = 3,
   #                           data_set = "exclosures_interp", quiet = FALSE))
  
  #expect_is(f_a, "list")
  #expect_is(f_c, "list")
  #expect_is(f_e, "list")

#})

check_jags <- function(){
  result <- capture_condition(runjags::findjags())
  if (any(c("warning", "error") %in% attributes(result)$class)){
    skip(result$message)
  }
}

test_that("jags_RW model", {

  # download is held back on cran

    skip_on_cran() 

  # check for JAGS installation on system

    check_jags() 

  rjc <- runjags_control(adapt = 100, burnin = 100, sample = 100)
  expect_message(f_a <- jags_RW(main = main, control_runjags = rjc,
                                data_set = "all", quiet = FALSE))
  expect_message(f_c <- jags_RW(main = main, control_runjags = rjc,
                                data_set = "controls", quiet = FALSE))
  expect_message(f_e <- jags_RW(main = main, control_runjags = rjc,
                                data_set = "exclosures", quiet = FALSE))
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})
