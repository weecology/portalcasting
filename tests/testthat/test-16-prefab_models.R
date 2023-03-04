context(desc = "prefab model functions")

main <- "./testing"

  # set up a mini version of data

  fill_data(main = main)  


test_that(desc = "AutoArima model", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(f_a <- AutoArima(main = main, 
                                  dataset = "All", quiet = FALSE))
  expect_message(f_c <- AutoArima(main = main, 
                                  dataset = "Controls", quiet = FALSE))
  expect_message(f_e <- AutoArima(main = main, 
                                  dataset = "Exclosures", quiet = FALSE)) 
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})

test_that(desc = "NaiveArima model", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(f_c <- NaiveArima(main = main,  
                                   dataset = "Controls", quiet = FALSE))
  expect_message(f_a <- NaiveArima(main = main, 
                                   dataset = "All", quiet = FALSE))
  expect_message(f_e <- NaiveArima(main = main, 
                                   dataset = "Exclosures", quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})



test_that(desc = "ESSS model", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(f_c <- ESSS(main = main,  
                             dataset = "Controls", quiet = FALSE))
  expect_message(f_a <- ESSS(main = main, 
                             dataset = "All", quiet = FALSE))
  expect_message(f_e <- ESSS(main = main, 
                             dataset = "Exclosures", quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})



test_that(desc = "nbGARCH model", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(f_c <- nbGARCH(main = main,  
                                dataset = "Controls", quiet = FALSE))
  expect_message(f_a <- nbGARCH(main = main, 
                                dataset = "All", quiet = FALSE))
  expect_message(f_e <- nbGARCH(main = main, 
                                dataset = "Exclosures", 
                                quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})

test_that(desc = "nbsGARCH model", {

  # download is held back on cran

    skip_on_cran() 

  expect_message(f_c <- nbsGARCH(main = main,  
                                 dataset = "Controls", quiet = FALSE))
  expect_message(f_a <- nbsGARCH(main = main, 
                                 dataset = "All", quiet = FALSE))
  expect_message(f_e <- nbsGARCH(main = main, 
                                 dataset = "Exclosures", 
                                 quiet = FALSE))
  
  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})


test_that(desc = "pevGARCH model", {

  # download is held back on cran

    skip_on_cran() 


  expect_message(f_a <- pevGARCH(main = main, lag = 6, 
                                  dataset = "All", quiet = FALSE))
  expect_message(f_c <- pevGARCH(main = main, lag = 6, 
                              dataset = "Controls", quiet = FALSE))
  expect_message(f_e <- pevGARCH(main = main, lag = 6, 
                                 dataset = "Exclosures",  
                                 quiet = FALSE))

  expect_is(f_a, "list")
  expect_is(f_c, "list")
  expect_is(f_e, "list")

})

check_jags <- function(){
  result <- capture_condition(runjags::findjags())
  if (any(c("warning", "error") %in% attributes(result)$class)){
    skip(result$message)
  }
}

test_that(desc = "jags_RW model", {

  # download is held back on cran

    skip_on_cran() 

  # check for JAGS installation on system

    check_jags() 

  rjc <- runjags_control(adapt = 100, burnin = 100, sample = 100, thin = 1)
  expect_message(f_a <- jags_RW(main = main, control_runjags = rjc,
                                dataset = "Controls", quiet = FALSE))
  expect_is(f_a, "list")

})

test_that(desc = "jags_logistic model", {

  # download is held back on cran

    skip_on_cran() 

  # check for JAGS installation on system

    check_jags() 

  rjc <- runjags_control(adapt = 100, burnin = 100, sample = 100, thin = 1)
  expect_message(f_a <- jags_logistic(main = main, control_runjags = rjc,
                                dataset = "Controls", quiet = FALSE))
  expect_is(f_a, "list")


})

test_that(desc = "jags_logistic_covariates model", {

  # download is held back on cran

    skip_on_cran() 

  # check for JAGS installation on system

    check_jags() 

  rjc <- runjags_control(adapt = 100, burnin = 100, sample = 100, thin = 1)
  expect_message(f_a <- jags_logistic_covariates(main = main, control_runjags = rjc,
                                dataset = "Controls", quiet = FALSE))
  expect_is(f_a, "list")


})


test_that(desc = "jags_logistic_competition model", {

  # download is held back on cran

    skip_on_cran() 

  # check for JAGS installation on system

    check_jags() 

  rjc <- runjags_control(adapt = 100, burnin = 100, sample = 100, thin = 1)
  expect_message(f_a <- jags_logistic_competition(main = main, control_runjags = rjc,
                                dataset = "Controls", quiet = FALSE))
  expect_is(f_a, "list")


})


test_that(desc = "jags_logistic_competition_covariates model", {

  # download is held back on cran

    skip_on_cran() 

  # check for JAGS installation on system

    check_jags() 

  rjc <- runjags_control(adapt = 100, burnin = 100, sample = 100, thin = 1)
  expect_message(f_a <- jags_logistic_competition_covariates(main = main, control_runjags = rjc,
                                dataset = "Controls", quiet = FALSE))
  expect_is(f_a, "list")


})
