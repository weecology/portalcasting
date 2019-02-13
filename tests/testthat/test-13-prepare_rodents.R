context("Test prepare_rodents functions")

rod_opts <- all_options(main = "testing_casting")$options_data$rodents
rod_opts2 <- rod_opts
rod_opts2$quiet <- TRUE
moon_opts <- all_options(main = "testing_casting")$options_data$moons
moons <- prep_moons(moon_opts)

test_that("prep_rodents", {
  expect_error(prep_rodents(1, rod_opts), "`moons` is not")
  expect_error(prep_rodents(moons, 1), "`options_rodents` is not")
  expect_message(rodents <- prep_rodents(moons, rod_opts))
  expect_silent(rodents2 <- prep_rodents(moons, rod_opts2))
  expect_is(rodents, "rodents_list")
  expect_is(rodents2, "rodents_list")
  expect_equal(rodents, rodents2)
})

test_that("rodents_data", {
  expect_error(rodents_data(1, rod_opts), "`moons` is not")
  expect_error(rodents_data(moons, 1), "`options_rodents` is not")
  expect_message(rodents <- rodents_data(moons, rod_opts))
  expect_silent(rodents2 <- rodents_data(moons, rod_opts2))
  expect_is(rodents, "rodents")
  expect_is(rodents2, "rodents")
  expect_equal(rodents, rodents2)
})

test_that("enforce_rodents_options", {
  expect_error(enforce_rodents_options(1, "all"), "`options_rodents` is not")
  expect_error(enforce_rodents_options(rod_opts, 1), "`tmnt_type` is not")
  expect_error(enforce_rodents_options(rod_opts, "ok"), "`tmnt_type` must")
  expect_silent(rod_opts1a <- enforce_rodents_options(rod_opts, "all"))
  expect_silent(rod_opts1b <- enforce_rodents_options(rod_opts, "controls"))
  expect_silent(rod_opts1c <- enforce_rodents_options(rod_opts, NULL))
  expect_is(rod_opts1a, "rodents_options")  
  expect_is(rod_opts1b, "rodents_options")  
  expect_is(rod_opts1c, "rodents_options")  
  expect_equal(rod_opts, rod_opts1c)
})

test_that("remove_spp", {
  rodents <- prep_rodents(moons, rod_opts)$all
  expect_error(remove_spp(1, "BA"), "`data` is not")
  expect_error(remove_spp(rodents, 1), "`drop_spp` is not")
  expect_silent(rodents1 <- remove_spp(rodents, NULL))
  expect_silent(rodents2 <- remove_spp(rodents, "BA"))
  expect_warning(rodents3 <- remove_spp(rodents, "ok"))
  expect_equal(rodents, rodents1)
  expect_is(rodents1, "rodents")
  expect_is(rodents2, "rodents")
  expect_equal(ncol(rodents2) < ncol(rodents1), TRUE)
  expect_is(rodents3, "rodents")  
  expect_equal(rodents, rodents3)
})

test_that("rodent_spp", {
  expect_silent(spp <- rodent_spp())
  expect_is(spp, "character")
  expect_equal(length(spp), 21)
  expect_error(rodent_spp("1"))
  expect_silent(spp <- rodent_spp(TRUE))
  expect_is(spp, "character")
  expect_equal(length(spp), 21)
})

test_that("is.spcol", {
  rodents <- prep_rodents(moons, rod_opts)$all
  expect_error(is.spcol(1, rodent_spp()), "`x` is not")
  expect_error(is.spcol(rodents, 1), "`spp_names` is not")
  expect_silent(sppTF <- is.spcol(rodents, rodent_spp()))
  expect_is(sppTF, "logical")
  expect_equal(sum(sppTF), 20)
})

test_that("trim_treatment", {
  rod_opts_a <- enforce_rodents_options(rod_opts, "all")
  rod_opts_c <- enforce_rodents_options(rod_opts, "controls")

  options_rodents <- rod_opts_a
  roda <-  summarize_rodent_data(path = main_path(options_rodents$tree), 
                        clean = FALSE, type = "Rodents", 
                            level = options_rodents$level, 
                            plots = options_rodents$plots, 
                            min_plots = options_rodents$min_plots, 
                            output = options_rodents$output) %>%
            classy(c("data.frame", "rodents")) %>% 
            remove_spp(options_rodents$drop_spp) %>%
            mutate(total = rowSums(.[ , is.spcol(.)])) %>%
            classy(c("data.frame", "rodents"))
  options_rodents <- rod_opts_c
  rodc <-  summarize_rodent_data(path = main_path(options_rodents$tree), 
                        clean = FALSE, type = "Rodents", 
                            level = options_rodents$level, 
                            plots = options_rodents$plots, 
                            min_plots = options_rodents$min_plots, 
                            output = options_rodents$output) %>%
            classy(c("data.frame", "rodents")) %>% 
            remove_spp(options_rodents$drop_spp) %>%
            mutate(total = rowSums(.[ , is.spcol(.)])) %>%
            classy(c("data.frame", "rodents"))

  expect_error(trim_treatment(1, rod_opts), "`data` is not")
  expect_error(trim_treatment(roda, 1), "`options_rodents` is not")
  expect_silent(rodat <-  trim_treatment(roda, rod_opts_a))
  expect_silent(rodct <-  trim_treatment(rodc, rod_opts_c))
  expect_equal(roda, rodat)
  expect_equal(ncol(rodc) > ncol(rodct), TRUE)
  expect_is(rodat, "rodents")
  expect_is(rodct, "rodents")
})

test_that("transfer_trapping_table", {
  d_opts <- all_options(main = "testing_casting")$options_data
  expect_error(transfer_trapping_table(1), "`options_data` is not")
  expect_message(transfer_trapping_table(d_opts))
})