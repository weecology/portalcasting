context(desc = "moons prepping functions")

main <- "./testing"

test_that(desc = "prep_moons pulls in moons or throws error", {

  # downloads take too long for cran checks

    skip_on_cran() 

  moons <- prepare_newmoons(main = main)
  expect_is(moons, "data.frame")

})

test_that(desc = "add_future_moons skips for 0 or adds specifically extra more", {

  # downloads take too long for cran checks

    skip_on_cran() 

  moons <- prepare_newmoons(main = main)
  moons$newmoondate <- moons$moondate

  moons2 <- add_future_newmoons(main = main, moons, lead = 0)
  expect_is(moons2, "data.frame")
  expect_equal(moons, moons2)
 
  raw_path <- file.path(main, "resources")
  traps_in <- load_trapping_data(path = raw_path, download_if_missing = FALSE,
                                 clean = FALSE, quiet = TRUE)
  moons_in <- traps_in[["newmoons_table"]]
#  moons3 <- add_future_newmoons(main = main, moons = moons_in, 
#                             lead = 15, origin = Sys.Date())
#  moons3 <- format_moons(moons3)
#  expect_is(moons3, "data.frame")
#  expect_equal(NROW(moons3) > NROW(moons2), TRUE)

})


test_that(desc = "add_moons_from_date adds moons on to weather", {

  # downloads take too long for cran checks

    skip_on_cran() 

  raw_path <- file.path(main, "resources")
  weather <- portalr::weather("daily", fill = TRUE, path = raw_path)
  moons <- prepare_newmoons(main = main)
  weather2 <- add_newmoonnumbers_from_dates(weather, moons)
  expect_is(weather2, "data.frame") 
  expect_equal(NCOL(weather) < NCOL(weather2), TRUE)

})

