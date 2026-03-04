library(testthat)
library(portalcasting)
library(shiny)

cache_dir <- Sys.getenv("PORTALCASTING_TEST_CACHE", file.path(tempdir(), "portalcasting_test_cache"))
assign("main1", file.path(tempdir(), "testing1"), envir = .GlobalEnv)
assign("main2", cache_dir, envir = .GlobalEnv)
assign("main3", cache_dir, envir = .GlobalEnv)

test_check("portalcasting")