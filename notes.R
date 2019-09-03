# to do
# update vignettes
# read through the code documentation again
#   donttest examples

connect read to filenames duh

move read_cov_casts to data_i_o
diff between read_cov_casts and read_covariate_casts?????
casts_metadata


get into the models: verbose, filenames control list

  moons <- ifnull(moons, read_moons(main = main, 
                                    control_files = control_files,
                                    arg_checks = arg_checks))


                            control_files = files_control(),
#'
#' @param control_files \code{list} of names of the folders and files within
#'  the sub directories and saving strategies (save, overwrite, append, etc.).
#'  Generally shouldn't need to be edited. See \code{\link{files_control}}.


devtools::document()
main <- "~/test"
setup_dir(main)


plot_cast_ts(main)



ESSS(main=main)
portalcast(main=main, models="ESSS")


portalcast(main, end_moons = 512:521)

devtools::test(filter="01")
devtools::test(filter="02")
devtools::test(filter="03")
devtools::test(filter="04")
devtools::test(filter="05")
devtools::test(filter="06")
devtools::test(filter="07")
devtools::test(filter="08")
devtools::test(filter="09")
devtools::test(filter="10")
devtools::test(filter="11")
devtools::test(filter="12")
devtools::test(filter="13")
devtools::test(filter="14")
devtools::test(filter="15")
devtools::test(filter="16")
devtools::test(filter="17")
devtools::test(filter="18")
devtools::test(filter="19")










