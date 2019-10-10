#working on a few things:
#the general jags build
# USE THE DATA TO INFORM THE STATES
#messaging 
#  soften specified path not found errors
#  pull errors through portalcast from the run_status


for when jags_RW is ready

#'  \code{jags_RW} fits a simple random walk using the JAGS (Just Another 
#'  Gibbs Sampler; Plummer 2003) infrastructure. 

#'
#' @param control_runjags \code{list} of arguments passed to 
#'  \code{\link[runjags]{run.jags}} via \code{\link{runjags_control}}
#'   jags_RW()

#' @rdname prefab_model_functions
#'
#' @export
#'


devtools::document()
devtools::load_all()




main <- "c:/users/dappe/dropbox/uf/jags_build"
#main <- "c:/users/uf/dropbox/uf/jags_build"
#main <- "~/logistic_build"
#setup_dir(main)



portalcast(main = main, models = c("ESSS"))






portalcast(main = main,models = c("nbsGARCH"), 
           end_moons = 512)
plot_cast_ts(main=main)


plot_cast_point(main=main)
plot_cast_point(main=main,with_census=T)
plot_casts_err_lead(main)
plot_casts_cov_RMSE(main)

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
devtools::test(filter="20")
