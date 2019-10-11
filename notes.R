#working on a few things:
#messaging 
#  soften specified path not found errors
#  pull errors through portalcast from the run_status



xx <- run.jags(model = jags_model, monitor = monitor, 
                          inits = inits, data = data, 
                          n.chains = 3,#control_runjags$nchains, 
                          adapt = 1e5,#control_runjags$adapt, 
                          burnin = 1e5,#control_runjags$burnin, 
                          sample = 1e5,#control_runjags$sample, 
                          thin = control_runjags$thin, 
                          modules = control_runjags$modules, 
                          method = "parallel",#control_runjags$method, 
                          factories = control_runjags$factories, 
                          mutate = control_runjags$mutate, 
                          summarise = FALSE, plots = FALSE)
summary(xx)
save(xx, file="xx.RData")

for when jags_RW is ready

#'  \code{jags_RW} fits a log-scale density random walk with a Poisson 
#'  observation process using JAGS (Just Another Gibbs Sampler; 
#'  Plummer 2003) hierarchical Bayesian inference. 

#'
#' @param control_runjags \code{list} of arguments passed to 
#'  \code{\link[runjags]{run.jags}} via \code{\link{runjags_control}}
#'   jags_RW()

#' @rdname prefab_model_functions
#'
#' @export
#'

#'  \code{jags_RW} fits a hierarchical log-scale density random walk model 
#'  with a Poisson observation process using the JAGS (Just Another Gibbs 
#'  Sampler; Plummer 2003) infrastructure. There are two process parameters:
#'  \code{mu}, the density of the species at the beginning of the time series,
#'  and \code{tau}, the precision (inverse variance) of the random walk, which
#'  is Gaussian on the log scale. The observation model has no additional
#'  parameters. The prior distributions for \code{mu} and \code{tau} are
#'  informed by the available data collected prior to the start of the data
#'  used in the time series. \code{mu} is normally distributed with a mean
#'  equal to the average log-scale density 
#'  Due to the presence of 0s in the data and the modeling on the log scale,
#'  an offset of \code{count + 0.1} is used prior to taking the log and then
#'  the offset is removed after the reconversion (exponentiation)
#'  as \code{density - 0.1}. 
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
