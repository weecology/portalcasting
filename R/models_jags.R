
#' @title Create a Control List for a runjags JAGS Model Run
#'
#' @description We leverage the \code{\link[runjags]{run.jags}} function in the runjags (Denwood 2016) package to run JAGS (Plummer 2003) models in portalcasting. That function has a number of control parameters that users may be interested in changing, and this function wraps those parameters with a few portalcasting-specific parameters into a control list for input into specific model functions.
#'
#' @param nchains Non-negative \code{integer}-conformable value of the number of parallel chains to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param adapt Non-negative \code{integer}-conformable value of the number of adaptation steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param burnin Non-negative \code{integer}-conformable value of the number of burnin steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param sample Non-negative \code{integer}-conformable value of the number of sampling steps to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param thin Positive \code{integer}-conformable value of the thinning interval to use. See \code{\link[runjags]{run.jags}}.
#'
#' @param modules \code{character} vector of external modules to add to JAGS. See \code{\link[runjags]{run.jags}}.
#'
#' @param method \code{character} value of the \code{\link[runjags]{run.jags}} method to use. Options include \code{"rjags"}, \code{"simple"}, \code{"interruptible"}, \code{"parallel"}, \code{"rjparallel"}, \code{"background"}, \code{"bgparallel"}, and \code{"snow"}. See \code{\link[runjags]{run.jags}}.
#'
#' @param factories \code{character} vector of factory modules to add to JAGS. See \code{\link[runjags]{run.jags}}.
#'
#' @param mutate A \code{function} or \code{list} (with the first element being a \code{function}) used to add variables to the posterior chain (rather than throughout sampling). See \code{\link[runjags]{run.jags}}. 
#'
#' @param silent_jags \code{logical} value for quieting the output from the runjags function, including the underlying JAGS output. 
#'
#' @return \code{list} of controls. 
#'
#' @references 
#'  Denwood, M. J. 2016. runjags: an R package providing interface utilities, model templates, parallel computing methods and additional distributions for MCMC models in JAGS. Journal of Statistical Software, 71:9. \href{https://www.jstatsoft.org/article/view/v071i09}{URL}. 
#'  
#'  Plummer, M. 2003. JAGS: A program for analysis of Bayesian graphical models using Gibbs Sampling. Proceedings of the 3rd International Workshop on Distributed Statistical Computing (DSC 2003). ISSN 1609-395X. \href{https://bit.ly/33aQ37Y}{URL}.
#'
#' @examples
#'  runjags_control()
#'
#' @export
#'
runjags_control <- function (nchains     = 4, 
                             adapt       = 1e4, 
                             burnin      = 1e4, 
                             sample      = 1e4, 
                             thin        = 10, 
                             modules     = "glm", 
                             method      = "interruptible", 
                             factories   = "", 
                             mutate      = NA, 
                             silent_jags = FALSE){

  list(nchains     = nchains, 
       adapt       = adapt, 
       burnin      = burnin, 
       sample      = sample,
       thin        = thin, 
       modules     = modules, 
       method      = method, 
       factories   = factories,
       mutate      = mutate, 
       silent_jags = silent_jags)

}
