% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_newmoons.R
\name{prepare newmoons}
\alias{prepare newmoons}
\alias{prep-newmoons}
\alias{newmoons}
\alias{prepare_newmoons}
\alias{add_forecast_newmoons}
\title{Prepare Lunar Data for the Portalcasting Repository}
\usage{
prepare_newmoons(main = ".")

add_forecast_newmoons(main = ".", newmoons = NULL)
}
\arguments{
\item{main}{\code{character} value of the name of the main component of the directory tree.}

\item{newmoons}{\code{data.frame} of newmoon data.}
}
\value{
Some version of a newmoons \code{data.frame}. \cr
\code{prepare_newmoons}: fully appended and formatted \code{data.frame} (also saved out if \code{settings$save = TRUE}). \cr
\code{add_forecast_newmoons}: fully appended and formatted \code{data.frame}.
}
\description{
Get time information (calendar dates, census periods, and newmoon numbers) associated with trapping events (achieved and missed) based on a lunar survey schedule. \cr
\code{add_forecast_newmoons} adds future newmoon dates to the newmoon table from \code{prepare_newmoons} associated with the forecast.
}
\examples{
\dontrun{
   main1 <- file.path(tempdir(), "newmoons")

   create_dir(main = main1)
   fill_resources(main = main1)
   fill_forecasts(main = main1)
   fill_fits(main = main1)
   fill_models(main = main1)

   prepare_newmoons(main = main1)

   unlink(main1, recursive = TRUE)
}

}
\seealso{
Content preparation functions:
\code{\link{directory filling}},
\code{\link{prepare covariates}},
\code{\link{prepare metadata}},
\code{\link{prepare models}},
\code{\link{prepare rodents}}
}
\concept{content-prep}
