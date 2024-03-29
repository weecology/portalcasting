% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{foy}
\alias{foy}
\title{Calculate the Fraction of the Year from a Date}
\usage{
foy(dates = NULL)
}
\arguments{
\item{dates}{\code{Date}(s) or \code{Date}-conformable value(s) to be converted to the fraction of the year.}
}
\value{
\code{numeric} value(s) of the fraction of the year.
}
\description{
Based on the year in which the date occurred, determine the fraction of the year (foy) for the date (in relation to New Year's Eve in that year).
}
\examples{
   Sys.Date( )
   foy(Sys.Date())

}
\seealso{
Other utilities: 
\code{\link{file_ext}()},
\code{\link{ifnull}()},
\code{\link{messages}},
\code{\link{named_null_list}()},
\code{\link{package_version_finder}()},
\code{\link{round_na.interp}()},
\code{\link{update_list}()}
}
\concept{utilities}
