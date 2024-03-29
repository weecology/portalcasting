% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_dataset.R
\name{new datasets}
\alias{new datasets}
\alias{dataset_controls_template}
\alias{add_new_dataset}
\alias{new_dataset_controls}
\alias{new_dataset_metadata}
\alias{new_dataset_fun}
\alias{new_dataset_args}
\title{Facilitate Adding Datasets to a Directory}
\usage{
dataset_controls_template()

add_new_dataset(
  main = ".",
  new_dataset_controls = dataset_controls_template(),
  models = NULL
)

new_dataset_controls(...)

new_dataset_metadata(...)

new_dataset_fun(fun = dataset_controls_template()$fun)

new_dataset_args(...)
}
\arguments{
\item{main}{\code{character} value of the name of the main component of the directory tree.}

\item{new_dataset_controls}{\code{list} of controls for any new datasets (not in the prefab datasets) listed in \code{datasets} that are to be added to the control list and file.}

\item{models}{\code{character} vector of the names of the models that are to have their controls updated to include the new dataset.}

\item{...}{Named \code{list} of arguments passed to \code{\link{update_list}}.}

\item{fun}{\code{character} value of the generation function.}
}
\value{
\code{dataset_controls_template}: \code{list} of named dataset controls elements, many as \code{NULL}. \cr
\code{new_dataset_controls}: \code{list} of named dataset controls. \cr
\code{new_dataset_metadata}: \code{list} of named dataset metadata elements to the controls \code{list}. \cr
\code{new_dataset_fun}: \code{character} of dataset generation function. \cr
\code{new_dataset_args}: named \code{list} of argument elements to the generating function. \cr
\code{add_new_dataset}: dataset controls \code{list} for the new dataset, \code{\link[base:invisible]{invisible}}-ly.
}
\description{
Create a new dataset's controls. Using the \code{dataset_controls_template} file as a basis for the \code{list} and leveraging element-specific functions to fill in details. \cr
Each of the specific \verb{new_dataset_< >} functions wraps an \code{\link{update_list}} call starting with the \code{\link{dataset_controls_template}} as the main list and taking any named elements as inputs via \code{...}.
}
\details{
Having been created using \code{\link{new_dataset_controls}}, the new dataset's controls can either be added to the directory at directory creation (via \code{\link{setup_dir}} or related \code{\link[=setup_dir]{setup_<>}} functions) or update (via \code{\link{update_dir}}) steps or via \code{\link{add_new_dataset}}.
}
\examples{
\dontrun{
   main1 <- file.path(tempdir(), "new_dataset_controls")
   setup_dir(main = main1)

   dataset_controls_template( )

   args <- new_dataset_args(name     = "newdata", 
                            filename = "rodents_newdata.csv")

   new_controls <- new_dataset_controls(metadata = new_dataset_metadata(name = "newdata"),
                                        args     = args)

   added <- add_new_dataset(main                 = main1, 
                            new_dataset_controls = new_controls,
                            models               = "AutoArima")
 
   portalcast(main     = main1, 
              datasets = "newdata", 
              models   = "AutoArima")

   unlink(main1, recursive = TRUE)
 }

}
\seealso{
Directory customization functions:
\code{\link{new models}}
}
\concept{customize}
