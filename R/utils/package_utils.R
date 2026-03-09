#' @title Find an Object's Host Package and Version Information
#'
#' @description Locate basic package information of an R object. If nothing is input, it operates on itself. \cr
#'              If the object is sourced through multiple packages, each package and its version are included.
#'
#' @param what An R object.
#'
#' @return `list` of the object, its class, the packages it is sourced from / through, and the versions of those packages.
#'
#' @name package_version_finder
#'
#' @family utilities
#'
#' @examples
#'    package_version_finder( )
#'
NULL

#' @rdname package_version_finder
#'
#' @export
#'
package_version_finder <- function (what) {

  if (missing(what)) {

    what <- "package_version_finder"

  }

  object_expr       <- parse(text          = what)
  object_eval       <- eval(expr           = object_expr)
  object_class      <- class(x             = object_eval)

  helps             <- help.search(pattern = what, 
                                   agrep   = FALSE)

  packages_names    <- helps$matches$Package

  packages_versions <- sapply(X           = packages_names,
                              FUN         = packageDescription,
                              fields      = "Version")
  
  names(packages_versions) <- packages_names

  list(object   = what,
       class    = object_class,
       package  = packages_names,
       version  = packages_versions)

}
