# there are going to be a handful of specific needs here, list them out
#  need standardized handlers for reading and writing

# Specialized handlers for reading model control yaml files
# The biggest issue is the yes / no input for booleans
#  We need "y" for actual inputs! 

read_model_handlers <- function ( ) {

  y_handler <- function (x) {

    if (x == "y") {"y"} else {TRUE} 

  }

  n_handler <- function (x) {

    if (x == "n") {"n"} else {FALSE}

  }

  list("bool#yes" = y_handler,
       "bool#no"  = n_handler)
  
}

write_model_handlers <- function ( ) {

  logical_handler <- function (x) {

    result        <- ifelse(x, "TRUE", "FALSE")
    class(result) <- "verbatim"
    return(result)

  }

  list(logical = logical_handler)

}

read_model_yaml <- function (file, handlers = read_model_handlers() ) {

  read_yaml(file = file, handlers = handlers)

}