# working here to really try and take the modeling to the next level wrt implementation

# use count as the response object's name, rather than abundance


count <- (read_rodents(main))[[1]][,4,drop=T]

ept <- function (x) {
  eval(parse(text = x))
}


do.call(what = auto.arima, args = list(y = y))

getElement

  model_controls <- read_model_controls(main)
  nmodels        <- length(model_controls)

  model_calls    <- named_null_list(names(model_controls))

  for (i in 1:nmodels) {

    what <- model_controls[[i]]$fun
    args <- model_controls[[i]]$args

    nargs <- length(args)

    for (j in 1:nargs) {

      to_ept <- grepl("^ept__", args[[j]])

      if (to_ept[1]) {  # really bad patch

        args[[j]] <- ept(gsub("ept__", "", args[[j]]))

      }

    }

    model_calls[[i]] <- list(what = what,
                             args = args)

  }
i<-1
model_calls[[i]]

eval(parse(text =model_calls[[i]]$args$y))


i<-2
output  <- named_null_list(names(model_calls))
for (i in 1:nmodels) {

  output[[i]] <- do.call(model_calls[[i]]$what, model_calls[[i]]$args)

}

output