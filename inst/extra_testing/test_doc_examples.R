# this script finds all of the example code in the .Rd files, collates it, then runs it
#   this is an important workflow because, due to the runtime duration of many examples, we use extensive \dontrun calls
#   as a result, there is no formal check on the code, which means it could break without notifications
#

Rd  <- list.files(file.path("man"), full.names = TRUE)
Rd  <- Rd[-which(Rd == "man/figures")]
nRd <- length(Rd)

Rdnames <- gsub(".Rd", "", gsub("man/", "", Rd))
examples <- named_null_list(Rdnames)

for (i in 1:nRd) {

  ex <- dr <- dt <- tlb <- slb <- lb <- NULL

  scanned <- scan(Rd[i], what = "character", sep = "\n", quiet = TRUE)

  ex <- grep("examples\\{", scanned)

  if (length(ex) == 0) {

    next

  }

  lb <- max(which(gsub(" ", "", scanned) == "}"))

  dr <- grep("dontrun\\{", scanned)

  if (length(dr) == 1) {

    bs <- (which(gsub(" ", "", scanned) == "}"))
    nbs <- length(bs)
    slb <- bs[nbs - 1]

  }

  dt <- grep("donttest\\{", scanned)

  if (length(dt) == 1) {

    bs <- (which(gsub(" ", "", scanned) == "}"))
    nbs <- length(bs)
    tlb <- bs[nbs - 1]

  }

  scanned[c(ex, dr, dt, tlb, slb, lb)] <- paste0("#", scanned[c(ex, dr, dt, tlb, slb, lb)])

  examples_i <- scanned[ex:lb]

  examples[[i]] <- examples_i

}

times <- named_null_list(Rdnames)
nlines <- sapply(examples, length)

for(i in 1:nRd) {

  times[[i]] <- list()

  for (j in 1:nlines[i]) {

    times[[i]][[j]] <- tryCatch(system.time({eval(parse(text = examples[[i]][j]))}),
                                error = function(x) {NA})

  }

}


