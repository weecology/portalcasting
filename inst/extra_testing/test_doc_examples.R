# this script finds all of the example code in the .Rd files, collates it, then runs it
#   this is an important workflow because, due to the runtime duration of many examples, we use extensive \dontrun calls
#   as a result, there is no formal check on the code, which means it could break without notifications
#

Rd  <- list.files(file.path("man"), full.names = TRUE)
Rd  <- Rd[-which(Rd == "man/figures")]
nRd <- length(Rd)

Rdnames <- gsub(".Rd", "", gsub("man/", "", Rd))

times <- named_null_list(Rdnames)

dir.create(path = file.path(tempdir(), "examples"),
           showWarnings = FALSE)

for (rd in 1:nRd) {

  print(Rdnames[rd])

  rex <- dr <- dt <- tlb <- slb <- lb <- NULL

  scanned <- scan(Rd[rd], what = "character", sep = "\n", quiet = TRUE)

  ref <- grep("references\\{", scanned)

  if (ref) {

    scanned <- scanned[1:(ref - 1)]

  }

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

  scanned[c(ex, dr, dt, tlb, slb, lb)] <- paste0("##", scanned[c(ex, dr, dt, tlb, slb, lb)])

  marked <- scanned[ex:lb]

  cropped <- marked[!(substr(marked, 1, 2) == "##")]

  temp_name  <- file.path(tempdir(), "examples", Rdnames[rd])
  R_name     <- paste0(temp_name, ".R")  

  pasted_rd <- paste(cropped, collapse = "\n")

  write(x    = pasted_rd,
        file = R_name)

  times[[rd]] <- tryCatch(system.time({source(R_name)}),
                          error = function(x) {NA})  

}

print(times)

if (any(is.na(times))) {

  nas <- Rdnames[which(is.na(times))]
  stop(paste("Example ", nas, " errored during test"))

}
