# this script finds all of the example code in the .R files, collates it, then runs it
#   this is an important workflow because, due to the runtime duration of many examples, we use extensive \dontrun calls
#   as a result, there is no formal check on the code, which means it could break without notifications
#

library(portalcasting)

rooted_in_tests <- FALSE

if (rooted_in_extra_tests) {

  R_scripts <- list.files(file.path("..", "..", "R"), full.names = TRUE)

} else {

  R_scripts <- list.files(file.path("R"), full.names = TRUE)

}

nR_scripts <- length(R_scripts)

examples <- named_null_list(R_scripts)

for (i in 1:nR_scripts) {

  scanned <- scan(R_scripts[i], what = "character", sep = "\n", quiet = TRUE)

  first_line <- grep("#' @examples", scanned)
  last_line  <- which(scanned == "NULL")

  nsets <- length(first_line)

  if (nsets == 0) {
    next
  }

  sets <- list()

  for(j in 1:nsets) {

    adontrun <- any(grepl("dontrun", scanned[first_line[j]:last_line[j]]))

    tidied     <- gsub(" ", "", gsub("\\\\dontrun\\{", "", gsub("@examples", "", gsub("#'", "", scanned[first_line[j]:last_line[j]]))))
    trimmed    <- tidied[!(tidied %in% c("", "NULL"))]
  
    if (adontrun) { 
 
      trimmed <- trimmed[-length(trimmed)]
 
    }

    sets[[j]]  <- trimmed

  }

  examples_i    <- sets

  examples[[i]] <- examples_i

}



