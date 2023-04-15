# this script finds all of the code in the .Rmd files, collates it, then runs it
#   this is an important workflow because, due to the runtime duration of many examples, we use eval=FALSE for many code chunks
#   as a result, there is no formal check on the code, which means it could break without notifications
#

options(knitr.purl.inline = TRUE)

Rmd  <- list.files(file.path("vignettes"), full.names = TRUE)
Rmd  <- Rmd[grepl("\\.Rmd", Rmd)]
nRmd <- length(Rmd)

Rmdnames  <- gsub(".Rmd", "", gsub("vignettes/", "", Rmd))
times     <- named_null_list(Rmdnames)

dir.create(path = file.path(tempdir(), "vignettes"),
           showWarnings = FALSE)

for (rmd in 1:nRmd) {

  print(Rmdnames[rmd])

  temp_name  <- file.path(tempdir(), "vignettes", Rmdnames[rmd])
  txt_name   <- paste0(temp_name, ".txt")
  R_name     <- paste0(temp_name, ".R")

  purled    <-  knitr::purl(Rmd[rmd], output = txt_name, quiet = TRUE, documentation = 0)

  scanned <- scan(purled, what = "character", sep = "\n", quiet = TRUE)

  tidied  <- gsub("#> ", "", gsub("#> #>", "##", scanned))

  cropped <- tidied[!(substr(tidied, 1, 2) == "##")]

  outted  <- write(cropped, file = R_name)

  times[[rmd]] <- tryCatch(system.time({source(R_name)}),
                           error = function(x) {NA})

}

print(times)

if (any(is.na(times))) {

  nas <- Rmdnames[which(is.na(times))]
  stop(paste("Vignette ", nas, " errored during build"))

}

