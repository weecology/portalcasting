settings <- read_directory_settings(main = main)

messageq("Reading in casts metadata and evaluation files ...", quiet = settings$quiet)

casts_metadata    <- read_casts_metadata(main = main)
casts_evaluations <- read_casts_evaluations(main = main)

messageq(" ... done.", quiet = settings$quiet)