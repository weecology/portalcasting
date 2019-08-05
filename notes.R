# stripping down the package to build it back up in a more generalized
#  fashion
# the key here will then be that the data loading capacity of R will allow us 
#  to retain a list that has the set up for any specific forecasting pipeline
#  like portal
# we'll want some utilities to make constructing the list easier


downloads <- list(PD = list(type = "zenodo", concept_rec_id = "1215988"),
                  PP = list(type = "zenodo", concept_rec_id = "833438"))

PortalPredictions <- list(downloads = downloads)

# we also might want to create a simple repo for testing!
# basically, like the PD and PP, but just for tests
#
# can we set it up so we just run a do.call on setup_dir with 
# PortalPredictions !!!

# later capacity: 
#  1. make it so download version numbers are retained to allow checking to
#  prevent unneeded downloads
#  2. github
# 

# important notes for news
#  got rid of most of the unnecessary lists
#   dirtree is no more, even tho the structure is still there