#
# streamline population modeling
# the ricker model works well but there's so much clutter and the logs etc
# make it really easy and clean to construct the process 
#

#
#  
#


devtools::load_all()
main <- "./testing"

rodents_table <- read_rodents_table(main)
metadata <- read_metadata(main)
species <- "DM"

metadata$start_moon
metadata$end_moon





