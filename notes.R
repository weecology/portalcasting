# in the thick of a major refactor again now
# may not want to go full on but will need to very soon for a variety of reasons
# at this point, currently have a working AutoArima set up within the model construction space
#
# push pause here to think about end user design space now
# will need to mull on the endpoints specifically
# what are the tools that the user wants at the command line?


devtools::document()
devtools::load_all()


main <- "./testing"

setup_production(main)




create_dir(main)
write_directory_config(main)
fill_dir(main)



# fundamental time unit is now day
# this will enable use of a much more rich array of models
# in forecast_future_moons, we divide days by the average newmoon duration, 29.53 days
#  still results in 12 steps forward under default 