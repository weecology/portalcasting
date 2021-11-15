
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