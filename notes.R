
devtools::document()
devtools::load_all()


main <- "./testing2"

create_dir(main)


setup_dir(main)
portalcast(main, models = "ESSS")

plot_cast_ts(main = main, data_set = "controls")
plot_cast_point(main = main, data_set = "controls")
most_ab <- most_abundant_species(main = main, data_set = "controls")
for(i in 1:3){
  plot_cast_ts(main = main, data_set = "controls", species = most_ab[i])
}
plot_casts_err_lead(main = main)
plot_casts_cov_RMSE(main = main)


devtools::build()
devtools::test()



