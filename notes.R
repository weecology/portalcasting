# to do:
# add to the vignette (issue 145)
# fix plotting bug (issue 140)
# release

devtools::document()
devtools::load_all()

main <- "~/portalcasting_testing"
portalcast(main = main, models = c("nbsGARCH"), 
           end_moons = 512)
plot_cast_ts(main=main)

plot_cast_point(main=main)
plot_cast_point(main=main,with_census=T)
plot_casts_err_lead(main)
plot_casts_cov_RMSE(main)


