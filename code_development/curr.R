devtools::load_all()
g<-jags_logistic(main="~/pct", dataset="all", species="DM", control_runjags = runjags_control(burnin=100, adapt = 100, sample = 100, thin = 1))
  
currently working through updates to the jags models
have done a round through all of the jags models
still lots to tidy and generalize tho



working in the portalcast function to loop over the model dataset species combos
currently should work for all of the models

main <- "~/pct"
main <- "~/portalcasting"

mc <- model_controls(main)
md <- read_metadata(main)

models <- names(mc)
nmodels <- length(mc)

for (i in 1:nmodels) {

  datasets  <- names(mc[[i]]$datasets)
  ndatasets <- length(datasets)

  for (j in 1:ndatasets) {

    rodents_table <- read_rodents_table(main, dataset = datasets[j])

    moons_in <- rodents_table$newmoonnumber >= md$time$start_moon & rodents_table$newmoonnumber <= md$time$end_moon

    species   <- mc[[i]]$datasets[[j]]$species
    nspecies  <- length(species)   

    for(k in 1:nspecies) {

      abundance <- rodents_table[moons_in, species[k]]

      if (mc[[i]]$interpolate$needed) {

        abundance <- do.call(what = mc[[i]]$interpolate$fun,
                             args = list(abundance))

      }

      mod_out <- do.call(what = mc[[i]]$fun,
                         args = list(abundance        = abundance,
                                     lead_time        = md$time$lead_time,
                                     confidence_level = md$confidence_level))     
      mod_metadata <- list(time             = md$time,
                           dataset          = dataset[j],
                           species          = species[k],
                           model            = models[i],
                           cast_group       = md$cast_group,
                           cast_type        = md$cast_type,
                           confidence_level = md$confidence_level)

      update_list(mod_out, metadata = mod_metadata)
 

    }
  }
}

# can we actually do this? it seems like a goose chase to get a very tidy function wrapper

auto_arima <- function (abundance, lead_time, confidence_level) {

  fit  <- auto.arima(y = abundance)
  cast <- forecast(fit, h = lead_time, level = confidence_level)
  list(fit  = fit,
       cast = cast)
}

