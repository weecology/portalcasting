# another major directory update associated with breaking apart the forecasts by species
# and renaming casts as forecasts within the files

# definitely still a work in progress


main <- "~/pc"

casts_metadata <- read_casts_metadata(main)
moons <- read_moons(main)

head(moons)

casts_metadata$t0      <- moons$newmoondate[match(casts_metadata$start_moon, moons$newmoonnumber)]
casts_metadata$origin  <- moons$newmoondate[match(casts_metadata$end_moon, moons$newmoonnumber)]
casts_metadata$horizon <- round(casts_metadata$lead_time * 29.5)

template_row <- data.frame(forecast_id             = integer(),
                           forecast_group          = integer(),
                           forecast_date           = character(),
                           t0                      = integer(),
                           origin                  = character(),
                           horizon                 = integer(),
                           model                   = character(), 
                           dataset                 = character(),
                           species                 = character(),
                           portalcasting_version   = character(),
                           QAQC                    = logical(),
                           notes                   = character())

template_row

nr <- nrow(casts_metadata)
for (i in 1:nr) {

  casts_metadata[i, ]

  

}


