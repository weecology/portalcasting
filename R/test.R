
data_control2 <- function(moons = moons_control(),
                         rodents = rodents_control(),
                         covariates = covariates_control(),
                         metadata = metadata_control()){

  list(moons = moons, rodents = rodents, covariates = covariates,
       metadata = metadata)
}

