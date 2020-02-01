evaluate <- function(){

  # for each cast
  evaluate_cast()

}

evaluate_cast <- function(observations, prediction, metric, ...){

  # for each species
  #for(i in 1:nspecies){
  #  eval[i] <- evaluate_observations(observations = observations, 
  #                                   prediction = prediction, 
  #                                   metric = metric, ...)
  #}
}

evaluate_observations <- function(observations, prediction, metric, ...){

  # for each observation
  #for(i in 1:nobs){
  #  eval[i] <- evaluate_observation(observation = observations[i], 
  #                                  prediction = prediction,
  #                                  metric = metric, ...)
  #}
}

evaluate_observation <- function(observation, prediction, metric, ...){

  #do.call(metric, observation = observation, prediction = prediction, ...)
}


