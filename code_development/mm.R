


Model			Fit Fun	Fit Args		Cast Fun
AutoArima		auto.arima	y = abundance	cast_fit


  model_fit   <- auto.arima(y     = abundance)
  model_cast  <- forecast(object  = model_fit, 
                          h       = metadata$time$lead_time_newmoons, 
                          level   = metadata$confidence_level)
  model_cast  <- data.frame(pred  = model_cast$mean,
                            lower = model_cast$lower[,1], 
                            upper = model_cast$upper[,1])


NaiveArima

  model_fit   <- Arima(y     = abundance, 
                  order = c(0, 1, 0))
  model_cast  <- forecast(object = model_fit, 
                     h      = metadata$time$rodent_lead_time, 
                     level  = metadata$confidence_level)

  model_cast  <- data.frame(pred  = model_cast$mean,
                          lower = model_cast$lower[,1], 
                          upper = model_cast$upper[,1])


ESSS

  model_fit   <- ets(y = abundance)
  model_cast  <- forecast(object = model_fit, 
                     h      = metadata$time$rodent_lead_time, 
                     level  = metadata$confidence_level,
                     allow.multiplicative.trend = TRUE)

  model_cast  <- data.frame(pred  = model_cast$mean,
                          lower = model_cast$lower[,1], 
                          upper = model_cast$upper[,1])


nbGARCH

    past <- list(past_obs  = 1, 
                 past_mean = 13)
    model_fit <- tryCatch(
                   tsglm(ts    = abundance, 
                         model = past, 
                         distr = "nbinom", 
                         link  = "log"),
                   warning = function(x){NA}, 
                   error = function(x){NA})
    if(all(is.na(model_fit)) || AIC(model_fit) == Inf){
      model_fit <- tryCatch(
                   tsglm(ts    = abundance, 
                         model = past, 
                         distr = "poisson", 
                         link  = "log"),
                     warning = function(x){NA}, 
                     error = function(x){NA})
    }
    if(!all(is.na(model_fit))){
      model_cast <- predict(object  = model_fit, 
                       n.ahead = metadata$time$rodent_lead_time, 
                       level   = metadata$confidence_level)
      model_cast <- data.frame(pred = as.numeric(model_cast$pred),
                       lower = as.numeric(model_cast$interval[ , 1]),
                       upper = as.numeric(model_cast$interval[ , 2]))
    }  