#
# creating the historical covariate forecasts from before we started saving
#  our covariate forecasts as well
#

fill_fcast <- function(dtable){

  t_date <- as.Date(paste(dtable$year, dtable$month, "1", sep = "-"))

  dtable <- dtable[t_date >= "1982-07-01" & t_date <= "2018-04-01", ]
  lead7s <- which(colnames(dtable) == "lead7")
  if (length(lead7s) > 0){
    dtable <- dtable[ , -lead7s]
  }
  dtable_filled <- dtable

  for(i in which(colnames(dtable) %in% sprintf("lead%d", 1:6))){
    dtable_filled[ , i] <- round(fillin(dtable[ , i]), 3)
  }

  return(dtable_filled)
}

fillin <- function(xx){
  nnas <- length(which(is.na(xx)))
  while (nnas > 0){
    xx <- fillin_one(xx)
    nnas <- length(which(is.na(xx)))
  }
  return(xx)
}

fillin_one <- function(xx){

  nas <- which(is.na(xx))
  nas_first <- nas[1]
  diff_nas <- diff(nas)
  nas_ingroup <- c(nas_first, nas[which(diff_nas == 1) + 1])
  xx1 <- xx[-(nas_first:length(xx))]
  fcastxx1 <- forecast::auto.arima(xx1)
  fcastnas <- forecast::forecast(fcastxx1, length(nas_ingroup))$mean
  xx[nas_ingroup] <- fcastnas
  return(xx)
}


hist_tmean <- read.csv("tmean_forecasts.csv")
hist_precip <- read.csv("precip_forecasts.csv")

hist_tmean_filled <- fill_fcast(hist_tmean)
hist_precip_filled <- fill_fcast(hist_precip) 

fc_nm <- rep(403:499, each = 6)
nmn <- numeric(0)
for (i in 403:499){
  nmn <- c(nmn, i + (1:6))
}

hist_tab <- data.frame(forecast_newmoon = fc_nm, newmoonnumber = nmn, 
  mintemp = NA, maxtemp = NA, meantemp = NA, precipitation = NA, ndvi = NA)

moons$newmoonyear <- as.numeric(format(as.Date(moons$newmoondate), "%Y"))
moons$newmoonmonth <- as.numeric(format(as.Date(moons$newmoondate), "%m"))

for (i in 403:499){

  yr <- moons$newmoonyear[moons$newmoonnumber == i]
  mo <- moons$newmoonmonth[moons$newmoonnumber == i]
  matchd <- which(hist_tmean_filled$year == yr & 
              hist_tmean_filled$month == mo)
  nxt6 <- matchd + 1:6
  nxt6t <- hist_tmean_filled[nxt6, ]
  nxt6m <- nxt6t[ , 3:8]
  fcast_vals <- round(diag(as.matrix(nxt6m)), 3)
  hist_tab[which(hist_tab$forecast_newmoon == i), "meantemp"] <- fcast_vals

  matchd <- which(hist_precip_filled$year == yr & 
              hist_precip_filled$month == mo)
  nxt6 <- matchd + 1:6
  nxt6t <- hist_precip_filled[nxt6, ]
  nxt6m <- nxt6t[ , 3:8]
  fcast_vals <- round(diag(as.matrix(nxt6m)), 3)
  hist_tab[which(hist_tab$forecast_newmoon == i),
    "precipitation"] <- fcast_vals
}

ndvi_data <- select(covariates, c("newmoonnumber", "ndvi"))

for (i in 403:499){
  ndvi_temp <- ndvi_data[ndvi_data$newmoonnumber <= i, ]
  ndvi_fcast <- fcast_ndvi(ndvi_temp, "newmoon", lead = 6, moons)$ndvi
  hist_tab[hist_tab$forecast_newmoon == i, "ndvi"] <- ndvi_fcast
}


# according to the climate folks, min and max temp are predicted from combos
#  of mean temp and precip
#  -using the best-fit for each and including prediction error

minmod <- lm(mintemp ~ meantemp + precipitation, data = covariates)
minpred <- predict(minmod, 
             newdata = list(meantemp = hist_tab$meantemp, 
                            precipitation = hist_tab$precipitation),
             se.fit = TRUE, interval = "prediction")
mintemps <- rnorm(nrow(hist_tab), minpred$fit[,1], minpred$residual.scale)

maxmod <- lm(maxtemp ~ meantemp * precipitation, data = covariates)
maxpred <- predict(maxmod,
             newdata = list(meantemp = hist_tab$meantemp, 
                            precipitation = hist_tab$precipitation),
             se.fit = TRUE, interval = "prediction")
maxtemps <- rnorm(nrow(hist_tab), maxpred$fit[,1], maxpred$residual.scale)

hist_tab$mintemp <- mintemps
hist_tab$maxtemp <- maxtemps
hist_tab$source <- "retroactive"
hist_tab$date_made <- Sys.Date()

write.csv(hist_tab, "covariate_forecasts.csv", row.names = FALSE)


