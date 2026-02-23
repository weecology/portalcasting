# Current Models

## Models

We currently analyze and forecast rodent data at Portal using fifteen
models: AutoARIMA, Seasonal AutoARIMA, ESSS, NaiveARIMA, Seasonal
NaiveARIMA, nbGARCH, nbsGARCH, pGARCH, psGARCH, pevGARCH, Random Walk,
Logistic, Logistic Covariates, Logistic Competition, Logistic
Competition Covariates (WeecologyLab 2019).

### AutoARIMA

AutoARIMA (Automatic Auto-Regressive Integrated Moving Average) is a
flexible Auto-Regressive Integrated Moving Average (ARIMA) model. ARIMA
models are defined according to three model structure parameters – the
number of autoregressive terms (p), the degree of differencing (d), and
the order of the moving average (q), and are represented as ARIMA(p, d,
q) (Box and Jenkins 1970). All submodels are fit and the final model is
selected using the `auto.arima` function in the **forecast** package
(Hyndman and Athanasopoulos 2013; Hyndman 2017). AutoARIMA is fit
flexibly, such that the model parameters can vary from fit to fit. Model
forecasts are generated using the `forecast` function from the
**forecast** package (Hyndman and Athanasopoulos 2013; Hyndman 2017).
While the `auto.arima` function allows for seasonal models, the
seasonality is hard-coded to be on the same period as the sampling,
which is not the case for the Portal rodent surveys. As a result, no
seasonal models are evaluated within this model set, but see the
sAutoARIMA model. The model is fit using a normal distribution, and as a
result can generate negative-valued predictions and forecasts.

### Seasonal AutoARIMA

sAutoARIMA (Seasonal Automatic Auto-Regressive Integrated Moving
Average) is a flexible Auto-Regressive Integrated Moving Average (ARIMA)
model. ARIMA models are defined according to three model structure
parameters – the number of autoregressive terms (p), the degree of
differencing (d), and the order of the moving average (q), and are
represented as ARIMA(p, d, q) (Box and Jenkins 1970). All submodels are
fit and the final model is selected using the `auto.arima` function in
the **forecast** package (Hyndman and Athanasopoulos 2013; Hyndman
2017). sAutoARIMA is fit flexibly, such that the model parameters can
vary from fit to fit. Model forecasts are generated using the `forecast`
function from the **forecast** package (Hyndman and Athanasopoulos 2013;
Hyndman 2017). While the `auto.arima` function allows for seasonal
models, the seasonality is hard-coded to be on the same period as the
sampling, which is not the case for the Portal rodent surveys. We
therefore use two Fourier series terms (sin and cos of the fraction of
the year) as external regressors to achieve the seasonal dynamics in the
sAutoARIMA model. The model is fit using a normal distribution, and as a
result can generate negative-valued predictions and forecasts.

### ESSS

ESSS (Exponential Smoothing State Space) is a flexible exponential
smoothing state space model (Hyndman et al. 2008). The model is fit
using the `ets` function in the **forecast** package (Hyndman 2017) with
the `allow.multiplicative.trend` argument set to `TRUE`. Model forecasts
are generated using the `forecast` function from the **forecast**
package (Hyndman and Athanasopoulos 2013; Hyndman 2017). Models fit
using `ets` implement what is known as the “innovations” approach to
state space modeling, which assumes a single source of noise that is
equivalent for the process and observation errors (Hyndman et al. 2008).
In general, ESSS models are defined according to three model structure
parameters – error type, trend type, and seasonality type (Hyndman et
al. 2008). Each of the parameters can be an N (none), A (additive), or M
(multiplicative) state (Hyndman et al. 2008). However, because of the
difference in period between seasonality and sampling of the Portal
rodents combined with the hard-coded single period of the `ets`
function, we do not include the seasonal components to the ESSS model.
ESSS is fit flexibly, such that the model parameters can vary from fit
to fit. The model is fit using a normal distribution, and as a result
can generate negative-valued predictions and forecasts.

### NaiveARIMA

NaiveARIMA (Naive Auto-Regressive Integrated Moving Average) is a fixed
Auto-Regressive Integrated Moving Average (ARIMA) model of order
(0,1,0). The model is fit using the `Arima` function in the **forecast**
package (Hyndman and Athanasopoulos 2013; Hyndman 2017). Model forecasts
are generated using the `forecast` function from the **forecast**
package (Hyndman and Athanasopoulos 2013; Hyndman 2017). While the
`Arima` function allows for seasonal models, the seasonality is
hard-coded to be on the same period as the sampling, which is not the
case for the Portal rodent surveys. As a result, NaiveARIMA does not
include seasonal terms, but see the sNaiveARIMA model. The model is fit
using a normal distribution, and as a result can generate
negative-valued predictions and forecasts.

### Seasonal NaiveARIMA

sNaiveARIMA (Seasonal Naive Auto-Regressive Integrated Moving Average)
is a fixed Auto-Regressive Integrated Moving Average (ARIMA) model of
order (0,1,0). The model is fit using the `Arima` function in the
**forecast** package (Hyndman and Athanasopoulos 2013; Hyndman 2017).
Model forecasts are generated using the `forecast` function from the
**forecast** package (Hyndman and Athanasopoulos 2013; Hyndman 2017).
While the `Arima` function allows for seasonal models, the seasonality
is hard-coded to be on the same period as the sampling, which is not the
case for the Portal rodent surveys. We therefore use two Fourier series
terms (sin and cos of the fraction of the year) as external regressors
to achieve the seasonal dynamics in the NaiveARIMA model. The model is
fit using a normal distribution, and as a result can generate
negative-valued predictions and forecasts.

### nbGARCH

nbGARCH (Negative Binomial Auto-Regressive Conditional
Heteroskedasticity) is a generalized autoregressive conditional
heteroskedasticity (GARCH) model with overdispersion (*i.e.*, a negative
binomial response). The model is fit to the interpolated data using the
`tsglm` function in the **tscount** package (Liboschik et al. 2017).
GARCH models are generalized ARMA models and are defined according to
their link function, response distribution, and two model structure
parameters – the number of autoregressive terms (p) and the order of the
moving average (q), and are represented as GARCH(p, q) (Liboschik et al.
2017). The nbGARCH model is fit using the log link and a negative
binomial response (modeled as an over-dispersed Poisson), as well as
with p = 1 (first-order autoregression) and q = 13 (approximately yearly
moving average). The `tsglm` function in the **tscount** package
(Liboschik et al. 2017) uses a (conditional) quasi-likelihood based
approach to inference and models the overdispersion as an additional
parameter in a two-step approach. This two-stage approach has only been
minimally evaluated, although preliminary simulation-based studies are
promising (Liboschik, Fokianos, and Fried 2017). Forecasts are made
using the **portalcasting** method function `forecast.tsglm` as
`forecast`.

### nbsGARCH

nbsGARCH (Negative Binomial Seasonal Auto-Regressive Conditional
Heteroskedasticity) is a generalized autoregressive conditional
heteroskedasticity (GARCH) model with overdispersion (*i.e.*, a negative
binomial response) and seasonal predictors modeled using two Fourier
series terms (sin and cos of the fraction of the year) fit to the
interpolated data. The model is fit using the `tsglm` function in the
**tscount** package (Liboschik et al. 2017). GARCH models are
generalized ARMA models and are defined according to their link
function, response distribution, and two model structure parameters –
the number of autoregressive terms (p) and the order of the moving
average (q), and are represented as GARCH(p, q) (Liboschik et al. 2017).
The nbsGARCH model is fit using the log link and a negative binomial
response (modeled as an over-dispersed Poisson), as well as with p = 1
(first-order autoregression) and q = 13 (approximately yearly moving
average). The `tsglm` function in the **tscount** package (Liboschik et
al. 2017) uses a (conditional) quasi-likelihood based approach to
inference and models the overdispersion as an additional parameter in a
two-step approach. This two-stage approach has only been minimally
evaluated, although preliminary simulation-based studies are promising
(Liboschik, Fokianos, and Fried 2017). Forecasts are made using the
**portalcasting** method function `forecast.tsglm` as `forecast`.

### pGARCH

pGARCH (Poisson Auto-Regressive Conditional Heteroskedasticity) is a
generalized autoregressive conditional heteroskedasticity (GARCH) model.
The model is fit using the `tsglm` function in the **tscount** package
(Liboschik et al. 2017). GARCH models are generalized ARMA models and
are defined according to their link function, response distribution, and
two model structure parameters – the number of autoregressive terms (p)
and the order of the moving average (q), and are represented as GARCH(p,
q) (Liboschik et al. 2017). The pGARCH model is fit using the log link
and a Poisson response, as well as with p = 1 (first-order
autoregression) and q = 13 (approximately yearly moving average).
Forecasts are made using the **portalcasting** method function
`forecast.tsglm` as `forecast`.

### psGARCH

psGARCH (Poisson Seasonal Auto-Regressive Conditional
Heteroskedasticity) is a generalized autoregressive conditional
heteroskedasticity (GARCH) model with seasonal predictors modeled using
two Fourier series terms (sin and cos of the fraction of the year) fit
to the interpolated data. The model is fit using the `tsglm` function in
the **tscount** package (Liboschik et al. 2017). GARCH models are
generalized ARMA models and are defined according to their link
function, response distribution, and two model structure parameters –
the number of autoregressive terms (p) and the order of the moving
average (q), and are represented as GARCH(p, q) (Liboschik et al. 2017).
The pGARCH model is fit using the log link and a Poisson response, as
well as with p = 1 (first-order autoregression) and q = 13
(approximately yearly moving average). Forecasts are made using the
**portalcasting** method function `forecast.tsglm` as `forecast`.

### pevGARCH

pevGARCH (Poisson Environmental Variable Auto-Regressive Conditional
Heteroskedasticity) is a generalized autoregressive conditional
heteroskedasticity (GARCH) model. The response variable is Poisson, and
a variety of environmental variables are considered as covariates. The
overall model is fit using the **portalcasting** function `meta_tsglm`
that iterates over the submodels, which are fit using the `tsglm`
function in the **tscount** package (Liboschik et al. 2017). GARCH
models are generalized ARMA models and are defined according to their
link function, response distribution, and two model structure parameters
– the number of autoregressive terms (p) and the order of the moving
average (q), and are represented as GARCH(p, q) (Liboschik et al. 2017).
The pevGARCH model is fit using the log link and a Poisson response, as
well as with p = 1 (first-order autoregression) and q = 13 (yearly
moving average). The environmental variables potentially included in the
model are min, mean, and max temperatures, precipitation, and NDVI. The
`tsglm` function in the **tscount** package (Liboschik et al. 2017) uses
a (conditional) quasi-likelihood based approach to inference. This
approach has only been minimally evaluated for models with covariates,
although preliminary simulation-based studies are promising (Liboschik,
Fokianos, and Fried 2017). The overall model is composed of 11 submodels
from a (nonexhaustive) combination of the environmental covariates –
\[1\] max temp, mean temp, precipitation, NDVI; \[2\] max temp, min
temp, precipitation, NDVI; \[3\] max temp, mean temp, min temp,
precipitation; \[4\] precipitation, NDVI; \[5\] min temp, NDVI; \[6\]
min temp; \[7\] max temp; \[8\] mean temp; \[9\] precipitation; \[10\]
NDVI; and \[11\] -none- (intercept-only). The single best model of the
11 is selected based on AIC. Forecasts are made using the
**portalcasting** method function `forecast.tsglm` as `forecast`.

### Random Walk

Random Walk fits a hierarchical log-scale density random walk model with
a Poisson observation process using the JAGS (Just Another Gibbs
Sampler) infrastructure (Plummer 2003). Similar to the NaiveArima model,
Random Walk has an ARIMA order of (0,1,0), but in Random Walk, it is the
underlying density that takes a random walk on the log scale, whereas in
NaiveArima, it is the raw counts that take a random walk on the
observation scale. There are two process parameters – mu (the density of
the species at the beginning of the time series) and sigma (the standard
deviation of the random walk, which is Gaussian on the log scale). The
observation model has no additional parameters. The prior distributions
for mu is informed by the available data collected prior to the start of
the data used in the time series. mu is normally distributed with a mean
equal to the average log-scale density and a standard deviation of 0.04.
sigma was given a uniform distribution between 0 and 1. The Random Walk
model is fit and forecast using the **portalcasting** functions
`fit_runjags` and `forecast.runjags` (called as `forecast`),
respectively.

### Logistic

Logistic fits a hierarchical log-scale density logistic growth model
with a Poisson observation process using the JAGS (Just Another Gibbs
Sampler) infrastructure (Plummer 2003). Building upon the Random Walk
model, Logistic expands upon the “process model” underlying the Poisson
observations. There are four process parameters – mu (the density of the
species at the beginning of the time series), sigma (the standard
deviation of the random walk, which is Gaussian on the log scale), r
(growth rate), and K (carrying capacity). The observation model has no
additional parameters. The prior distributions for mu and K are slightly
informed in that they are vague but centered using the available data
and sigma and r are set with vague priors. mu is normally distributed
with a mean equal to the average log-scale density and a standard
deviation of 0.04. K is modeled on the log-scale with a prior mean equal
to the log maximum count and a standard deviation of 0.04. r is given a
normal prior with mean 0 and standard deviation 0.04. sigma was given a
uniform distribution between 0 and 1. The Logistic model is fit and
forecast using the **portalcasting** functions `fit_runjags` and
`forecast.runjags` (called as `forecast`), respectively.

### Logistic Covariates

Logistic Covariates fits a hierarchical log-scale density logistic
growth model with a Poisson observation process using the JAGS (Just
Another Gibbs Sampler) infrastructure (Plummer 2003). Building upon the
Logistic model, Logistic Covariates expands upon the “process model”
underlying the Poisson observations. There are six process parameters –
mu (the density of the species at the beginning of the time series),
sigma (the standard deviation of the random walk, which is Gaussian on
the log scale), and then intercept and slope parameters for r (growth
rate) and K (carrying capacity) as a function of covariates (r being a
function of the integrated warm rain over the past 3 lunar months and K
being a function of average NDVI over the 13 lunar months). The
observation model has no additional parameters. The prior distributions
for mu and the K interceptare slightly informed in that they are vague
but centered using the available data and sigma and r are set with vague
priors. mu is normally distributed with a mean equal to the average
log-scale density and standard deviation 0.04. The K intercept is
modeled on the log-scale with a prior mean equal to the maximum count
and standard deviation 0.04. The r intercept is given a normal prior
with mean 0 and standard deviation 0.04. sigma was given a uniform
distribution between 0 and 1. The slopes for r and log-scale K were
given priors with mean 0 and standard deviation 1. The Logistic
Covariates model is fit and forecast using the **portalcasting**
functions `fit_runjags` and `forecast.runjags` (called as `forecast`),
respectively.

### Logistic Competition

Logistic Competition fits a hierarchical log-scale density logistic
growth model with a Poisson observation process using the JAGS (Just
Another Gibbs Sampler) infrastructure (Plummer 2003). Building upon the
Logistic model, Logistic Competition expands upon the “process model”
underlying the Poisson observations. There are six process parameters –
mu (the density of the species at the beginning of the time series),
sigma (the standard deviation of the random walk, which is Gaussian on
the log scale), and then intercept and slope parameters for r (growth
rate) and K (carrying capacity) as a function of competitior density (K
being a function of current DO counts). The observation model has no
additional parameters. The prior distributions for mu and the K
intercept are slightly informed in that they are vague but centered
using the available data and sigma and r are set with vague priors. mu
is normally distributed with a mean equal to the average log-scale
density and standard deviation 0.04. The K intercept is modeled on the
log-scale with a prior mean equal to the maximum of `past counts` and
standard deviation 0.04. The r intercept is given a normal prior with
mean 0 and standard deviation 0.04. sigma was given a uniform
distribution between 0 and 1. The slope for log-scale K was given a
prior with mean 0 and standard deviation 1. The Logistic Competition
model is fit and forecast using the **portalcasting** functions
`fit_runjags` and `forecast.runjags` (called as `forecast`),
respectively.

### Logistic Competition Covariates

Logistic Competition Covariates fits a hierarchical log-scale density
logistic growth model with a Poisson observation process using the JAGS
(Just Another Gibbs Sampler) infrastructure (Plummer 2003). Building
upon the Logistic model, Logistic Competition Covariates expands upon
the “process model” underlying the Poisson observations. There are seven
process parameters – mu (the density of the species at the beginning of
the time series), sigma (the standard deviation of the random walk,
which is Gaussian on the log scale), and then intercept and slope
parameters for r (growth rate) and K (carrying capacity) with r being a
function of the integrated warm rain over the past 3 lunar months and K
being a function of average NDVI over the past 13 lunar months as well
as a function of current DO count). The observation model has no
additional parameters. The prior distributions for mu and the K
intercept are are slightly informed in that they are vague but centered
using the available data and sigma and r are set with vague priors. mu
is normally distributed with a mean equal to the average log-scale
density and standard deviation 0.04. The K intercept is modeled on the
log-scale with a prior mean equal to the maximum count and standard
deviation 0.04. The r intercept is given a normal prior with mean 0 and
standard deviation 0.04. Sigma was given a uniform distribution between
0 and 0.001. The slope for r was given a normal prior with mean 0 and
standard deviation 1, whereas the normal priors for the log K slopes
were given mean 0 and standard deviation 0.0625. The Logistic
Competition Covariates model is fit and forecast using the
**portalcasting** functions `fit_runjags` and `forecast.runjags` (called
as `forecast`), respectively.

### Ensemble

In addition to the base models, we include a starting-point ensemble.
Prior to v0.9.0, the ensemble was based on AIC weights, but in the shift
to separating the interpolated from non-interpolated data in model
fitting, we had to transfer to an unweighted average ensemble model. The
ensemble mean is calculated as the mean of all model means and the
ensemble variance is estimated as the sum of the mean of all model
variances and the variance of the estimated mean, calculated using the
unbiased estimate of sample variances. Given that the current ensemble
is unweighted and includes a number of very naive models, we do not
currently consider it the best model for forecasting.

## References

Box, G., and G. Jenkins. 1970. *Time Series Analysis: Forecasting and
Control*. Holden-Day.

Hyndman, R. J. 2017. “**forecast**: Forecasting Functions for Time
Series and Linear Models.” 2017. <http://pkg.robjhyndman.com/forecast>.

Hyndman, R. J., and G. Athanasopoulos. 2013. *Forecasting: Principles
and Practice*. OTexts.

Hyndman, R. J., A. b. Koehler, J. K. Ord, and R. D. Snyder. 2008.
*Forecasting with Exponential Smoothing: The State Space Approach*.
Springer-Verlag.

Liboschik, T., K. Fokianos, and R. Fried. 2017. “**tscount**: An r
Package for Analysis of Count Time Series Following Generalized Linear
Models.” *Journal of Statistical Software* 82: 1–51.
<https://www.jstatsoft.org/article/view/v082i05>.

Liboschik, T., R. Fried, K. Fokianos, and P. Probst. 2017. “**tscount**:
Analysis of Count Time Series.” 2017.
<https://CRAN.R-project.org/package=tscount>.

Plummer, M. 2003. “A Program for Analysis of Bayesian Graphical Models
Using Gibbs Sampling.” *Proceedings of the 3rd International Workshop on
Distributed Statistical Computing*.
<http://www.ci.tuwien.ac.at/Conferences/DSC-2003/Proceedings/Plummer.pdf>.

WeecologyLab. 2019. “Portal Forecasting.” 2019.
<https://github.com/weecology/portalPredictions/>.
