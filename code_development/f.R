                 past_N                 = length(past_count)
    log_past_count      <- log(past_count + 0.1)
    mean_log_past_count <- mean(log_past_count)
    sd_log_past_count   <- max(c(sd(log_past_count) * sqrt(2), 0.01))
    diff_log_past_count <- rep(NA, past_N - 1)

    log_bonus_max_past_count <- max(log(past_count * 1.2))

    for (i in 1:(past_N - 1)) {

      diff_count             <- log_past_count[i + 1] - log_past_count[i]
      diff_time              <- past_moon[i + 1] - past_moon[i] 
      diff_log_past_count[i] <- diff_count / diff_time

    }

    sd_diff_log_past_count        <- max(c(sd(diff_log_past_count) * sqrt(2), 0.01))
    var_diff_log_past_count       <- sd_diff_log_past_count^2
    precision_diff_log_past_count <- 1/(var_diff_log_past_count)
    rate                          <- 5
    shape                         <- precision_diff_log_past_count * rate

plot(density(from = 0, rgamma(1e5, rate = rate, shape = shape)))

plot(density(exp(rnorm(1e5, 3.7, rgamma(1e5, rate = rate, shape = shape^(-1/2))))))