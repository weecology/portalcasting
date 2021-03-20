
    past_count <- data$past_count 
    past_moon <- data$past_moon 
    past_N <- data$past_N
    moon <- data$moon

    mean_past_count <- mean(past_count)
    max_past_count <- max(past_count)
    sd_past_count <- sd(past_count)
    
    last_count <- past_count[past_N]
    last_moon <- past_moon[past_N]
    first_moon <- moon[1]
    last_time_diff <- first_moon - last_moon
    
    mean_past_diff_rate <- mean(past_diff_rate)
    sd_past_diff_rate <- max(c(sd(past_diff_rate) * sqrt(2), 0.01))
    var_past_diff_rate <- sd_past_diff_rate^2
    precision_past_diff_rate <- 1/(var_past_diff_rate)

    pred_mu <- last_count + last_time_diff * mean_past_diff_rate

    rate <- 100
    shape <- precision_past_diff_rate * rate



    mean_past_count <- mean(past_count)
    max_past_count <- max(past_count)
    sd_past_count <- max(c(sd(past_count) * sqrt(2), 0.01))
    var_past_count <- sd_past_count^2
    precision_past_count <- 1/(var_past_count)

    last_count <- past_count[past_N]
    last_moon <- past_moon[past_N]
    first_moon <- moon[1]
    last_time_diff <- first_moon - last_moon

    mean_past_diff_rate <- mean(past_diff_rate)
    sd_past_diff_rate <- max(c(sd(past_diff_rate) * sqrt(2), 0.01))
    var_past_diff_rate <- sd_past_diff_rate^2
    precision_past_diff_rate <- 1/(var_past_diff_rate)

    pred_mu <- max(c(last_count + last_time_diff * mean_past_diff_rate,
                     1e-4))

    rate <- 100
    shape <- precision_past_diff_rate * rate





