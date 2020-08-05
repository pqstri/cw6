#### Sample size simulation ####

# N. of patients
ss <- 100

# N. of treatment group
n_trt <- 3

# A priori probability of having the event PASI90 response
baseline_p <- 0.80

# Size of the effect
effect <- 0.16

# N. of iteration in the simulation process
iteration <- 1000

# N. of status the marker can assume
n_marker <- 2 # can be positive (1) or negative (2)

# Set the alpha level
alpha <- 0.05

# Set up
results <- tibble(cicle = 1:iteration, p = NA)
n_trt <- n_trt*n_marker

# Start the loop
for (cicle in 1:iteration) {
  responders_48w_trt1_mp <- rbinom(ss / n_trt, 1, baseline_p+effect)
  responders_48w_trt1_mn <- rbinom(ss / n_trt, 1, baseline_p-effect)
  responders_48w_trt2_mp <- rbinom(ss / n_trt, 1, baseline_p-effect)
  responders_48w_trt2_mn <- rbinom(ss / n_trt, 1, baseline_p+effect)
  responders_48w_trt3_mp <- rbinom(ss / n_trt, 1, baseline_p)
  responders_48w_trt3_mp <- rbinom(ss / n_trt, 1, baseline_p)
  
  temp <- tibble(response = c(responders_48w_trt1_mp,
                              responders_48w_trt1_mn,
                              responders_48w_trt2_mp,
                              responders_48w_trt2_mn,
                              responders_48w_trt3_mp,
                              responders_48w_trt3_mp),
                 trt = c(rep("1", ss / n_trt), 
                         rep("1", ss / n_trt),
                         rep("2", ss / n_trt),
                         rep("2", ss / n_trt),
                         rep("3", ss / n_trt),
                         rep("3", ss / n_trt)),
                 mrk = c(rep("p", ss / n_trt), 
                         rep("n", ss / n_trt),
                         rep("p", ss / n_trt),
                         rep("n", ss / n_trt),
                         rep("p", ss / n_trt),
                         rep("n", ss / n_trt)))
  
  model_1 <- glm(family = "binomial", data = temp, response ~ mrk * trt)
  model_2 <- glm(family = "binomial", data = temp, response ~ mrk + trt)
  results$p[cicle] <- epiDisplay::lrtest(model_1, model_2)$p
}

# Test the power
mean(results$p < alpha)




