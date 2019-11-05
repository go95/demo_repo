library(pbapply)
library(dplyr)


generate_data <- function(k, m, sd) {
  return(rnorm(k, m, sd))
}


estimate_ci <- function(data) {
  est_m <- mean(data)
  est_sd <- sd(data)
  est_k <- length(data)
  return(
    c(
      est_m - 1.96*est_sd/sqrt(est_k-1),
      est_m + 1.96*est_sd/sqrt(est_k-1)
      )
    )
}


evaluate <- function(est, m) {
  return(c(est[1] < m && m < est[2], est[2] - est[1]))
}


iteration <- function(k, m, sd) {
  data <- generate_data(k, m, sd)
  est <- estimate_ci(data)
  return(evaluate(est, m))
}

iteration(10, 100, 30)

simulation <- function(n, k, m, sd) {
  results <- pbsapply(1:n, function(x) {iteration(k, m, sd)})
  rowMeans(results)
}

simulation(1000, 1000, 100, 5)

sim_data <- data.frame(n=(1:10) * 100)
stats <- t(sapply(sim_data$n,
                  function(n) {
  simulation(1000, n, 100, 5)
  }
))

sim_data <- cbind(sim_data, stats)



# RD


generate_data <- function(n) {
  base_ec_results <- runif(n, 0, 5)
  share <- (runif(n, 0, 10) + base_ec_results)/15
  winner <- share > 0.5
  ec_results <- base_ec_results + winner*3 + rnorm(n)
  return(data.frame(share=share,
                    winner=winner,
                    ec_results=ec_results))
}

data <- generate_data(10000)

estimate <- function(data, h) {
  data <- data[abs(data$share - 0.5) < h,]
  return(mean(data$ec_results[data$winner]) -
    mean(data$ec_results[!data$winner]))
}

evaluate <- function(est) {
  return((est - 3))
}

iteration <- function(k, h) {
  data <- generate_data(k)
  est <- estimate(data, h)
  return(evaluate(est))
}

simulation <- function(n, k, h) {
  results <- pbsapply(1:n, function(x) {iteration(k, h)})
  c(mean(results), var(results))
}

simulation(1000, 10000, 0.7)
simulation(1000, 10000, 0.6)
simulation(1000, 10000, 0.5)
simulation(1000, 10000, 0.4)
simulation(1000, 10000, 0.3)
simulation(1000, 10000, 0.2)
simulation(1000, 10000, 0.1)
