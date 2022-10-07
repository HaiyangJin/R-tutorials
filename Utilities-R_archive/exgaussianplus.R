library(retimes)

# Create a dataset from the ex-gaussian distribution
rexgaussplus <- function(n = 100, mu = 0, sigma = 1, tau = 1) {
  
  if (tau > 0) { # right skewness
    x = rnorm(n, mean = mu, sd = sigma) + rexp(n, rate = 1/tau)
  } else if (tau < 0) { # left skewness
    x = rnorm(n, mean = mu, sd = sigma) - rexp(n, rate = -1/tau)
  } else if (tau == 0) { # normal distribution
    x = rnorm(n, mean = mu, sd = sigma)
  }
  
  return(x)
}


# check the direction of skewness
checktaudir <- function(x) {
  diff = mean(x) - median(x)
  
  if (diff >= 0) {
    dir = 1
  } else if (diff < 0) {
    dir = -1
  }
  
  return(dir)
}

# model ex-guassian distribution
mexgaussianplus <- function(x, plot = TRUE) {
  taudir <- checktaudir(x)
  
  output <- mexgauss(taudir*x)
  
  output[1] = output[1] * taudir
  output[3] = output[3] * taudir
  
  return(output)
}
  
