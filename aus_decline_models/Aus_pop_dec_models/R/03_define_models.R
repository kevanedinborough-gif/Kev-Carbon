theta <- binned$theta
N <- length(theta)

single_slope_code <- nimble::nimbleCode({
  r ~ dnorm(0, sd = 0.02)
  for (i in 1:N) {
    theta[i] ~ dExponentialGrowth(a = startBP, b = endBP, r = r)
  }
})

change_point_code <- nimble::nimbleCode({
  r_early ~ dnorm(0, sd = 0.02)
  r_late  ~ dnorm(0, sd = 0.02)
  mu ~ dunif(changeLow, changeHigh)
  for (i in 1:N) {
    theta[i] ~ dDoubleExponentialGrowth(
      a = startBP, b = endBP, r1 = r_early, r2 = r_late, mu = mu
    )
  }
})

# Smooth logistic transition. The parameter m is the inflection date, while r
# controls the direction and steepness of the transition. Unlike the abrupt
# change-point model, this model permits a gradual bend and stabilisation.
logistic_transition_code <- nimble::nimbleCode({
  r_logistic ~ dnorm(0, sd = 0.02)
  m ~ dunif(changeLow, changeHigh)
  for (i in 1:N) {
    theta[i] ~ dLogisticGrowth2(
      a = startBP, b = endBP, m = m, r = r_logistic
    )
  }
})
