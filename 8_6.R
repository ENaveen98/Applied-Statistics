# Load the dependent libraries
library(Bolstad)

pi_values = seq(from = 0, to = 1, by=0.0001)

pi_prior = rep(0,length(pi_values))
pi_prior[pi_values<=0.2] = pi_values[pi_values<=0.2]
pi_prior[pi_values>0.2 & pi_values<=0.3] = 0.2
pi_prior[pi_values>0.3 & pi_values<=0.5] = 0.5 - pi_values[pi_values>0.3 & pi_values<=0.5]
pi_prior[pi_values>0.5] = 0
output_1 = binogcp(x = 7, n = 20, density="user", pi = pi_values, pi.prior = pi_prior)

pi_prior = rep(1,length(pi_values))
output_2 = binogcp(x = 7, n = 20, density="user", pi = pi_values, pi.prior = pi_prior)

# Plotting the two posterior distributions in same graph.
plot(pi_values, output_1$posterior, type='l', xlab = 'Pi', ylab = 'Posterior density', col="red",
     main = 'Posterior Distributions for Q8.4 & Q8.5')
points(pi_values, output_2$posterior, type='l', xlab = 'Pi', ylab = 'Posterior density', col="blue")
legend(x = 0.65, y = 5.35, legend=c("Q8.4 Posterior", "Q8.5 Posterior"), col = c("red", "blue"), lty=1:1, cex=0.7)

