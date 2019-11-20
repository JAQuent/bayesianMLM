# Script to explain central limit theorem
# Version 1.0
# Date: 18/11/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Librarues
library(ggplot2)
library(cowplot)
library(brms)

# /* 
# ----------------------------- Create data ---------------------------
# */
# Simulate data
n   <- 1000000
# Create soemthing that could be RT data
fRT <- rexgaussian(n, 600, 50, 100) 

# Transform vector to matrix and use rowMeans. The effect is that we
# now take the mean across 2 and as comparison 20 values
fRT_matrix1 <- matrix(fRT, ncol = 2) 
means1       <- rowMeans(fRT_matrix1)
fRT_matrix2 <- matrix(fRT, ncol = 20)
means2      <- rowMeans(fRT_matrix2)

# /* 
# ----------------------------- Plot ---------------------------
# */
plot1 <- ggplot(data.frame(fRT), aes(x = fRT))  +
  geom_histogram(binwidth = 10) + 
  labs(x = 'Simulated RT', y = 'Count', title = 'Raw ex-Gaussian distribution')

plot2 <- ggplot(data.frame(means1), aes(x = means1))  +
  geom_histogram(binwidth = 10) + 
  labs(x = 'Average RT', y = 'Count', title = 'Average of 2 points')

plot3 <- ggplot(data.frame(means2), aes(x = means2))  +
  geom_histogram(binwidth = 10) + 
  labs(x = 'Average RT', y = 'Count', title = 'Averaged of 20 points')

plot4 <- ggplot(data.frame(fRT), aes(x = log(fRT)))  +
  geom_histogram(binwidth = 0.01) + 
  labs(x = 'log(RT)', y = 'Count', title = 'Log transformed data')

plot_grid(plot1, plot2, plot3, plot4, ncol = 2)

# The plot above shows a) a typical distribution that could be RT data, then what happens if this data
# is averaged across two data points and the same over 20 data points. When averaged across 20 data points,
# data much more resembles normal distribtion. Similar is true for log transformed data. 