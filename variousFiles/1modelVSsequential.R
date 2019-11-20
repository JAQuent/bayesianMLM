# Script to show that it makes no difference if data is analysed in one model or sequentially
# Version 1.0
# Date: 18/11/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Librarises
library(brms)
library(assortedRFunctions)

cores2use <- 4
iter      <- 15000 

# seeds
set.seed(23939)
bmrsSeeds <- sample(100000, 5)

# /* 
# ----------------------------- Generating data ---------------------------
# */
# Dataset1 
beta0 <- 3.3
beta1 <- 1
n1 <- 20
x  <- runif(n1, -3, 3)
y  <- beta0 + beta1*x + rnorm(n1, 0, 3)
data1 <- data.frame(x = x, y = y)
data1$scaledx <- scale(data1$x)
data1$scaledy <- scale(data1$y)

# Dataset2

beta0 <- 2.3
beta1 <- 1.5
n2 <- 24
x  <- runif(n2, -3, 3)
y  <- beta0 + beta1*x + rnorm(n2, 0, 3)
data2 <- data.frame(x = x, y = y)
data2$scaledx <- scale(data2$x)
data2$scaledy <- scale(data2$y)

# Concatenated data frames
dataBoth <- rbind(data1, data2)


# /* 
# ----------------------------- Combinded analysis in 1 model ---------------------------
# */
# startingPrior
startingPriors <- c(prior(normal(0, 1), class = "b")) 

# One model for both with fixed effects
model_bothFixedEffects <- brm(scaledy ~ scaledx, 
                              data = dataBoth,
                              prior = startingPriors,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              cores = cores2use,
                              iter = iter,
                              seed = bmrsSeeds[1])

# /* 
# ----------------------------- Sequential analys ---------------------------
# */
# Data1
model_data1 <- brm(scaledy ~ scaledx,
                   data = data1,
                   prior = startingPriors,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   cores = cores2use,
                   iter = iter,
                   seed = bmrsSeeds[2])

model_data1_fixef <- fixef(model_data1)



# Extract posterior and make prior
priors_data2 <- c(set_prior(priorString_normal(model_data1_fixef[1, 1], model_data1_fixef[1, 2]),
                                   class = "Intercept"),
                  set_prior(priorString_normal(model_data1_fixef[2, 1], model_data1_fixef[2, 2]),
                                   class = "b",
                                   coef = "scaledx"))

model_data2 <- brm(scaledy ~ scaledx, 
                   data = data2,
                   prior = priors_data2,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   cores = cores2use,
                   iter = iter,
                   seed = bmrsSeeds[3])

# /* 
# ----------------------------- Using bayes_factor() ---------------------------
# */
# Alternative way of calculating Bayes factor
# Calculating null models
# Sequential 
# Data1
model_data1_null <- brm(scaledy ~ 1, 
                   data = data1,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   cores = cores2use,
                   iter = iter,
                   seed = bmrsSeeds[4])

model_data1_null_fixef <- fixef(model_data1_null)
bf1 <- bayes_factor(model_data1, model_data1_null)

# Extract posterior and make prior
priors_data2_null <- c(set_prior(priorString_normal(model_data1_null_fixef[1, 1], model_data1_null_fixef[1, 2]),
                            class = "Intercept"))

model_data2_null <- brm(scaledy ~ 1, 
                   data = data2,
                   prior = priors_data2_null,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   cores = cores2use,
                   iter = iter,
                   seed = bmrsSeeds[5])

bf2 <- bayes_factor(model_data2, model_data2_null)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image('1modelVSsequential.RData')