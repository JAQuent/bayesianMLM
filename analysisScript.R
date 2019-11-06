# This scripts performs the analysis for understanding Bayesian mixed linear models

# Loading libraries
library(brms)
library(assortedRFunctions)
library(ggplot2)
library(lmerTest)

# General variables
cores2use <- 4
#exgaussian(link = "identity", link_sigma = "log", link_beta = "log")
# https://mc-stan.org/workshops/stancon2019_hierarchical/
# https://mc-stan.org/workshops/stancon2018_hierarchical/AHM1.pdf

# Loading data
load("exampleData/stroopData.RData")

# Scale RT for model purpose
stroopData$sRT <- scale(stroopData$RT)

# Frequentist model
model_lmer <- lmer(sRT ~ congruency + (congruency | subNum) + (1 | stimulus), 
                   data = stroopData)

## Priors based on reccommendations of Andrew Gelman
priors <- c(prior(normal(0, 1), class = "Intercept"),
            prior(normal(0, 1), class = "b")) 

# Running full model
model_full <- brm(sRT ~ congruency + (congruency | subNum) + (1 | stimulus), 
                  data = stroopData,
                  prior = priors,
                  save_all_pars = TRUE,
                  sample_prior = TRUE,
                  cores = cores2use)

## Weakly informative prior, very weak based on reccommendations of Andrew Gelman
priors <- c(prior(normal(0, 1), class = "Intercept"),
            prior(normal(0, 1), class = "b")) 

# Running full model with ex-Gaussian link
model_full_exGauss <- brm(sRT ~ congruency + (congruency | subNum) + (1 | stimulus), 
                          family = exgaussian(link = "identity", link_sigma = "log", link_beta = "log"),
                          data = stroopData,
                          prior = priors,
                          save_all_pars = TRUE,
                          sample_prior = TRUE,
                          cores = cores2use)

# Running full model distrubtional ex-Gaussian
model_full_exGauss <- brm(bf(sRT ~ congruency + (congruency | subNum) + (1 | stimulus),
                             sigma ~ congruency + (congruency | subNum) + (1 | stimulus), 
                             beta ~ congruency + (congruency | subNum) + (1 | stimulus)),
                          family = exgaussian(link = "identity", link_sigma = "log", link_beta = "log"),
                          data = stroopData,
                          prior = priors,
                          save_all_pars = TRUE,
                          sample_prior = TRUE,
                          cores = cores2use)