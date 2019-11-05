# Checking whether having one model is the same as having to sequential models

library(brms)
library(assortedRFunctions)
library(ggplot2)

cores2use <- 3

# seeds
set.seed(1939)
bmrsSeeds <- sample(100000, 5)

beta0 <- 3.3
beta1 <- 4

# Dataset1 
n1 <- 20
x  <- runif(n1, -3, 3)
y  <- beta0 + beta1*x + rnorm(n1, 0, 1)
data1 <- data.frame(x = x, y = y)
data1$scaledx <- scale(data1$x)
data1$scaledy <- scale(data1$y)

# Dataset2
n2 <- 24
x  <- runif(n2, -3, 3)
y  <- beta0 + beta1*x + rnorm(n2, 0, 1)
data2 <- data.frame(x = x, y = y)
data2$scaledx <- scale(data2$x)
data2$scaledy <- scale(data2$y)

# Concatenated data frames
dataBoth <- rbind(data1, data2)

# startingPrior
startingPriors <- c(prior(normal(0, 1), class = "b")) 

# One model for both with fixed effects
model_bothFixedEffects <- brm(scaledy ~ scaledx, 
                              data = dataBoth,
                              prior = startingPriors,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              cores = cores2use,
                              seed = bmrsSeeds[1])

# Sequential 
# Data1
model_data1 <- brm(scaledy ~ scaledx 
                   data = data1,
                   prior = startingPriors,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   cores = cores2use,
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
                   seed = bmrsSeeds[3])

# Compare the results
# Estimates
fixef(model_bothFixedEffects)
fixef(model_data2)

# Posterior density
model_bothFixedEffects_postDen <- posterior_samples(model_bothFixedEffects)
model_data2_postDen            <- posterior_samples(model_data2)
df_postDensity                 <- data.frame(method = c(rep('All data at once', 4000),
                                                       rep('Sequential', 4000)),
                                             values = c(model_bothFixedEffects_postDen[,'b_scaledx'],
                                                       model_data2_postDen[,'b_scaledx']))

ggplot(df_postDensity, aes(x = values, fill = method)) +
  geom_density(alpha = 0.5) 

# Bayes factor
hypo1 <- hypothesis(model_bothFixedEffects, 'scaledx = 0')
hypo2 <- hypothesis(model_data2, 'scaledx = 0')

plot(hypo1)
plot(hypo2)


# Alternative way of calculating Bayes factor
# Calculating null models
# Sequential 
# Data1
model_data1_null <- brm(scaledy ~ 1, 
                   data = data1,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   cores = cores2use,
                   seed = bmrsSeeds[4])

model_data1_null_fixef <- fixef(model_data1_null)

# Extract posterior and make prior
priors_data2_null <- c(set_prior(priorString_normal(model_data1_null_fixef[1, 1], model_data1_null_fixef[1, 2]),
                            class = "Intercept"))

model_data2_null <- brm(scaledy ~ 1, 
                   data = data2,
                   prior = priors_data2_null,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   cores = cores2use,
                   seed = bmrsSeeds[5])

bayes_factor(model_data2, model_data2_null)

save.image('1modelVSsequential.RData')