load("exampleData/stroopData.RData")

library(brms)

# General variables
chains    <- 8
cores2use <- 8

# Log transform to make closer to normal
stroopData$logRT   <- log(stroopData$RT)

# Then scale to mean = 0 and sd = 1
stroopData$s_logRT <- scale(stroopData$logRT)

# Priors
priors <- c(prior(normal(0, 1), class = "Intercept"), 
            prior(normal(0, 1), class = "b"))


# Running model
logRT_model <- brm(s_logRT ~ congruency + (congruency | subNum) + (1 | stimulus),
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   data = stroopData,
                   cores = cores2use,
                   chains = chains, 
                   iter = 3000,
                   prior = priors, 
                   control = list(adapt_delta = 0.9))

priors <- c(prior(normal(0, 1), class = "Intercept"))

logRT_model_null <- brm(s_logRT ~ 1 + (congruency | subNum) + (1 | stimulus),
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   data = stroopData,
                   cores = cores2use,
                   chains = chains, 
                   iter = 3000,
                   prior = priors, 
                   control = list(adapt_delta = 0.9))

bf <- bayes_factor(logRT_model, logRT_model_null)

save.image('logRT_model.RData')