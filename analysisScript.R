# This scripts performs the analysis for understanding Bayesian mixed linear models

# Loading libraries
library(brms)
library(assortedRFunctions)
library(ggplot2)
library(lmerTest)

# General variables
cores2use <- 3

# Loading data
load("exampleData/stroopData.RData")

# Scale RT
stroopData$scaledRT <- scale(stroopData$RT)

# Frequentist modley
model_lmer <- lmer(scaledRT ~ congruency*colorType*filling + (1 | subNum), 
                   data = stroopData)

## verify that the priors indeed found their way into Stan's model code
priors <- c(prior(student_t(30, 0, 4), class = "b")) 


# Random intercepts
# With build in priors
model1 <- brm(scaledRT ~ congruency*colorType*filling + (1 | subNum), 
              data = stroopData,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              cores = cores2use)

model1_postDen <- posterior_samples(model1)

# Priors very wide
model2_priors <- c(prior(normal(0, 20), class = "b")) 

model2 <- brm(scaledRT ~ congruency*colorType*filling + (1 | subNum), 
              data = stroopData,
              prior = model2_priors,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              cores = cores2use)

model2_postDen <- posterior_samples(model2)

hypothesis(model2, "colorTypeSLC:fillingfilledSpaces   = 0")
plot(hypothesis(model2, "congruencyneutral = 0"))

# Priors medium wide
model3_priors <- c(prior(normal(0, 5), class = "b")) 

model3 <- brm(scaledRT ~ congruency*colorType*filling + (1 | subNum), 
              data = stroopData,
              prior = model3_priors,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              cores = cores2use)

model3_postDen <- posterior_samples(model3)

hypothesis(model3, "colorTypeSLC:fillingfilledSpaces = 0")
plot(hypothesis(model3, "congruencyneutral = 0"))

# Illustration
df_postDensity_congruency <- data.frame(prior = c(rep('Default', 4000),
                                                  rep('normal(0, 20)', 4000),
                                                  rep('normal(0, 5)', 4000)),
                                        values = c(model1_postDen[,'b_congruencyneutral'],
                                                   model2_postDen[,'b_congruencyneutral'],
                                                   model3_postDen[,'b_congruencyneutral']))


ggplot(df_postDensity_congruency, aes(x = values, fill = prior)) +
  geom_density(alpha = 0.5) 


# Save current image
save.image('currentImage.RData')

# summary(combinedData_case1_model)
# hypothesis(combinedData_case1_model, "IobjLocTargetRatingMUobjLocTargetRating = 0")
# combinedData_case1_model_postDen    <- posterior_samples(combinedData_case1_model)
# plot(hypothesis(combinedData_case1_model, "IobjLocTargetRatingMUobjLocTargetRating = 0"))
# plot(marginal_effects(combinedData_case1_model), points = TRUE, rug = TRUE)
# ranef(combinedData_case1_model)