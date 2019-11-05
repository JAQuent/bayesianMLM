# This scripts performs the analysis for understanding Bayesian mixed linear models

# Loading libraries
library(brms)
library(assortedRFunctions)
library(ggplot2)
library(lmerTest)
library(BayesFactor)

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


# Priors narrow wide
model4_priors <- c(prior(normal(0, 1), class = "b")) 

model4 <- brm(scaledRT ~ congruency*colorType*filling + (1 | subNum), 
              data = stroopData,
              prior = model4_priors,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              cores = cores2use)

model4_postDen <- posterior_samples(model4)



model5_priors <- c(prior(normal(0, 0.1), class = "b")) 

model5 <- brm(scaledRT ~ congruency*colorType*filling + (1 | subNum), 
              data = stroopData,
              prior = model5_priors,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              cores = cores2use)

model5_postDen <- posterior_samples(model5)

model6_priors <- c(prior(normal(0.5, 1), class = "b")) 

model6 <- brm(scaledRT ~ congruency*colorType*filling + (1 | subNum), 
              data = stroopData,
              prior = model6_priors,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              cores = cores2use)

model6_postDen <- posterior_samples(model6)

# Illustration
df_postDensity_congruency <- data.frame(prior = c(rep('Default', 4000),
                                                  rep('normal(0, 20)', 4000),
                                                  rep('normal(0, 5)', 4000),
                                                  rep('normal(0, 1)', 4000),
                                                  rep('normal(0, 0.1)', 4000),
                                                  rep('normal(0.5, 1)', 4000)),
                                        values = c(model1_postDen[,'b_congruencyneutral'],
                                                   model2_postDen[,'b_congruencyneutral'],
                                                   model3_postDen[,'b_congruencyneutral'],
                                                   model4_postDen[,'b_congruencyneutral'],
                                                   model5_postDen[,'b_congruencyneutral'],
                                                   model6_postDen[,'b_congruencyneutral']))

df_postDensity_congruencyFilling <- data.frame(prior = c(rep('Default', 4000),
                                                  rep('normal(0, 20)', 4000),
                                                  rep('normal(0, 5)', 4000),
                                                  rep('normal(0, 1)', 4000),
                                                  rep('normal(0, 0.1)', 4000),
                                                  rep('normal(0.5, 1)', 4000)),
                                        values = c(model1_postDen[,'b_congruencyneutral:fillingfilledSpaces'],
                                                   model2_postDen[,'b_congruencyneutral:fillingfilledSpaces'],
                                                   model3_postDen[,'b_congruencyneutral:fillingfilledSpaces'],
                                                   model4_postDen[,'b_congruencyneutral:fillingfilledSpaces'],
                                                   model5_postDen[,'b_congruencyneutral:fillingfilledSpaces'],
                                                   model6_postDen[,'b_congruencyneutral:fillingfilledSpaces']))


ggplot(df_postDensity_congruencyFilling, aes(x = values, fill = prior)) +
  geom_density(alpha = 0.5) 


df_postDensity_colorTypeFilling <- data.frame(prior = c(rep('Default', 4000),
                                                         rep('normal(0, 20)', 4000),
                                                         rep('normal(0, 5)', 4000),
                                                         rep('normal(0, 1)', 4000),
                                                         rep('normal(0, 0.1)', 4000),
                                                         rep('normal(0.5, 1)', 4000)),
                                               values = c(model1_postDen[,'b_colorTypeSLC:fillingfilledSpaces'],
                                                          model2_postDen[,'b_colorTypeSLC:fillingfilledSpaces'],
                                                          model3_postDen[,'b_colorTypeSLC:fillingfilledSpaces'],
                                                          model4_postDen[,'b_colorTypeSLC:fillingfilledSpaces'],
                                                          model5_postDen[,'b_colorTypeSLC:fillingfilledSpaces'],
                                                          model6_postDen[,'b_colorTypeSLC:fillingfilledSpaces']))


ggplot(df_postDensity_colorTypeFilling, aes(x = values, fill = prior)) +
  geom_density(alpha = 0.5) 


hypothesis(model6, 'colorTypeSLC:fillingfilledSpaces > 0') 
plot(hypothesis(model6, 'colorTypeSLC:fillingfilledSpaces = 0'))



model7 <- brm(scaledRT ~ congruency*colorType*filling, 
                 data = stroopData,
                 prior = model3_priors,
                 save_all_pars = TRUE,
                 sample_prior = TRUE,
                 cores = cores2use)

model8 <- brm(scaledRT ~ congruency*colorType*filling + (congruency*colorType*filling | subNum), 
              data = stroopData,
              prior = model3_priors,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              cores = cores2use)


hypothesis(model8, 'congruencyneutral:colorTypeSLC = 0')
hypothesis(model8, 'congruencyneutral:fillingfilledSpaces > 0')

fixef(model8)

model9 <- brm(scaledRT ~ congruency + 
                colorType +
                filling + 
                congruency*filling +
                colorType*filling  + 
                (congruency + 
                   colorType +
                   filling  +
                   congruency*filling +
                   colorType*filling  | subNum), 
              data = stroopData,
              prior = model3_priors,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              cores = cores2use)


bf1 <- bayes_factor(model8, model9)


model10 <- brm(scaledRT ~ congruency + 
                colorType +
                filling + 
                 congruency*colorType +
                congruency*filling +
                colorType*filling, 
              data = stroopData,
              prior = model3_priors,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              cores = cores2use)

model11 <- brm(scaledRT ~ congruency + 
                 colorType +
                 filling +
                 congruency*filling +
                 colorType*filling, 
               data = stroopData,
               prior = model3_priors,
               save_all_pars = TRUE,
               sample_prior = TRUE,
               cores = cores2use)

hypothesis(model8, 'congruencyneutral:colorTypeSLC = 0')
bf2 <- bayes_factor(model10, model11)
bf2.2 = anovaBF(scaledRT ~ congruency + 
                  colorType +
                  filling + 
                  congruency*colorType +
                  congruency*filling +
                  colorType*filling, data = stroopData, whichModels = 'withmain')

bf2.2[17]/bf2.2[16]


model8_ranef <- ranef(model8)



intercepts <- fixef(model8)[1,1] + as.data.frame(model8_ranef$subNum[,,'Intercept'])[,1]
slopes     <- fixef(model8)[2,1] + as.data.frame(model8_ranef$subNum[,,'congruencyneutral'])[,1]

df_random_congruency <- data.frame(subNum = c(row.names(random_congruency), row.names(random_congruency)),
                                   x      = c(rep('incongruent', length(row.names(random_congruency))), rep('neutral', length(row.names(random_congruency)))),
                                   y      = c(intercepts, intercepts + slopes))


ggplot(df_random_congruency, aes(x = x, y = y, colour = subNum, group = subNum)) + geom_line()


save.image('currentImage.RData')
# Delate models that are not needed

# summary(combinedData_case1_model)
# hypothesis(combinedData_case1_model, "IobjLocTargetRatingMUobjLocTargetRating = 0")
# combinedData_case1_model_postDen    <- posterior_samples(combinedData_case1_model)
# plot(hypothesis(combinedData_case1_model, "IobjLocTargetRatingMUobjLocTargetRating = 0"))
# plot(marginal_effects(combinedData_case1_model), points = TRUE, rug = TRUE)
# ranef(combinedData_case1_model)