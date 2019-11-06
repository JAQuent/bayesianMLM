load("~/GitHub/bayesianMLM/exampleData/stroopData.RData")

library(brms)
cores2use <- 4

# https://cran.r-project.org/web/packages/brms/vignettes/brms_distreg.html

model_exguss <- brm(RT ~ congruency + (congruency | subNum),
                    family = exgaussian(link = "identity", link_sigma = "log", link_beta = "log"),
                    data = stroopData,
                    save_all_pars = TRUE,
                    sample_prior = TRUE,
                    cores = cores2use)

# https://discourse.mc-stan.org/t/interpretation-brms-results/6374

ranef(model_exguss)

pp_check(model_exguss)

# Oke thank for you answers! So, if I am correct with my example you estimate αi∼N(γ,σ) , where the posterior distribution of γ is summarized in “Population-level effects” and the posterior distribution for σ is summarized in “Group-Level effects”? Here the normal distribution is just an example