# Script to run analysis of logRT model from IIG talk
# Version 1.0
# Date:  14/05/2020
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 13846

# Libaries
library(brms)
library(rslurm)

# /*
# ----------------------------- Parameters for analysis ---------------------------
# */
# BRMS parameters
chains       <- 1
nRuns        <- 8
iterPerChain <- 4000
seeds1       <- sample(99999, nRuns)
seeds2       <- sample(99999, nRuns)

# Job parameters
n_nodes       <- 1
cpus_per_node <- nRuns # 1 cpu per run

# /* 
# ----------------------------- Preparing data and priors ---------------------------
# */
# Loading data
load("/home/aq01/Projects/bayesianMLM/CBU_clusterGuide/exampleData/stroopData.RData")

# Pre processing
# Log transform to make closer to normal
stroopData$logRT   <- log(stroopData$RT)

# Then scale to mean = 0 and sd = 1
stroopData$s_logRT <- scale(stroopData$logRT)

# Priors
priors_full <- c(prior(normal(0, 1), class = "Intercept"), 
                 prior(normal(0, 1), class = "b"))

priors_null <- c(prior(normal(0, 1), class = "Intercept"))

pars_full  <- data.frame(i = 1:nRuns, seed = seeds1)
pars_null  <- data.frame(i = 1:nRuns, seed = seeds2)


# /* 
# ----------------------------- Full model ---------------------------
# */
# Run 1 for model compilation so it can be used via update
starterModel_full  <- brm(s_logRT ~ congruency + (congruency | subNum) + (1 | stimulus),
                          data = stroopData,
                          prior = priors_full,
                          cores = 1,
                          chains = 1,
                          iter = iterPerChain,
                          save_all_pars = TRUE,
                          sample_prior = TRUE,
                          save_dso = TRUE, 
                          seed = pars_full[1, 'seed']) 

# rslurm function
helper1<- function(i, seed){
  if(i == 1){
    return(list(i = i,
                seed = seed,
                model = starterModel_full))
  } else {
    model_full <- update(starterModel_full,
                         newdata = stroopData,
                         recompile = FALSE,
                         cores = 1,
                         chains = 1,
                         iter = iterPerChain,
                         save_all_pars = TRUE,
                         sample_prior = TRUE,
                         save_dso = TRUE,
                         seed = seed)
    return(list(i = i,
                seed = seed,
                model = model_full))
  }
  
}


# Create rslurm job 
sjob1 <- slurm_apply(helper1, pars_full, jobname = 'fullModel',
                     add_objects = c("starterModel_full", 'stroopData', 'priors_full', 'iterPerChain'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)


# /* 
# ----------------------------- Null model ---------------------------
# */
# Run 1 for model compilation so it can be used via update
starterModel_null  <- brm(s_logRT ~ 1 + (congruency | subNum) + (1 | stimulus),
                          data = stroopData,
                          prior = priors_null,
                          cores = 1,
                          chains = 1,
                          iter = iterPerChain,
                          save_all_pars = TRUE,
                          sample_prior = TRUE,
                          save_dso = TRUE, 
                          seed = pars_full[1, 'seed']) 

# rslurm function
helper2<- function(i, seed){
  if(i == 1){
    return(list(i = i,
                seed = seed,
                model = starterModel_null))
  } else {
    model_null <- update(starterModel_null,
                         newdata = stroopData,
                         recompile = FALSE,
                         cores = 1,
                         chains = 1,
                         iter = iterPerChain,
                         save_all_pars = TRUE,
                         sample_prior = TRUE,
                         save_dso = TRUE,
                         seed = seed)
    return(list(i = i,
                seed = seed,
                model = model_null))
  }
  
}


# Create rslurm job 
sjob1 <- slurm_apply(helper2, pars_null, jobname = 'nullModel',
                     add_objects = c("starterModel_null", 'stroopData', 'priors_null', 'iterPerChain'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)
