Guide to run BRMS on CBU cluster
================

Preamble
========

This is a guide how to use the package *brms* on the high performance computer cluster at the MRC CBU.

You need:

Step 0: Set up your access to the CBU cluster and R.
====================================================

1.  Log on to a node with PuTTY.
    -   Note that not all nodes work.
    -   In my case I use login node 25.
2.  Start VNC server.
3.  Connect to the virtual machine via TurboVNC connection. Use the correct login node (e.g. login25:6).

You also need to install the packages (*brms* & *rslurm*) that you need to run everything. To do this type R in the terminal. And do something like this

``` r
.libPaths("/home/aq01/R/x86_64-redhat-linux-gnu-library/3.5")
```

Then you should be able to install *brms* and *rslurm* so you can use it on the cluster. Just type in these lines into your open R window:

``` r
install.packages('brms')
install.packages('rslurm')
```

Step 1: Create script that compiles a slurm job.
================================================

The whole script can be found here.

Step 2: Running that script.
============================

1.  Open terminal change directory (cd) to the correct folder.

2.  Run the script that creates shell script that can be submited to the cluster.

``` terminal
cd /home/aq01/Projects/bayesianMLM/IIG_talk/
Rscript logRT_model_rslurm.R
```

Step 3: Submit the slurm job to the cluster.
============================================

When the script above is finished running, you see it has created two new folders in your directory. In order to submit the job for the full model, you need to cd into that folder and run the shell script (submit.sh).

``` terminal
cd /home/aq01/Projects/bayesianMLM/IIG_talk/_rslurm_fullModel/
sbatch submit.sh
```

For the null model you need to do the same:

``` terminal
cd /home/aq01/Projects/bayesianMLM/IIG_talk/_rslurm_nullModel/
sbatch submit.sh
```

Step 4: Get the results.
========================

In this last step, we get the date and bring it into a format that can used by us for analysis.

Step 5: The end
===============

With the last step you can now just load your image you just saved and analyse the results:

``` r
# Load results
load("U:/Projects/bayesianMLM/CBU_clusterGuide/combinedModels.RData")


# Inspect results
summary(fullModel)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: s_logRT ~ congruency + (congruency | subNum) + (1 | stimulus) 
    ##    Data: stroopData (Number of observations: 4968) 
    ## Samples: 8 chains, each with iter = 4000; warmup = 2000; thin = 1;
    ##          total post-warmup samples = 16000
    ## 
    ## Group-Level Effects: 
    ## ~stimulus (Number of levels: 8) 
    ##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sd(Intercept)     0.05      0.03     0.00     0.12       5417 1.00
    ## 
    ## ~subNum (Number of levels: 26) 
    ##                                  Estimate Est.Error l-95% CI u-95% CI
    ## sd(Intercept)                        0.54      0.08     0.41     0.73
    ## sd(congruencyneutral)                0.25      0.05     0.17     0.36
    ## cor(Intercept,congruencyneutral)    -0.65      0.14    -0.86    -0.33
    ##                                  Eff.Sample Rhat
    ## sd(Intercept)                          3041 1.00
    ## sd(congruencyneutral)                  5376 1.00
    ## cor(Intercept,congruencyneutral)       7340 1.00
    ## 
    ## Population-Level Effects: 
    ##                   Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## Intercept             0.11      0.11    -0.10     0.34       2070 1.00
    ## congruencyneutral    -0.23      0.07    -0.36    -0.09       4192 1.00
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
    ## sigma     0.89      0.01     0.87     0.91      23056 1.00
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).
