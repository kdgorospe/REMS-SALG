# Do confirmatory factor analysis of pre vs. post using the results from EFA on pre-data as the confirmed model
# kdgorospe@gmail.com
# See: HIRSCHFELD and VON BRACHEL 2014 in "Resources" folder

rm(list=ls())

library(lavaan)
library(semPlot)
library(semTools)
library(tidyverse)

# CHOOSE:
# Restarting point
load("2021-02-13_all-data-prior-to-CFA_all-vars.RData") 

######################################################################################################
# RETAINING ALL VARIABLES (including cross-loading variables)
model_6 <- '
hypothesis =~ attitudes_confidentresearch + skills_communicate_pooled + skills_developH0 + skills_evalH0 + skills_testH0 + understanding_sciprocess
marine_sci =~ attitudes_confidentunderstanding + understanding_ecology + understanding_fertilization + understanding_oceanacid_pooled + understanding_sound_pooled
identity =~ attitudes_career + attitudes_confidentunderstanding + attitudes_discussing + attitudes_enthusiastic 
practice =~ attitudes_confidentresearch + integration_applyingknowledge + integration_connectingknowledge
peers =~ attitudes_workwithothers + skills_withothers
society =~ understanding_relatetolife + understanding_society_pooled
 '

# Tried only using the largest loading per variable (i.e., remove double loadings for attitudes_confidentresearch and attitudes_confidentunderstanding)
# Note: doesn't change results significantly
# model_6 <- '
# hypothesis =~ skills_communicate_pooled + skills_developH0 + skills_evalH0 + skills_testH0 + understanding_sciprocess
# marine_sci =~ understanding_ecology + understanding_fertilization + understanding_oceanacid_pooled + understanding_sound_pooled
# identity =~ attitudes_career + attitudes_confidentunderstanding + attitudes_discussing + attitudes_enthusiastic
# practice =~ attitudes_confidentresearch + integration_applyingknowledge + integration_connectingknowledge
# peers =~ attitudes_workwithothers + skills_withothers
# society =~ understanding_relatetolife + understanding_society_pooled
#  '

# Note: lavaan automatically adds covariances between latent variables automatically

######################################################################################################
# INSPECT BASELINE MODEL

# Combine the pre and post dataframes to compare them in a CFA measurement invariance framework
tidy_dat_all <- rbind.data.frame(tidy_dat_pre_final, tidy_dat_post_final)

fit <- cfa(model_6, data = tidy_dat_all, std.lv = TRUE) 
# std.lv = standardize latent variables
# doing so constrains latent variables to have a mean of 0 and a variance of 1 (allows the latent covariances to be interpreted as CORRELATIONS)

# option to do multiple imputation of missing data gives similar results, so just keep it simple (no imputation)
# fit <- cfa(model_6, data = tidy_dat_all, std.lv = TRUE, missing = "fiml")

summary(fit, standardized = TRUE, fit.measures = TRUE)
# Interpreting model outputs, see: http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
# CFI > 0.9 is an OK fit
# TLI (more conservative than CFI because it penalizes complex models) > 0.9 is an OK fit
# RMSEA however is > 0.05 (not a good fit)

# Visualize pathways
semPaths(fit, "std")

# Parameter estiamtes
parameterEstimates(fit, standardized=TRUE)
# also note CORRELATIONS (not covariance, because they are standardized) between latent variables
# for example: parameterEstimates(fit, standardized=TRUE) %>% filter((lhs == "identity" | rhs == "identity") & op == "~~") %>% arrange(desc(est))
# Identity most highly correlated with practicing science, working with peers, and hypothesis-testing; as abilities in one increases, identity also increases (but not causation)

# Question should this initial test be done on the full dataset or just the pre data?
# Doesn't change results significantly so just stick with the full dataset
# fit_pre <- cfa(model_6, data = tidy_dat_pre_final, std.lv = TRUE, missing = "fiml") 
# summary(fit_pre, standardized = TRUE, fit.measures = TRUE)
# semPaths(fit_pre, "std")
# parameterEstimates(fit_pre, standardized=TRUE)


######################################################################################################
# MEASUREMENT INVARIANCE FRAMEWORK FOR CONTINUOUS VARIABLES:
# First test the baseline configuration model, then test for weak, strong, strict measurement invariance
config <- lavaan::cfa(model_6, data = tidy_dat_all, group = "test")
weak <- lavaan::cfa(model_6, data = tidy_dat_all, group = "test", group.equal = "loadings")
strong <- lavaan::cfa(model_6, data = tidy_dat_all, group = "test", group.equal = c("loadings", "intercepts"))
strict <- lavaan::cfa(model_6, data = tidy_dat_all, group = "test", group.equal = c("loadings", "intercepts", "residuals"))
anova(config, weak, strong, strict)

measurementInvariance(model = model_6, data = tidy_dat_all, group = "test")

######################################################################################################
# NOTE: UNABLE TO SPECIFY THESE ARE ORDINAL VARIABLES
# AVERAGING RESPONSES FOR THE POOLED QUESTIONS CREATES TOO MANY CATEGORIES 

# SEE EXAMPLE WARNING MESSAGE:
orindal_config <- lavaan::cfa(model_6, ordered = TRUE, data = tidy_dat_all, group = "test")