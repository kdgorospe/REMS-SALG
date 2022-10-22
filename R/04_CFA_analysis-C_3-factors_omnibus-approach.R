# "Omnibus" approach based on Vandenberg and Lance 2000

rm(list=ls())

library(lavaan)
library(tidyverse)
library(googledrive)

# Restarting point
load("2022-10-08_all-data-prior-to-CFA_pooled-qs-removed_likert-standardized_NAs-dropped.RData") 

######################################################################################################
# QUESTION FOR LISA: CONFIRM THAT FIRST STEP IS TO IDENTIFY AN EXPLORATORY MODEL USING EFA and confirm on the POST data
# STEP 1: Since EFA was done on just the PRE data, do a CFA on just the POST data to make sure the EFA was not overfitting the data

# Define model:
# Only allow one item to load onto each latent variable (assumption with CFA is that each items loads equally)
# Use EFA (data) and expected model (theory) along with model fit measures and anova to arrive at final model

# EFA Results for FOUR factors: https://docs.google.com/spreadsheets/d/1PV5B4tzEFQUa1_8KEfHZAFZoYlGPVUG-rbk7Wqgf6b0/edit#gid=1486559735
model <- '
process =~ skills_developH0 + skills_evalH0 + skills_testH0
interest =~ attitudes_career + attitudes_discussing + attitudes_enthusiastic 
integration =~ integration_applyingknowledge + integration_connectingknowledge
content =~ understanding_ecology + understanding_fertilization
 '

# NOTE: See FOUR factor results with more permissive cutoff = 0.4: https://docs.google.com/spreadsheets/d/1y5w1OhuB2VbnYgcQ5W9d5iI8qeC2xQKIQVf-LetNMmc/edit#gid=1486559735 
# Only gain one more variable and not in any of the factors that could use more measured variables
# Lowering cutoff further to 0.3 makes it more difficult to interpret factors (e.g., work effectively with others starts to load with scientific process factor)
# Since lowering the cutoff doesn't gain us much, keep stricter 0.7 cutoff so that we can say we chose a model with strong loadings

# Fit indices (results) are the same for parameterization = "delta" (same setting as measurement invariance) or "theta"
# Use MLR estimator for continuous-ish variables (e.g., often used when there are 5 or more response options)
# MLR = maximum likelihood estimation with robust (Huber-White) standard errors and a scaled test statistic that is (asymptotically) equal to the Yuan-Bentler test statistic.
# https://lavaan.ugent.be/tutorial/est.html
fit <- cfa(model, data = tidy_dat_post_final, std.lv = TRUE, estimator = "MLR") 


# fit_3 <- cfa(model_3, data = tidy_dat_post_final, std.lv = TRUE, ordered = TRUE, parameterization = "delta")
# fit_3_theta <- cfa(model_3, data = tidy_dat_post_final, std.lv = TRUE, ordered = FALSE, parameterization = "theta")
# std.lv = standardize latent variables
# doing so constrains latent variables to have a mean of 0 and a variance of 1 (allows the latent covariances to be interpreted as CORRELATIONS)
summary(fit, fit.measures = TRUE)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
post_fit <- matrix(NA, nrow = 1, ncol = 6)
colnames(post_fit) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(post_fit) <- c("model_delta-parameter")
post_fit[1,] <- round(data.matrix(fitmeasures(fit, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

post_fit

overfit_test_name <- "meas-invar_CFA-on-post-to-check-for-overfitting.txt"
sink(overfit_test_name)
print(post_fit)
sink()

#drive_upload(overfit_test_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("18Zdh3-sD81h7CUU_Ip8lbVR4WGH6z1Zc"), media = overfit_test_name)  
file.remove(overfit_test_name)
# CFI > 0.9 is an OK fit
# TLI (more conservative than CFI because it penalizes complex models) > 0.9 is an OK fit
# RMSEA however is < 0.08 is marginal
# From Leandre R ,et al. "Evaluating the use of exploratory factor analysis in psychological research." Psychological methods 4, no. 3 (1999): 272:
# "It has been suggested that RMSEA values less than 0.05 are good, values between 0.05 and 0.08 are acceptable, 
# values between 0.08 and 0.1 are marginal, and values greater than 0.1 are poor [8]. 

######################################################################################################
# STEP 2: OMNIBUS APPROACH TO MEASUREMENT INVARIANCE ANALYSIS based on Vandenberg and Lance 2000
# Estimate the null model. The null model does not include any differences across groups

# Combine the pre and post dataframes to compare them in a CFA measurement invariance framework
tidy_dat_all <- rbind.data.frame(tidy_dat_pre_final, tidy_dat_post_final)

no_groups <-cfa(model, tidy_dat_all, estimator = "MLR", meanstructure = T)
summary(no_groups, standardized = T, fit.measures = T)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
no_groups_fit <- matrix(NA, nrow = 1, ncol = 6)
colnames(no_groups_fit) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(no_groups_fit) <- c("model_delta-parameter")
no_groups_fit[1,] <- round(data.matrix(fitmeasures(no_groups, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

no_groups_fit

residuals <- residuals(no_groups, type = "standardized")
# write.csv(residuals,"finalmod_residuals.csv") # save?


# QUESTION FOR LISA: Minimum value can just be 0 right? (only 119 rows for my model)
# Calculate modification indices
mod_ind <- modificationindices(no_groups, sort.=TRUE, minimum.value=3)
mod_ind <- modificationindices(no_groups, sort.=TRUE) # default minimum.value = 0
#write.csv(mod_ind,"finalmod_ModInd.csv")

######################################################################################################
# STEP 3 - Multiple Group comparison using test groups (pre vs post)

tidy_dat_all$test <- as.factor(tidy_dat_all$test)

test_mod <- cfa(model, tidy_dat_all, estimator = "MLR", group = "test")
summary(test_mod, fit.measures = T)

# Extract just the fit indices:
# Create matrix for storing results (6 fit indices across three different models)
test_mod_fit <- matrix(NA, nrow = 1, ncol = 6)
colnames(test_mod_fit) <- c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
rownames(test_mod_fit) <- c("model_delta-parameter")
test_mod_fit[1,] <- round(data.matrix(fitmeasures(test_mod, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)

test_mod_fit

# Compare model fits:
no_groups_fit
test_mod_fit

# Your CFA is a pretty good fit for "real life" data and 
# yes your no groups model is a better fit which suggests that you have measurement invariance. 

# Following Putnick and Bornstein 2016: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5145197/
# Reproduce Table 3 for these results, modified for omnibus approach

# Get Chi Sq and (DF)
chisq_base <- round(fitmeasures(no_groups, fit.measures = c("chisq.scaled")), digits = 3)
df_base <- fitmeasures(no_groups, fit.measures = c("df.scaled"))
chisq_df_base <- paste(chisq_base,
                       " (", 
                       df_base,
                       ")",
                       sep = "", collapse = "")
chisq_test <- round(fitmeasures(test_mod, fit.measures = c("chisq.scaled")), digits = 3)
df_test <- fitmeasures(test_mod, fit.measures = c("df.scaled"))
chisq_df_test <- paste(chisq_test,
                       " (", 
                       df_test,
                       ")",
                       sep = "", collapse = "")
# Get P-value
p_base <- round(fitmeasures(no_groups, fit.measures = c("pvalue.scaled")), digits = 3)
p_test <- round(fitmeasures(test_mod, fit.measures = c("pvalue.scaled")), digits = 3)

# Get CFI
cfi_base <- round(fitmeasures(no_groups, fit.measures = c("cfi.scaled")), digits = 3)
cfi_test <- round(fitmeasures(test_mod, fit.measures = c("cfi.scaled")), digits = 3)

# GEt RMSEA
rmsea_base <- round(fitmeasures(no_groups, fit.measures = c("rmsea.scaled")), digits = 3)
rmsea_ci_base <- paste(round(rmsea_base, digits = 3),
                       " (", 
                       round(fitmeasures(no_groups, fit.measures = c("rmsea.ci.lower.scaled")), digits = 3),
                       "-",
                       round(fitmeasures(no_groups, fit.measures = c("rmsea.ci.upper.scaled")), digits = 3),
                       ")",
                       sep = "", collapse = "")

rmsea_test <- round(fitmeasures(test_mod, fit.measures = c("rmsea.scaled")), digits = 3)
rmsea_ci_test <- paste(round(rmsea_test, digits = 3),
                       " (", 
                       round(fitmeasures(test_mod, fit.measures = c("rmsea.ci.lower.scaled")), digits = 3),
                       "-",
                       round(fitmeasures(test_mod, fit.measures = c("rmsea.ci.upper.scaled")), digits = 3),
                       ")",
                       sep = "", collapse = "")

# Get SRMR
srmr_base <- round(fitmeasures(no_groups, fit.measures = c("srmr")), digits = 3)
srmr_test <- round(fitmeasures(test_mod, fit.measures = c("srmr")), digits = 3)

# Get deltas
# FIX IT: Revisit abs(); Look up model rejection cutoffs for deltas (test - base vs base - test)
delta_chisq <- round(abs(chisq_base - chisq_test), digits = 3)
delta_df <- abs(df_base - df_test)
delta_chisq_df <- paste(delta_chisq, " (", delta_df, ")", sep = "", collapse = "")
delta_cfi <- abs(cfi_base - cfi_test)
delta_rmsea <- abs(rmsea_base - rmsea_test)
delta_srmr <- abs(srmr_base - srmr_test)

# Assemble into table:
mi_table <- matrix(NA, nrow = 2, ncol = 9)
colnames(mi_table) <- c("chisq_df", "pvalue_scaled", "cfi_scaled", "rmsea_ci", "srmr", "delta_chisq_df", "delta_cfi", "delta_rmsea", "delta_srmr")
rownames(mi_table) <- c("no_groups", "with_groups")
mi_table[1,] <- c(chisq_df_base, round(c(p_base, cfi_base), digits = 3), rmsea_ci_base, srmr_base, NaN, NaN, NaN, NaN)
mi_table[2,] <- c(chisq_df_test, p_test, cfi_test, rmsea_ci_test, srmr_test, delta_chisq_df, delta_cfi, delta_rmsea, delta_srmr)

mi_file <- "mi_omnibus_approach.csv"
write.csv(mi_table, quote = FALSE, row.names = TRUE, file = mi_file)
# drive_upload(mi_file, path = as_dribble("REMS_SALG/Results")) # for initial upload
drive_update(file = as_id("1zlARe49uQg3o_73bagfusaX4vkdojMLQ"), media = mi_file)
file.remove(mi_file)
######################################################################################################
# STEP 4 - Compare the null and pre/post models

anova(no_groups, test_mod)
# Output / report result?

######################################################################################################
# STEP 5 - Calculate means in SALG responses for the three latent factors pre vs post and do T-test for significance

# FIX IT - OUTPUT FINAL SAMPLE NUMBERS:
tidy_dat_all %>%
  #na.omit() %>%
  group_by(test) %>%
  count()

# CALCULATE GROUP MEANS ACROSS STUDENT RESPONSES
group_means <- tidy_dat_all %>%
  na.omit() %>%
  group_by(test) %>%
  summarise(attitudes_career = mean(attitudes_career, na.rm = TRUE), 
            attitudes_confidentresearch = mean(attitudes_confidentresearch, na.rm = TRUE),
            attitudes_confidentunderstanding = mean(attitudes_confidentunderstanding, na.rm = TRUE),
            attitudes_discussing = mean(attitudes_discussing, na.rm = TRUE),
            attitudes_enthusiastic = mean(attitudes_enthusiastic, na.rm = TRUE),
            attitudes_workwithothers = mean(attitudes_workwithothers, na.rm = TRUE),
            integration_applyingknowledge = mean(integration_applyingknowledge, na.rm = TRUE),
            integration_connectingknowledge = mean(integration_connectingknowledge, na.rm = TRUE),
            skills_developH0 = mean(skills_developH0, na.rm = TRUE),
            skills_evalH0 = mean(skills_evalH0, na.rm = TRUE),
            skills_testH0 = mean(skills_testH0, na.rm = TRUE),
            skills_withothers = mean(skills_withothers, na.rm = TRUE),
            understanding_ecology = mean(understanding_ecology, na.rm = TRUE),
            understanding_fertilization = mean(understanding_fertilization, na.rm = TRUE),
            understanding_relatetolife = mean(understanding_relatetolife, na.rm = TRUE),
            understanding_sciprocess = mean(understanding_sciprocess, na.rm = TRUE))

# FIX IT - why does this method of calculating means NOT MATCH the t-test calculation of means down below
# CALCULATE MEANS FOR LATENT FACTORS:
group_means %>%
  rowwise() %>%
  mutate(process = mean(skills_developH0, skills_evalH0, skills_testH0),
         interest = mean(attitudes_career, attitudes_discussing, attitudes_enthusiastic),
         integration = mean(integration_applyingknowledge, integration_connectingknowledge),
         content = mean(understanding_ecology, understanding_fertilization)) %>%
  select(test, process, interest, integration, content)

# NOTE: this collapses all variables for a shared factor into a single vector; one for pre and one for post data and performs a t-test on the two vectors
# i.e. - individual variables are lost in that the t-test is not taking a mean of the means for each of the three variables in a factor, rather a single mean for all the data points
# T-Test for PROCESS
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(skills_developH0, skills_evalH0, skills_testH0),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(skills_developH0, skills_evalH0, skills_testH0))

# T-test for INTEREST
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(attitudes_career, attitudes_discussing, attitudes_enthusiastic),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(attitudes_career, attitudes_discussing, attitudes_enthusiastic))

# T-test for INTEGRATION
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(integration_applyingknowledge, integration_connectingknowledge),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(integration_applyingknowledge, integration_connectingknowledge))

# T-test for CONTENT
t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(understanding_ecology, understanding_fertilization),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(understanding_ecology, understanding_fertilization))

# All significant differences between pre and post
# Students were increased their:
# perceived understanding of the scientific process (PROCESS)
# interest in marine science (INTEREST)
# their perceived ability to connect what they learn in classes to other areas
# their perceived knowledge of scientific content (CONTENT)