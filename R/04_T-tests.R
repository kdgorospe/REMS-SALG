# "Omnibus" approach based on Vandenberg and Lance 2000

rm(list=ls())

library(tidyverse)
library(googledrive)
library(broom)

######################################################################################################
# NEW VERSION: No CFA or ME/I analysis

######################################################################################################
# Calculate means in SALG responses for the latent factors pre vs post and do T-test for significance

# OUTPUT FINAL SAMPLE NUMBERS:
tidy_dat_all %>%
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


# NOTE: this collapses all variables for a shared factor into a single vector; one for pre and one for post data and performs a t-test on the two vectors
# i.e. - individual variables are lost in that the t-test is not taking a mean of the means for each of the three variables in a factor, rather a single mean for all the data points

# SCIENTIFIC PROCESS
# Get 95% confidence intervals around the mean for pre data
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(skills_developH0, skills_evalH0, skills_testH0), conf.level = 0.95))
# Get 95% confidence intervals around the mean for post data
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(skills_developH0, skills_evalH0, skills_testH0), conf.level = 0.95))
# T-test
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(skills_developH0, skills_evalH0, skills_testH0),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(skills_developH0, skills_evalH0, skills_testH0), conf.level = 0.95))

####################################################################################################
##### BASED ON MANUSCRIPT REVIEW, REMOVING attitudes_career from the INTEREST/IDENTITY latent factor
####################################################################################################
# INTEREST/IDENTITY
# Get 95% confidence intervals around the mean for pre data
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(attitudes_discussing, attitudes_enthusiastic), conf.level = 0.95))
# Get 95% confidence intervals around the mean for post data
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(attitudes_discussing, attitudes_enthusiastic), conf.level = 0.95))
# T-test
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(attitudes_discussing, attitudes_enthusiastic),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(attitudes_discussing, attitudes_enthusiastic), conf.level = 0.95))


# INTEGRATION
# Get 95% confidence intervals around the mean for pre data
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(integration_applyingknowledge, integration_connectingknowledge), conf.level = 0.95))
# Get 95% confidence intervals around the mean for post data
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(integration_applyingknowledge, integration_connectingknowledge), conf.level = 0.95))
# T-test
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(integration_applyingknowledge, integration_connectingknowledge),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(integration_applyingknowledge, integration_connectingknowledge), conf.level = 0.95))


# CONTENT
# Get 95% confidence intervals around the mean for pre data
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(understanding_ecology, understanding_fertilization), conf.level = 0.95))
# Get 95% confidence intervals around the mean for post data
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(understanding_ecology, understanding_fertilization), conf.level = 0.95))
# T-test
broom::tidy(t.test(x = tidy_dat_all %>% filter(test == "pre") %>% select(-c("Number", "year", "test")) %>% select(understanding_ecology, understanding_fertilization),
       y = tidy_dat_all %>% filter(test == "post") %>% select(-c("Number", "year", "test")) %>% select(understanding_ecology, understanding_fertilization), conf.level = 0.95))



# All significant differences between pre and post
# Students were increased their:
# perceived understanding of the scientific process (PROCESS)
# interest in marine science (INTEREST)
# their perceived ability to connect what they learn in classes to other areas
# their perceived knowledge of scientific content (CONTENT)