# Do preliminary analysis (correlation and scree plots) and exploratory factor analysis
# kdgorospe@gmail.com

### NOTE: Focus on EFA results on the pre-data, plan is to use this as the "confirmed" model
### Then use CFA on the pre and post in a factorial invariance framework to test for changes between the two time points
### Screeplot on the pre-data (after filtering out variables that fail normality test) suggests there are three latent factors

library(psych)
library(corrplot)
library(GPArotation) # needed for promax rotation in fa function



### REFERENCE: https://quantdev.ssri.psu.edu/tutorials/intro-basic-exploratory-factor-analysis

# FINAL CLEANING: do this up top, so that all correlation plots, etc are for the FINAL dataset
# Arrange data into tidy format (each row is a single student)
tidy_dat_all <- coded_and_standardized_dat %>%
  select(Number, answer, year, test, concept) %>% # Rename pooled_answer to just "answer"
  pivot_wider(values_from = answer, names_from = concept, id_cols = c(Number, year, test)) %>% # use values_fn = list(val = length) to identify coding duplicates %>%
  # REMOVE QUESTIONS ABOUT COLLEGE MAJOR (yes no)
  select(!starts_with("major")) %>%
  # REMOVE ALL EVALUATION QUESTIONS (only asked in the post test, and not part of our pre vs post framework)
  select(!starts_with("evaluation")) %>% # note: for some reason starts_with("evaluation") == FALSE does not work (not tidy?)
  # REMOVE VARIABLES WITH LIMITED PAIRWISE COMPARISONS (these labs were run only a few times)
  select(-c(understanding_ethology, understanding_aquaculture, understanding_coralclimate, understanding_coralskel, understanding_diversity)) %>%
  # DUPLICATE POOLED QUESTIONS - i.e., USE POOLED QUESTIONS TO FILL IN NA's FOR SPLIT QUESTIONS (REMINDER: taking the mean of split questions to standardize them with pooled questions creates problems with ordinal variable analysis)
  mutate(skills_communicate_split_oral = if_else(is.na(skills_communicate_split_oral), true = skills_communicate_pooled, false = skills_communicate_split_oral),
         skills_communicate_split_written = if_else(is.na(skills_communicate_split_written), true = skills_communicate_pooled, false = skills_communicate_split_written),
         understanding_oceanacid_split_chemistry = if_else(is.na(understanding_oceanacid_split_chemistry), true = understanding_oceanacid_pooled, false = understanding_oceanacid_split_chemistry),
         understanding_oceanacid_split_organisms = if_else(is.na(understanding_oceanacid_split_organisms), true = understanding_oceanacid_pooled, false = understanding_oceanacid_split_organisms),
         understanding_society_split_realissues = if_else(is.na(understanding_society_split_realissues), true = understanding_society_pooled, false = understanding_society_split_realissues),
         understanding_society_split_society = if_else(is.na(understanding_society_split_society), true = understanding_society_pooled, false = understanding_society_split_society),
         understanding_sound_split_general = if_else(is.na(understanding_sound_split_general), true = understanding_sound_pooled, false = understanding_sound_split_general),
         understanding_sound_split_organisms = if_else(is.na(understanding_sound_split_organisms), true = understanding_sound_pooled, false = understanding_sound_split_organisms)) %>%
  # NOW THAT THEY'VE REFORMATED AS "SPLIT" QUESTIONS, DROP POOLED QUESTIONS
  select(!contains("pooled")) %>%
  arrange(year, test, Number) %>%
  ungroup() 

# %>% drop_na()
# REMINDER: not always necessary to drop NAs, although sometimes this can help with the function fa.parallel if it produces error message about "Matrix not positive definite"

# Create subsets of pre and post datasets - filter both subsets to the same questions, so they are comparable
tidy_dat_pre <- tidy_dat_all %>%
  filter(test=="pre") 

tidy_dat_post <- tidy_dat_all %>%
  filter(test=="post")

###################################################################################################################
# Calculate and plot correlations
# NOTE ON CORRELATION RESULTS: Scaling vs not scaling the variables does not change results of correlation matrix

time_point <- c("pre", "post")
for (i in time_point){
  tidy_dat <- get(paste("tidy_dat_", i, sep=""))
  cor_dat <- tidy_dat[,4:dim(tidy_dat)[2]]
  cor_matrix <- cor(cor_dat, use="pairwise.complete.obs")
  cor_csv <- paste("cormatrix_", i, ".csv", sep="")
  write.csv(cor_matrix, cor_csv, row.names = TRUE)
  #drive_upload(cor_csv, path = as_dribble("REMS_SALG/Results")) # for initial upload
  
  # Plot histograms of responses:
  p <- ggplot(pivot_longer(cor_dat, cols = names(cor_dat)[1]:names(cor_dat)[ncol(cor_dat)]), aes(x = value)) +
    geom_histogram() +
    facet_wrap(.~name, ncol = 4)
  hist_pdf <- paste("histogram_", i, "_responses.pdf", sep="")
  pdf(hist_pdf)
  print(p)
  dev.off()
  #drive_upload(hist_pdf, path = as_dribble("REMS_SALG/Results")) # for initial upload
  
  # ANY REMAINING NA's at this point are "blanks" - student failed to or decided not to answer a particular question
  
  # NOTE: option use = "pairwise.complete.obs" computes correlations for each pair of columns using vectors formed by omitting rows with missing values on a pairwise basis
  # Caveat: this means each column vector varies depending on its pairing - and for small datasets can lead to weird results
  # See: http://bwlewis.github.io/covar/missing.html
  
  # Calculate significance tests
  p_95 <- cor.mtest(cor_dat, conf.level = 0.95)
  p_csv <- paste("cor_pmatrix_", i, ".csv", sep = "")
  write.csv(p_95$p, p_csv, row.names = TRUE)
  #drive_upload(p_csv, path = as_dribble("REMS_SALG/Results")) # for initial upload
  
  cor_pdf_plain <- paste("corrplot_", i, "_plain.pdf", sep="")
  pdf(cor_pdf_plain)
  corrplot(cor_matrix, tl.col="black", tl.cex=0.75) # insig = "label_sig" means label the significant p-values; otherwise option is to label the insignificant values 
  dev.off()
  #drive_upload(cor_pdf_plain, path = as_dribble("REMS_SALG/Results")) # for initial upload
  
  cor_pdf_x_insig <- paste("corrplot_", i, "_x_insig.pdf", sep="")
  pdf(cor_pdf_x_insig)
  corrplot(cor_matrix, p.mat = p_95$p, sig.level = 0.05, tl.col = "black", tl.cex = 0.75, insig = "blank")
  dev.off()
  # ALTERNATIVELY, place an "X" wherever values are insiginficant
  #corrplot(cor_matrix, p.mat = p_95$p, sig.level = 0.05, pch.cex = 0.8, tl.col="black", tl.cex=0.75) 
  #drive_upload(cor_pdf_x_insig, path = as_dribble("REMS_SALG/Results")) # for initial upload
  
  if (i=="pre"){ 
    drive_update(file = as_id("1m_aNu0sTDJB3oLG7W6Cxs3g94pnm-k5M"), media = cor_csv)
    drive_update(file = as_id("19LnaIlki7Ul_FIdRVmskhD4n0sf0LSOJ"), media = hist_pdf)
    drive_update(file = as_id("1E2eatXRHoYD8aQslVk8th5urOJ5huVHB"), media = p_csv)
    drive_update(file = as_id("1UFO24AtP5uAwwSXi-qgvUvSvzvC5Ctp2"), media = cor_pdf_plain)
    drive_update(file = as_id("1avdlaRZwo3YJSpHhQ5Zxm9sQLZJ-45jL"), media = cor_pdf_x_insig)
  }
  if (i=="post"){ 
    drive_update(file = as_id("1iMPlbfCSwbJCGUMf1ukBJCmxTgZkZgbz"), media = cor_csv)
    drive_update(file = as_id("1B4V67Jk1rX70oKFRnR--6IdUcVywZn77"), media = hist_pdf)
    drive_update(file = as_id("1Ny5K7AfMik7boTaqRKeDCO-rh-RnFP2s"), media = p_csv)
    drive_update(file = as_id("1PSf5PESVFXgHqSImh0kmCkoZwRyxKMoU"), media = cor_pdf_plain)
    drive_update(file = as_id("1zkl8ilhjMicNxj-DPnuFC2WrmrINKIT_"), media = cor_pdf_x_insig)
  }
  file.remove(cor_csv)
  file.remove(cor_pdf_plain)
  file.remove(cor_pdf_x_insig)
  file.remove(p_csv)
  file.remove(hist_pdf)
}

###################################################################################################################
# TEST FOR NORMALITY
## Note: scale() function only shifts data to mean = 0 and standard deviation = 1; i.e., does nothing to affect how normally distributed it is; keep raw data so we see results in terms of the Likert scale (e.g., mean response for each variable)
## From Godwin 2013 for EFA: The skew and kurtosis were evaluated for each item to ensure that the assumptions of multivariate normality were not severely violated 
## Do this separately for pre and post
## Use describe() to examine normality of each variable ()
## caution: describe() shares namespace with other packages (e.g., Hmisc); use psych::describe to specify

time_point <- c("pre", "post")
for (i in time_point){
  tidy_dat <- get(paste("tidy_dat_", i, sep=""))
  dat_describe <- psych::describe(tidy_dat[,4:dim(tidy_dat)[2]])
  
  # According to Goodwin 2016, these variables should be removed because they violate assumption of normality:
  skew_fail <- row.names(dat_describe[abs(dat_describe$skew) >= 2,])
  if (length(skew_fail) > 0){
    skew_list <- paste(row.names(dat_describe[abs(dat_describe$skew) >= 2,]), collapse = ", ")
    message <- paste(i, "dataset variables that fail skewness test:\n", skew_list, "\n")
    cat(message) # unlike print(), cat will print \n as newline in console
  }
  
  kurtosis_fail <- row.names(dat_describe[abs(dat_describe$kurtosis) >= 7,])
  if (length(kurtosis_fail) > 0){
    kurtosis_list <- paste(row.names(dat_describe[abs(dat_describe$kurtosis) >= 7,]), collapse = ", ")
    message <- paste(i, "dataset variables that fail kurtosis test:\n", kurtosis_list, "\n")
    cat(message)
  }
  
  describe_csv <- paste("dat_describe_", i, ".csv", sep = "")
  write.csv(dat_describe, describe_csv, row.names = TRUE) # write out row.names because these list the different "questions"
  #drive_upload(describe_csv, path = as_dribble("REMS_SALG/Results")) # for initial upload
  if (i == "pre"){
    drive_update(file = as_id("1UeCBaasbM1DKJ-3aZVylg6g9lvYobvHi"), media = describe_csv)
  }
  if (i == "post"){
    drive_update(file = as_id("10x5yXi-EmcsI0glRJ50duwtvM66CDe_m"), media = describe_csv)
  }
  file.remove(describe_csv)
}


###################################################################################################################
# Finalize variable list for further analysis:
tidy_dat_pre_final <- tidy_dat_pre # In case any further filtering needed
tidy_dat_post_final <- tidy_dat_post # In case any further filtering needed

###################################################################################################################
# NOTE FOR REMAINDER OF CODE BELOW - only producing outputs for "PRE" data since this is all that matters for EFA
# POST data re-enters in the measurement invariance analysis framework

# Additional EFA data inspection tests:

# 1 - Bartlett's test of sphericity - tests whether observed correlation matrix is an identity matrix;
# Tests whether or not the correlation matrix exhibits any relationships or if has a complete lack of relationships (i.e., the identity matrix)
# Just perform this on the pre data since that's what's being used in the EFA

pre_cor_matrix <- cor(tidy_dat_pre_final[,-c(1:3)], use="pairwise.complete.obs")

# RESULTS: Test shows results are significantly different from the identity matrix
cortest.bartlett(R = pre_cor_matrix, n = nrow(tidy_dat_pre_final), diag = TRUE) 

# SAVE RESULTS:
bartlett_name <- "pre_cor_matrix_Bartlett's_test.txt"
sink(bartlett_name)
print(cortest.bartlett(R = pre_cor_matrix, n = nrow(tidy_dat_pre_final), diag = TRUE))
sink()

#drive_upload(bartlett_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("1lT-ETitw-cHFn5vP7xfG6dbgDHxCe_4b"), media = bartlett_name)  
file.remove(bartlett_name)

# 2 - Kaiser, Meyer, Olkin measure of Sampling Adequacy

KMO(r = pre_cor_matrix)
# RESULS: Test shows all variables are above cutoff of 0.6
# This indicates that common variance (and thus latent factors) are present in the data

# SAVE RESULTS:
KMO_name <- "pre_cor_matrix_KMO_test.txt"
sink(KMO_name)
print(KMO(r = pre_cor_matrix))
sink()

#drive_upload(KMO_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
drive_update(file = as_id("1st17Og94fDKZS8MThtziHgy_6g2uCFnE"), media = KMO_name)  
file.remove(KMO_name)

###################################################################################################################
# SCREE PLOTS 

# NOTES on SCREE PLOTS: (from Howard 2016)
# plots each eigenvalue on a graph and determine when decreases in successive eigenvalues start to asymptote
# only include the number of factors up until the asymptote; these represent common variance better than the factors after the asymptote

i <- "pre"
tidy_dat <- get(paste("tidy_dat_", i, "_final", sep="")) 
dat_scaled <- scale(tidy_dat[,4:dim(tidy_dat)[2]], center=TRUE, scale=TRUE)

# SCREE PLOT TO DETERMINE THE NUMBER OF FACTORS IN THE DATA
# See: https://quantdev.ssri.psu.edu/tutorials/intro-basic-exploratory-factor-analysis
cor_matrix <- cor(dat_scaled, use = "pairwise.complete.obs")

scree_name <- paste("screeplot_", i, "_final.pdf", sep = "")
pdf(file = scree_name)
fa.parallel(x = cor_matrix, fm = "pa", fa = "fa", n.obs = nrow(dat_scaled)) # "ml" is the maximum likelihood method for "well-behaved" data
dev.off()
#drive_upload(scree_name, path = as_dribble("REMS_SALG/Results")) # for initial upload

#Use drive_update to update specific file based on ID number
drive_update(file = as_id("11EHmshd1Q9cXRMh04wRrEYGKna-zn0Hg"), media = scree_name)
file.remove(scree_name)

###################################################################################################################
# FACTOR ANALYSIS
# Uses nfactors 3 through 7
# REMINDER: 3 factors have eigenvalues greater than 1;
# 5 through 7 factors justified based on parallel analysis (+/- 1 from asymptote); 
# 4 factors just to be continuous between 3 and 7

# set nfactors to results from Scree plot
# rotate = "promax" what Goodwin 2016 used:
# A promax (non-orthogonal or oblique) rotation was employed since the theory naturally permits inter-correlation between the constructs (i.e., the factors were not expected to be orthogonal)


i <- "pre"

tidy_dat <- get(paste("tidy_dat_", i, "_final", sep=""))

cor_dat <- tidy_dat[,4:dim(tidy_dat)[2]]
cor_matrix <- cor(cor_dat, use="pairwise.complete.obs")

for (f in c(3, 4, 5, 6, 7)){ # Set numbers of factors here; IMPORTANT: if decide to change number of factors here, remember conditionals for drive_update below
  EFA_results <- fa(r = cor_matrix, nfactors = f, rotate = "promax", fm = "ml") 
  
  # Write model outputs to textfile
  efa_file <- paste("EFA_", i, "_final_", f, "factors.txt", sep = "")
  sink(efa_file)
  print(EFA_results)
  sink()
  
  # Simplify model outputs: Write just factor loadings to textfile:
  #efa_loadings_file <- paste("EFA_", i, "_final_", f, "factors_loadingsONLY.txt", sep = "")
  #sink(efa_loadings_file)
  #print(EFA_results$loadings)
  #sink()
  
  # Simplify model outputs: Write factor loadings to csv file
  efa_loadings_csv <- paste("EFA_", i, "_final_", f, "factors_loadingsONLY.csv", sep = "")
  EFA_loadings_only <- EFA_results$loadings[1:nrow(EFA_results$loadings), 1:ncol(EFA_results$loadings)]
  write.csv(EFA_loadings_only, file = efa_loadings_csv)
  
  # drive_upload(efa_file, path = as_dribble("REMS_SALG/Results")) # for initial upload
  # drive_upload(efa_loadings_csv, path = as_dribble("REMS_SALG/Results")) # for initial upload

  # Use drive_update to update specific file based on ID number
  if (i == "pre" & f == 3){
    drive_update(file = as_id("1xW1ukxjoShStWh9gUfjF24KpiQ5pNpBh"), media = efa_file) 
    drive_update(file = as_id("1niH7U2V08jPL4iyS9IPHcFjMkvNd1nGb"), media = efa_loadings_csv) 
  }
  if (i == "pre" & f == 4){
    drive_update(file = as_id("1_pwRqizTj9s-VQvSczx2RcM2yC2S2ALi"), media = efa_file) 
    drive_update(file = as_id("1bkz1zhpW6HvDEfjL9DelkbNNuYeMUtBW"), media = efa_loadings_csv) 
  }
  if (i == "pre" & f == 5){
    drive_update(file = as_id("1eZdkJaX0iIQaK-Sb5sAEbYdiMUmRgIV3"), media = efa_file)
    drive_update(file = as_id("1J5XO88QuKk_yIubgunuHkuNfIRytnAJA"), media = efa_loadings_csv)
  }
  if (i == "pre" & f == 6){
    drive_update(file = as_id("1HoVHh5M_-ZXxFy_au-BEVYgzqhQFbPFN"), media = efa_file)
    drive_update(file = as_id("1KxCgqLjx517-0vcRXXQqt-J7q0CAOwEH"), media = efa_loadings_csv)
  }
  if (i == "pre" & f == 7){
    drive_update(file = as_id("1CAPE0obrrtAiqn5JN7td9d51vu4WpPMG"), media = efa_file)
    drive_update(file = as_id("1JTSpuIHd1Lrp2y_nv_ueqOU6DYGXMqBq"), media = efa_loadings_csv)
  }
  
  file.remove(efa_file)
  file.remove(efa_loadings_csv)
} # END loop through different number of factors


# CHANGE DESCRIPTION AS NECESSARY:
insert_description <- "duplicate-pooled-qs_likert-standardized_NAs-dropped"
rdata_file <- paste(Sys.Date(), "_all-data-prior-to-CFA_", insert_description, ".RData", sep = "")
save.image(file = rdata_file)
# Upload to Google Drive manually (drive_upload for .RData file causes RStudio to crash)

# To output how much variance is accounted for by the factors:
# Or just inspect the table of "Cumulative Var"
#EFA_results$Vaccounted
#EFA_results$Vaccounted[3, f] # i.e., last column of the "cumulative variance" row

# Function "fa" will take either raw data or correlation matrix, butto get individual factor scores, need to input raw data 
#EFA_rawdat_results <- fa(r = scaled_pre[,4:dim(scaled_pre)[2]], nfactors = 5, rotate = "promax", fm = "ml") 
#EFA_rawdat_results$scores

