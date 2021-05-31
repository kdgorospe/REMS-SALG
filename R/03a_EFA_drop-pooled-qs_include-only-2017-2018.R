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
  # KEEP QUESTIONS THAT WERE SPLIT IN LATER YEARS, BUT DROP THEIR EARLIER POOLED VERSIONS (REMINDER: taking the mean of split questions to standardize them with pooled questions creates problems with ordinal variable analysis)
  select(!contains("pooled")) %>%
  # SINCE QUESTIONS WERE SPLIT ONLY IN 2017 and 2018, LIMIT ANALYSIS TO ONLY THESE YEARS
  filter(year %in% c(2017, 2018)) %>%
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
    drive_update(file = as_id("1M6sqMkUQTJbzRBZP5WTOgF_K53dXvMhK"), media = cor_csv)
    drive_update(file = as_id("14heeC2Ulp8hQZotEW2sz0irVfCtGsCju"), media = hist_pdf)
    drive_update(file = as_id("1iKY1hK5ubkiU7LIbQWmN-KIgn8YBy0Lq"), media = p_csv)
    drive_update(file = as_id("11Cgva5A4Bsn0b7JzRTjxFxfGohMKqotb"), media = cor_pdf_plain)
    drive_update(file = as_id("1warSl8QDSs_dGjX6Nrlnpm0629FXzSMP"), media = cor_pdf_x_insig)
  }
  if (i=="post"){ 
    drive_update(file = as_id("1u1SvUCGOU25VnnvVm38aB34wIoCFhlc6"), media = cor_csv)
    drive_update(file = as_id("1uYm4HgkUc4U74jrelNlywxwYsZtHnlHO"), media = hist_pdf)
    drive_update(file = as_id("1vuu5KqOY6isIbcPyrFLzSu_pD0dTN4tn"), media = p_csv)
    drive_update(file = as_id("1hSzu5-b4dNyB1X5qGda5D1cz6v3dZpnF"), media = cor_pdf_plain)
    drive_update(file = as_id("1f8Q1BeqlHWqlEed2_KdZWG19yKAbChcM"), media = cor_pdf_x_insig)
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
    drive_update(file = as_id("1OQXDcKMWTYWb6n3G-oOIZbWArLhh_7rg"), media = describe_csv)
  }
  if (i == "post"){
    drive_update(file = as_id("1v44BJ4AL3kdAFdAOVRSl6SkMjQiefLAy"), media = describe_csv)
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
drive_update(file = as_id("1TCPFl3xyvM9KKyfrZP_kxU7aYiwpJDX-"), media = bartlett_name)  
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
drive_update(file = as_id("1MpA40vNtFoyd1PfG3tYPpndlFiwc1WO5"), media = KMO_name)  
file.remove(KMO_name)

###################################################################################################################
# SCREE PLOTS 

# NOTES on SCREE PLOTS: (from Howard 2016)
# plots each eigenvalue on a graph and determine when decreases in successive eigenvalues start to asymptote
# only include the number of factors up until the asymptote; these represent common variance better than the factors after the asymptote

i <- "pre"
tidy_dat <- get(paste("tidy_dat_", i, "_final", sep="")) %>%
  drop_na()
# NOTE: FOR THIS MUCH SMALLER DATASET ONLY, if NAs aren't dropped, fa.parallel function gives warning: Matrix was not positive definite
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
drive_update(file = as_id("1q6ezu9mXLQI-DzWe5jDqLQ5wD2io4Wya"), media = scree_name)
file.remove(scree_name)

###################################################################################################################
# FACTOR ANALYSIS
# Uses nfactors 2 through 4
# REMINDER: 4 factors have eigenvalues greater than 1;
# 2 through 4 factors justified based on parallel analysis (+/- 1 from asymptote); 

# set nfactors to results from Scree plot
# rotate = "promax" what Goodwin 2016 used:
# A promax (non-orthogonal or oblique) rotation was employed since the theory naturally permits inter-correlation between the constructs (i.e., the factors were not expected to be orthogonal)


i <- "pre"

tidy_dat <- get(paste("tidy_dat_", i, "_final", sep="")) %>%
  drop_na()
# NOTE: FOR THIS MUCH SMALLER DATASET ONLY, if NAs aren't dropped, fa.parallel function gives warning: Matrix was not positive definite

cor_dat <- tidy_dat[,4:dim(tidy_dat)[2]]
cor_matrix <- cor(cor_dat, use="pairwise.complete.obs")

for (f in c(2, 3, 4)){ # Set numbers of factors here; IMPORTANT: if decide to change number of factors here, remember conditionals for drive_update below
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
  if (i == "pre" & f == 2){
    drive_update(file = as_id("1L-eigJzH8RmyIIcPbWi5WUAWXDhf9B-j"), media = efa_file) 
    drive_update(file = as_id("1A35UR2CK4AuE82BflBkhpRk0XlhtKgvZ"), media = efa_loadings_csv) 
  }
  if (i == "pre" & f == 3){
    drive_update(file = as_id("1z6hatwmFnDpihZGk2KBy1H6dWYi6trVq"), media = efa_file) 
    drive_update(file = as_id("1TCa56M8Fbyh21NJoStMIGpjbaR37uBWA"), media = efa_loadings_csv) 
  }
  if (i == "pre" & f == 4){
    drive_update(file = as_id("1lNwJfvRJx9rjkaFG5iVlUf8d-TAhxIe6"), media = efa_file) 
    drive_update(file = as_id("1_WL8SmiX33WNxAfqdaGD6OIMpEq6iLFF"), media = efa_loadings_csv) 
  }
  
  file.remove(efa_file)
  file.remove(efa_loadings_csv)
} # END loop through different number of factors


# CHANGE DESCRIPTION AS NECESSARY:
insert_description <- "pooled-qs-removed_likert-standardized_NAs-dropped_2017-2018-data-only"
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

