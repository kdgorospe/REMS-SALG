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
  # drop all questions that were pooled then split in later years (REMINDER: taking the mean of split questions to standardize them with pooled questions creates problems with ordinal variable analysis)
  select(-c(skills_communicate_pooled,
            skills_communicate_split_oral,
            skills_communicate_split_written,
            understanding_oceanacid_pooled,
            understanding_oceanacid_split_chemistry,
            understanding_oceanacid_split_organisms,
            understanding_society_pooled,
            understanding_society_split_realissues,
            understanding_society_split_society,
            understanding_sound_pooled,
            understanding_sound_split_general,
            understanding_sound_split_organisms)) %>%
  arrange(year, test, Number) %>%
  ungroup() 

############################################################################
############################################################################
#### MANUSCRIPT REVISION: one of the items (Career) has a factor loading greater than 1.0, which is a Heywood case. 
# re-run all tests excluding the Career item
tidy_dat_all <- tidy_dat_all %>%
  select(-attitudes_career)

############################################################################
############################################################################


# Create subsets of pre and post datasets - filter both subsets to the same questions, so they are comparable
tidy_dat_pre <- tidy_dat_all %>%
  filter(test=="pre") 

tidy_dat_post <- tidy_dat_all %>%
  filter(test=="post")

# Tally for pre and post datasets to report in Methods writeup
# tidy_dat_pre %>% select(Number, year) %>% distinct() %>% nrow()
# [1] 103
# > tidy_dat_post %>% select(Number, year) %>% distinct() %>% nrow()
# [1] 105

# Tally per year to figure out response rate:
# tidy_dat_pre %>% select(Number, year) %>% distinct() %>% group_by(year) %>% count()
# Pre-data:
# year      n
# <chr> <int>
# 1 2013     20
# 2 2014     16
# 3 2015     17
# 4 2016     16
# 5 2017     17
# 6 2018     17

# tidy_dat_post %>% select(Number, year) %>% distinct() %>% group_by(year) %>% count()
# Post-data:
# year      n
# <chr> <int>
#   1 2013     20
# 2 2014     15
# 3 2015     17
# 4 2016     19
# 5 2017     17
# 6 2018     17

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
  p <- ggplot(pivot_longer(cor_dat, cols = names(cor_dat)[1]:names(cor_dat)[ncol(cor_dat)]) %>% filter(is.na(value)==FALSE), aes(x = value)) +
    geom_histogram() +
    facet_wrap(.~name, ncol = 4)
  hist_pdf <- paste("histogram_", i, ".pdf", sep="")
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
  
  cor_pdf_plain <- paste("corrplot_plain_", i, ".pdf", sep="")
  pdf(cor_pdf_plain)
  corrplot(cor_matrix, tl.col="black", tl.cex=0.75) # insig = "label_sig" means label the significant p-values; otherwise option is to label the insignificant values 
  dev.off()
  #drive_upload(cor_pdf_plain, path = as_dribble("REMS_SALG/Results")) # for initial upload
  
  cor_pdf_x_insig <- paste("corrplot_x_insig_", i, ".pdf", sep="")
  pdf(cor_pdf_x_insig)
  corrplot(cor_matrix, p.mat = p_95$p, sig.level = 0.05, tl.col = "black", tl.cex = 0.75, insig = "blank")
  dev.off()
  # ALTERNATIVELY, place an "X" wherever values are insiginficant
  #corrplot(cor_matrix, p.mat = p_95$p, sig.level = 0.05, pch.cex = 0.8, tl.col="black", tl.cex=0.75) 
  #drive_upload(cor_pdf_x_insig, path = as_dribble("REMS_SALG/Results")) # for initial upload
  
  if (i=="pre"){ 
    drive_update(file = as_id("14xD9rjNBqHMs4YCrScgJkHOXBqjRKNq3"), media = cor_csv)
    drive_update(file = as_id("1H8S93QVwckWcdoZgswte8HIMGqXGQgjo"), media = hist_pdf)
    drive_update(file = as_id("1wcTlCgRiw6Cr2AInM7J9MBcjU_nWde1z"), media = p_csv)
    drive_update(file = as_id("1ldH4cd7FLuJCNPYpBty-lusqiSlZvXPi"), media = cor_pdf_plain)
    drive_update(file = as_id("1F273l84K8ZBzsrHioL-jDEtmO_BVAxcT"), media = cor_pdf_x_insig)
  }
  if (i=="post"){ 
    drive_update(file = as_id("1nFd4Drs6sZd3hQ_Gq9IWWjDec3cvLRo0"), media = cor_csv)
    drive_update(file = as_id("1iZroyQvuEnhJEMYfrZLWCDfsyJBC6EhK"), media = hist_pdf)
    drive_update(file = as_id("1eR4_lunZCUREmhDKboBSUjeIOTQn0BGh"), media = p_csv)
    drive_update(file = as_id("1f4gDObM6sPIB3ZfMN-3QZexIAl-f3Kq7"), media = cor_pdf_plain)
    drive_update(file = as_id("1z0d3A-XPbpulp6enqqasw2E8FQy8RAmd"), media = cor_pdf_x_insig)
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
## Use describe() to examine normality of each variable ()
## caution: describe() shares namespace with other packages (e.g., Hmisc); use psych::describe to specify

dat_describe <- psych::describe(tidy_dat_all[,4:dim(tidy_dat_all)[2]])
describe_csv <- "dat_describe.csv"
write.csv(dat_describe, describe_csv, row.names = TRUE) # write out row.names because these list the different "questions"
#drive_upload(describe_csv, path = as_dribble("REMS_SALG/Results")) # for initial upload
drive_update(file = as_id("1PH6jTAaII5YZykb537XVBL9-1tQTkyn-"), media = describe_csv)
file.remove(describe_csv)


###################################################################################################################
# Finalize variable list for further analysis:
tidy_dat_pre_final <- tidy_dat_pre # In case any further filtering needed
tidy_dat_post_final <- tidy_dat_post # In case any further filtering needed

# Additional EFA data inspection tests:

# 1 - Bartlett's test of sphericity - tests whether observed correlation matrix is an identity matrix;
# Tests whether or not the correlation matrix exhibits any relationships or if has a complete lack of relationships (i.e., the identity matrix)
pre_cor_matrix <- cor(tidy_dat_pre_final[,-c(1:3)], use="pairwise.complete.obs")

# RESULTS: Test shows results are significantly different from the identity matrix
cortest.bartlett(R = pre_cor_matrix, n = nrow(tidy_dat_pre_final), diag = TRUE) 

# SAVE RESULTS:
bartlett_name <- "Bartlett's_test_pre.txt"
sink(bartlett_name)
print(cortest.bartlett(R = pre_cor_matrix, n = nrow(tidy_dat_pre_final), diag = TRUE))
sink()

#drive_upload(bartlett_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("15Lt6GE_GMQMO_CBqhVndawWrIxm-4Zfr"), media = bartlett_name)  
file.remove(bartlett_name)

# Repeat Bartlett's test on post data:
post_cor_matrix <- cor(tidy_dat_post_final[,-c(1:3)], use="pairwise.complete.obs")

# RESULTS: Test shows results are significantly different from the identity matrix
cortest.bartlett(R = post_cor_matrix, n = nrow(tidy_dat_post_final), diag = TRUE) 

# SAVE RESULTS:
bartlett_name <- "Bartlett's_test_post.txt"
sink(bartlett_name)
print(cortest.bartlett(R = post_cor_matrix, n = nrow(tidy_dat_post_final), diag = TRUE))
sink()

# drive_upload(bartlett_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
# Use drive_update to update specific file based on ID number
drive_update(file = as_id("1S5pT0BAw4P9fpzsur4pmp2qadGbI7rxI"), media = bartlett_name)  
file.remove(bartlett_name)

# 2 - Kaiser, Meyer, Olkin measure of Sampling Adequacy

KMO(r = pre_cor_matrix)
# RESULS: Test shows all variables are above cutoff of 0.6
# This indicates that common variance (and thus latent factors) are present in the data

# SAVE RESULTS:
KMO_name <- "KMO_test_pre.txt"
sink(KMO_name)
print(KMO(r = pre_cor_matrix))
sink()

#drive_upload(KMO_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
drive_update(file = as_id("1zZrJnhstOanMublbRdU7CucNURGi2INK"), media = KMO_name)  
file.remove(KMO_name)

# REPEAT on post data:
KMO(r = post_cor_matrix)
# RESULS: Test shows all variables are above cutoff of 0.6
# This indicates that common variance (and thus latent factors) are present in the data

# SAVE RESULTS:
KMO_name <- "KMO_test_post.txt"
sink(KMO_name)
print(KMO(r = post_cor_matrix))
sink()

# drive_upload(KMO_name, path = as_dribble("REMS_SALG/Results")) # for initial upload
drive_update(file = as_id("1ZufOACxI-7PSp1gdtvH0LjHip2oeHt15"), media = KMO_name)  
file.remove(KMO_name)

###################################################################################################################
# SCREE PLOTS 
# RESULT: use 5 factors for PRE data; use 2 factors for POST data

# NOTES on SCREE PLOTS: (from Howard 2016)
# plots each eigenvalue on a graph and determine when decreases in successive eigenvalues start to asymptote
# only include the number of factors up until the asymptote; these represent common variance better than the factors after the asymptote

time_point <- c("pre", "post")
for (i in time_point){
  
  tidy_dat <- get(paste("tidy_dat_", i, "_final", sep=""))
  
  dat_scaled <- scale(tidy_dat[,4:dim(tidy_dat)[2]], center=TRUE, scale=TRUE)
  
  # SCREE PLOT TO DETERMINE THE NUMBER OF FACTORS IN THE DATA
  # FIX IT - in addition to scree plot, do parallel analysis?
  # See: https://quantdev.ssri.psu.edu/tutorials/intro-basic-exploratory-factor-analysis
  cor_matrix <- cor(dat_scaled, use = "pairwise.complete.obs")
  
  # Print to console:
  cat("For", i, "data:\n")
  
  scree_name <- paste("screeplot_", i, ".pdf", sep = "")
  pdf(file = scree_name)
  fa.parallel(x = cor_matrix, fm = "ml", fa = "fa", n.obs = nrow(dat_scaled)) # "ml" is the maximum likelihood method for "well-behaved" data
  dev.off()
  #drive_upload(scree_name, path = as_dribble("REMS_SALG/Results")) # for initial upload

  #Use drive_update to update specific file based on ID number
  if (i == "pre"){
    drive_update(file = as_id("14EXw5PQJHLfAlwKRQTmQ90Ss-wj-GvbP"), media = scree_name)
  }
  if (i == "post"){
    drive_update(file = as_id("1vI2xKnoYHSLvO728aJFo-LF7P5d-L8AA"), media = scree_name)
  }
  
  file.remove(scree_name)
  
}

###################################################################################################################
# FACTOR ANALYSIS
# Uses nfactors 2, 3, 4, 5, 6
# REMINDER: 2 and 3 factors justified based on criteria that eigenvalues be greater than 1; while 4, 5, and 6 factors justified based on parallel analysis
# Note: script produces results for pre and post data, but only the pre results are uploaded to Google Drive since this is the only one we care about for EFA

# set nfactors to results from Scree plot
# rotate = "promax" what Goodwin 2016 used:
# A promax (non-orthogonal or oblique) rotation was employed since the theory naturally permits inter-correlation between the constructs (i.e., the factors were not expected to be orthogonal)

time_point <- c("pre", "post")
for (i in time_point){
  
  tidy_dat <- get(paste("tidy_dat_", i, "_final", sep=""))
  cor_dat <- tidy_dat[,4:dim(tidy_dat)[2]]
  cor_matrix <- cor(cor_dat, use="pairwise.complete.obs")
  
  for (f in c(2, 3, 4, 5, 6)){ # Set numbers of factors here; IMPORTANT: if decide to change number of factors here, remember conditionals for drive_update below
    EFA_results <- fa(r = cor_matrix, nfactors = f, rotate = "promax", fm = "ml") 
    
    # Write model outputs to textfile
    efa_file <- paste("EFA_", f, "factors_", i, ".txt", sep = "")
    sink(efa_file)
    print(EFA_results)
    sink()
    
    # Simplify model outputs: Write just factor loadings to textfile:
    #efa_loadings_file <- paste("EFA_", i, "_final_", f, "factors_loadingsONLY.txt", sep = "")
    #sink(efa_loadings_file)
    #print(EFA_results$loadings)
    #sink()
    
    # Simplify model outputs: Write factor loadings to csv file
    efa_loadings_csv <- paste("EFA_", f, "factors_loadings_", i, ".csv", sep = "")
    EFA_loadings_only <- EFA_results$loadings[1:nrow(EFA_results$loadings), 1:ncol(EFA_results$loadings)]
    write.csv(EFA_loadings_only, file = efa_loadings_csv)
    
    # drive_upload(efa_file, path = as_dribble("REMS_SALG/Results")) # for initial upload
    # drive_upload(efa_loadings_csv, path = as_dribble("REMS_SALG/Results")) # for initial upload

    # Use drive_update to update specific file based on ID number
    if (i == "pre" & f == 2){
      drive_update(file = as_id("1r8GJaWzEIpu17YbxpnG8NooV3WxCan7s"), media = efa_file) 
      drive_update(file = as_id("1ohYtpB0FJ7yqV1YxXO4fShaq4Eg4xTCW"), media = efa_loadings_csv) 
    }
    if (i == "pre" & f == 3){
      drive_update(file = as_id("1C8Yshu--XRqa78RX5qA7i4TPqREPMllc"), media = efa_file) 
      drive_update(file = as_id("1vw4L5QeT-YyVhQLzXniTuJB_zf-Cny5j"), media = efa_loadings_csv) 
    }
    if (i == "pre" & f == 4){
      drive_update(file = as_id("1AbA0epFMe575AhxvApZ_xkU5Hh7WD6ik"), media = efa_file) 
      drive_update(file = as_id("1uVjjLA9KFL43P5Lcr6nKLPZCw4qJAGXQ"), media = efa_loadings_csv) 
    }
    if (i == "pre" & f == 5){
      drive_update(file = as_id("1VlHJPqcJyv6eKBspjL9nDTiWWBpUeHN4"), media = efa_file)
      drive_update(file = as_id("1jFOjuZ5wvF1Nh-1TyKTy2wEpzGssjh9W"), media = efa_loadings_csv)
    }
    if (i == "pre" & f == 6){
      drive_update(file = as_id("1OznjkUMAaBKYPjmkIKgW_xSaSPJ7m9UD"), media = efa_file)
      drive_update(file = as_id("1gfZ9QVFucVzQbejumzOreaSZp4leprKL"), media = efa_loadings_csv)
    }
    
    if (i == "post" & f == 2){
      drive_update(file = as_id("1O-rkcPO2r_5WW1i4PIWK_Jfu-7lhz_bm"), media = efa_file) 
      drive_update(file = as_id("1itde8givq9R8adOqs7RKoP8v0ZRhJMLa"), media = efa_loadings_csv) 
    }
    if (i == "post" & f == 3){
      drive_update(file = as_id("1AXZuqdgyTknX1PXysexBcVUgJr9BYzPm"), media = efa_file) 
      drive_update(file = as_id("1HhDWs2fJbVdggmFEbL2Pq28wHie9IXxK"), media = efa_loadings_csv) 
    }
    if (i == "post" & f == 4){
      drive_update(file = as_id("165hqfW5Xas3nUJIYcz1x9CFbQjmwomVR"), media = efa_file) 
      drive_update(file = as_id("1S_wAXd0iWTE3-PjxdjEkz68n2FMw_xvA"), media = efa_loadings_csv) 
    }
    if (i == "post" & f == 5){
      drive_update(file = as_id("1PE0AILn4n2dC9bWxajmfCaOl03mC0FyY"), media = efa_file)
      drive_update(file = as_id("1GDUu0l37C-Y2kyO6JvESVVRtlCRGx-bV"), media = efa_loadings_csv)
    }
    if (i == "post" & f == 6){
      drive_update(file = as_id("1wHwzTqiTe4Fn5QaUBiX-lWDjm7ooZb4Z"), media = efa_file)
      drive_update(file = as_id("1ZAJHVuPPaQoqMYTKxuwzp4EdHPSYI8ai"), media = efa_loadings_csv)
    }
    
    file.remove(efa_file)
    file.remove(efa_loadings_csv)
    
  } # END loop through different number of factors
  
} # END loop through pre and post

# CHANGE DESCRIPTION AS NECESSARY:
insert_description <- "pooled-qs-removed_likert-standardized_NAs-dropped"
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

