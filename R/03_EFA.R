# Do preliminary analysis (correlation and scree plots) and exploratory factor analysis
# kdgorospe@gmail.com

library(psych)
library(corrplot)
library(GPArotation) # needed for promax rotation in fa function


### REFERENCE: https://quantdev.ssri.psu.edu/tutorials/intro-basic-exploratory-factor-analysis
# FIX IT - check does Likert scale remain the same across years?
# Arrange data into tidy format (each row is a single student)
tidy_dat <- coded_dat %>%
  select(Number, answer, year, test, concept) %>%
  pivot_wider(values_from = answer, names_from = concept, id_cols = c(Number, year, test)) %>% # use values_fn = list(val = length) to identify coding duplicates
  arrange(year, test, Number) %>%
  # response value of 9 corresponsds to "NA"
  mutate_all(~na_if(., 9))

# Correlations:
cormatrix <- cor(tidy_dat[,4:dim(tidy_dat)[2]], use="pairwise.complete.obs")
write.csv(cormatrix, "cormatrix.csv", row.names = TRUE)
#drive_upload("cormatrix.csv", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for cormatrix.csv: 11J3A5fsuRS9bvB6Kun5tLrhexwE-tJZS
drive_update(file = as_id("11J3A5fsuRS9bvB6Kun5tLrhexwE-tJZS"), media = "cormatrix.csv")
file.remove("cormatrix.csv")

index_no_pairs <- which(is.na(cormatrix), arr.ind = TRUE)
vars_no_pairs <- cbind(rownames(cormatrix)[index_no_pairs[,1]], colnames(cormatrix)[index_no_pairs[,2]])
vars_no_pairs

# Inspect correlation table, remove variables with limited pairwise comparisons: 
# evaluation_aquaculture
# evaluation_ethology
# all evaluation_guestspeakers
# evaluation_plankton

# FIX IT - for now, remove major_multiplechoice and major_yesno and all variables that were either pooled or split - will need to recode all of these later
dat_analysis <- tidy_dat %>%
  select(-c(evaluation_aquaculture, evaluation_ethology, evaluation_planktoneco)) %>%
  select(-c(understanding_aquaculture, understanding_ethology)) %>%
  select_if(grepl("guestspeaker", names(.))==FALSE) %>%
  select_if(grepl("major", names(.))==FALSE) %>%
  select_if(grepl("pooled", names(.))==FALSE) %>%
  select_if(grepl("split", names(.))==FALSE)

# Plot shows clumps of correlated variables - evidence that common factors exist
pdf(file="corrplot.pdf")
corrplot(cor(dat_analysis[,4:dim(dat_analysis)[2]], use="pairwise.complete.obs"), order="hclust", tl.col="black", tl.cex=0.75)
dev.off()

#drive_upload("corrplot.pdf", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for corrplot.pdf: 11by0zbkkdEvmJYb6WbRgWqPLfYq_n7rS
drive_update(file = as_id("11by0zbkkdEvmJYb6WbRgWqPLfYq_n7rS"), media = "corrplot.pdf")
file.remove("corrplot.pdf")


## specify that we should use the describe function from psych package (shares namespace with other packages, e.g., Hmisc)
dat_describe <- psych::describe(dat_analysis[,4:dim(dat_analysis)[2]])

# According to Goodwin 2016, these variables should be removed because they violate assumption of normality: 
dat_describe[abs(dat_describe$skew) >= 2,]
dat_describe[abs(dat_describe$kurtosis) >= 7,]

write.csv(dat_describe, "dat_describe.csv", row.names = TRUE)
#drive_upload("dat_describe.csv", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for dat_describe.csv: 1F8VKg7lBRVEJ26Q77lFAgEn0n19TWPPJ
drive_update(file = as_id("1F8VKg7lBRVEJ26Q77lFAgEn0n19TWPPJ"), media = "dat_describe.csv")
file.remove("dat_describe.csv")



dat_scaled <- scale(dat_analysis[,4:dim(dat_analysis)[2]], center=TRUE, scale=TRUE)
dat_scaled <- cbind(dat_analysis[,1:3], dat_scaled)

# FIX IT - should i rerun describe and corrplot on separate pre and post dataframes?
# Split pre and post, do separate analyses for each
scaled_pre <- dat_scaled %>%
  filter(test == "pre") %>%
  # For PRE data, remove "evaluation" questions, these are only part of the POST data
  select_if(grepl("evaluation", names(.))==FALSE) %>%
  # FIX IT - how is analysis affected by NAs? Still runs even if we keep this in, but removing it for now
  # For preliminary analysis of PRE data, remove "understanding_coralclimate, coralskel, and diversity" questions which only showed up in 2017 and 2018
  select(-c(understanding_coralclimate, understanding_coralskel, understanding_diversity))
  
scaled_post <- dat_scaled %>%
  filter(test == "post")

# FIX IT - Analyze just the pre questions for now:

# SCREE PLOT TO DETERMINE THE NUMBER OF FACTORS IN THE DATA
# FIX IT - in addition to scree plot, do parallel analysis 
# See: https://quantdev.ssri.psu.edu/tutorials/intro-basic-exploratory-factor-analysis
cor_dat_pre <- cor(scaled_pre[,4:dim(scaled_pre)[2]], use = "pairwise.complete.obs")

pdf(file="pre_screeplot.pdf")
fa.parallel(x = cor_dat_pre, fm = "ml", fa = "fa") # "ml" is the maximum likelihood method for "well-behaved" data
dev.off()

#drive_upload("pre_screeplot.pdf", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for pre_screeplot.pdf: 1e3po3VpVgC8klgihg-3EHz0g6EoNy6mT
drive_update(file = as_id("1e3po3VpVgC8klgihg-3EHz0g6EoNy6mT"), media = "pre_screeplot.pdf")
file.remove("pre_screeplot.pdf")

# FACTOR ANALYSIS

# NOTE: set nfactors to results from Scree plot
# NOTE: rotate = "promax" is what Goodwin 2016 used
# A promax (non-orthogonal or oblique) rotation was employed since the theory naturally permits inter-correlation between the constructs (i.e., the factors were not expected to be orthogonal).
EFA_pre_cordat_5factor <- fa(r = cor_dat_pre, nfactors = 5, rotate = "promax", fm = "ml") 

# Write model outputs to textfile
sink("EFA_pre_cordat_5factor.txt")
EFA_pre_cordat_5factor
sink()

#drive_upload("EFA_pre_cordat_5factor.txt", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for EFA_pre_cordat_5factor.txt: 1xBtWz9dBAap1kjjh00H3liHr1Don_Niu
drive_update(file = as_id("1xBtWz9dBAap1kjjh00H3liHr1Don_Niu"), media = "EFA_pre_cordat_5factor.txt")
file.remove("EFA_pre_cordat_5factor.txt")

# Simplify model outputs: Write just factor loadings to textfile:
sink("EFA_pre_cordat_5factor_loadings.txt")
EFA_pre_cordat_5factor$loadings
sink()

#drive_upload("EFA_pre_cordat_5factor_loadings.txt", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for EFA_pre_cordat_5factor_loadings.txt: 1JUY5F8OWKNTKgpdN2HMtpTXAUDw2XUFZ
drive_update(file = as_id("1JUY5F8OWKNTKgpdN2HMtpTXAUDw2XUFZ"), media = "EFA_pre_cordat_5factor_loadings.txt")
file.remove("EFA_pre_cordat_5factor_loadings.txt")


# To find out how much variance is accounted for by the factors:
EFA_pre_cordat_5factor$Vaccounted
EFA_pre_cordat_5factor$Vaccounted[3, 5]


# Function "fa" will take either raw data or correlation matrix, butto get individual factor scores, need to input raw data 
#EFA_pre_rawdat_5factor <- fa(r = scaled_pre[,4:dim(scaled_pre)[2]], nfactors = 5, rotate = "promax", fm = "ml") 
#EFA_pre_rawdat_5factor$scores



