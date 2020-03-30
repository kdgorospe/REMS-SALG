# Read-in questions and check for alignment
rm(list=ls())
library(tidyverse)
library(googledrive)
library(data.table)

# note: file IDs saved as text file in google drive
# post data:
post_ids <- c("1qyFMQvFGbXaQxODcFrXMTTjKU18VOeTa",
              "1YrvX4e_vfP0ioJm7f7lI7bb4ItEbmjHg",
              "1J2vfrYA0uKpVIeXDeDQvAsFxXn3MiMXw",
              "1-9VQREFtQv1S4bgexdVs98kSj1wqdQN_",
              "11smDxjIWjXvRKAWOcR3oPCLW9XOoZaSL",
              "1htQGcatpXFVVVfVKE9EsWSFXXxY81x0G")

# pre data:
pre_ids <- c("19yWE7PkE4G1IAGvAJzLrH70Oj-UXcPLu",
             "1pRgwUjvMlui8c0mO4Nz0-OAjL44EP-AM",
             "1pIY3SgwcsmHhsbAXmaZCVCiKYgOZ8M7j",
             "136Ql2UfwM8N6zZAgtzmYMA8RG28_NBdx",
             "1okd1yAcTUpEG8GBbQ66SDJcBIp-W3wUW",
             "1zWutigwA789lXtpb3rHkk3uctynjfM2O")

# Download files to local computer
for (i in 1:length(post_ids)) {
  drive_download(as_id(post_ids[i]), overwrite=TRUE)
}
for (i in 1:length(pre_ids)) {
  drive_download(as_id(pre_ids[i]), overwrite=TRUE)
}

# Read post data files into R
all_filenames <- list.files()
post_grep <- grep("POST", all_filenames)

for (i in post_grep) {
  dat_year <- strsplit(all_filenames[i], split = " ")[[1]][1]
  dat_object <- paste("post", dat_year, sep = "")
  assign(dat_object, read.csv(all_filenames[i]))
}

# Read pre data files into R
pre_grep <- grep("PRE", all_filenames)

for (i in pre_grep) {
  dat_year <- strsplit(all_filenames[i], split = " ")[[1]][1]
  dat_object <- paste("pre", dat_year, sep = "")
  assign(dat_object, read.csv(all_filenames[i]))
}

# Remove files from local computer
for (i in post_grep) {
  file.remove(all_filenames[i])
}
for (i in pre_grep) {
  file.remove(all_filenames[i])
}

years <- seq(2013, 2018, 1)
question_list = list()

for (i in 1:length(years)){
  year_i = years[i]
  year_objects <- apropos(as.character(year_i))
  question_pair = list()
  for (q in 1:length(year_objects)){
    # Get the questions
    question_dat <- get(year_objects[q])

    question_dat <- question_dat %>%
      # Filter - Keep only columns "Number" and "Question"
      select(Number, Question) %>%
      # Remove questions that don't have a "." (these are just heading numbers, not questions)
      filter(grepl("\\.", question_dat$Number)) %>%
      # Need to add an ID column so that pivot_wider knows to collapse all data into one row
      mutate(id = year_objects[q]) %>%
      pivot_wider(names_from = Number, values_from = Question)

    question_pair[[q]] <- question_dat
  }

  question_list[[i]] <- rbindlist(question_pair, fill = TRUE)

}

question_list <- rbindlist(question_list, fill = TRUE)

# Remove question 1.1 (header - not actually a question)
question_list <- question_list %>%
  select(-'1.1')

question_list <- question_list %>%
  mutate_if(.predicate = is.factor, .funs = as.character)



write.csv(question_list, "question_list.csv", row.names = FALSE)
#drive_upload("question_list.csv", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for question_list: 1ncufvuw3zCHRPiLX6ykdLKv498JD01poJIKjljQjYAc
drive_update(file = as_id("1ncufvuw3zCHRPiLX6ykdLKv498JD01poJIKjljQjYAc"), media = "question_list.csv")
file.remove("question_list.csv")


### THe following creates a list of question IDs to be used for specifying the data structure
question_id_list = list()
# Create list of questions and ids
# start with i=2 because i=1 is ID column
for (i in 2:length(names(question_list))){
  # get IDs just for those questions that are not NAs
  ids <- question_list$id[!is.na(question_list[,i])]
  question_id_set <- cbind.data.frame(ids, q_number = names(question_list)[i], question = )
  question_id_set <- question_id_set %>%
    extract(col = ids, into = c("pre_post", "year"), regex = "^([a-z]+)(\\d+)$")
  question_id_list[[i]] <- question_id_set
}

question_id_list <- rbindlist(question_id_list)


# Assign concepts to questions
# See: notes_question_alignment google doc
question_id_list <- question_id_list %>%
  mutate(concept = case_when(
    # Questions that stay consistent throughout
    pre_post == "pre" & q_number == "1.1.1" ~ "pre_understanding_ecology",
    pre_post == "post" & q_number == "1.1.1" ~ "post_understanding_ecology",
    
    pre_post == "pre" & q_number == "1.1.2" ~ "pre_understanding_sciprocess",
    pre_post == "post" & q_number == "1.1.2" ~ "post_understanding_sciprocess",
  
    pre_post == "pre" & q_number == "1.1.5" ~ "pre_understanding_urchinfertilization",
    pre_post == "post" & q_number == "1.1.5" ~ "post_understanding_urchinfertilization",
    
        
    # Questions that drop out and/or appear (need to filter by year)
    pre_post == "pre" & q_number == "1.1.4" & year >= 2013 & year <= 2014 ~ "pre_understanding_aquaculture",
    pre_post == "post" & q_number == "1.1.4" & year >= 2013 & year <= 2014 ~ "post_understanding_aquaculture",
    
    pre_post == "pre" & q_number == "1.1.4" & year >= 2015 & year <= 2016 ~ "pre_understanding_ethology",
    pre_post == "post" & q_number == "1.1.4" & year >= 2015 & year <= 2016 ~ "post_understanding_ethology",
    
    pre_post == "pre" & q_number == "1.2" & year >= 2013 & year <= 2016 ~ "pre_understanding_life",
    pre_post == "pre" & q_number == "2.1" & year >= 2017 & year <= 2018 ~ "pre_understanding_life",
    pre_post == "post" & q_number == "1.2" & year >= 2013 & year <= 2016 ~ "post_understanding_life",
    pre_post == "post" & q_number == "2.1" & year >= 2017 & year <= 2018 ~ "post_understanding_life",
    
    
    pre_post == "pre" & q_number == "2.5" & year >= 2013 & year <= 2016 ~ "pre_skill_withothers",
    pre_post == "post" & q_number == "2.5" & year >= 2013 & year <= 2016 ~ "post_skill_withothers",
    
    pre_post == "pre" & q_number == "1.1.8" ~ "pre_understanding_diversity",
    pre_post == "post" & q_number == "1.1.8" ~ "post_understanding_diversity",
    
    pre_post == "pre" & q_number == "1.1.9" ~ "pre_understanding_coralskel",
    pre_post == "post" & q_number == "1.1.9" ~ "post_understanding_coralskel",
    
    pre_post == "pre" & q_number == "1.1.10" ~ "pre_understanding_climatechange",
    pre_post == "post" & q_number == "1.1.10" ~ "post_understanding_climatechange",
    
    
    # Questions only asked "POST" that change numbers in different years: at least two queries for each pre and post set
    pre_post == "post" & q_number == "6.1" & year >= 2013 & year <= 2016 ~ "prog_eval_coraleco",
    pre_post == "post" & q_number == "7.1" & year >= 2017 & year <= 2018 ~ "prog_eval_coraleco",
    
    pre_post == "post" & q_number == "6.3" & (year == 2013 | (year>= 2015 & year <= 2016)) ~ "prog_eval_oceanacid",
    pre_post == "post" & q_number == "6.2" & year == 2014 ~ "prog_eval_oceanacid",
    pre_post == "post" & q_number == "7.2" & year >= 2017 & year <= 2018 ~ "prog_eval_oceanacid",
    
    pre_post == "post" & q_number == "6.5" & year == 2013 ~ "prog_eval_bioacoustics",
    pre_post == "post" & q_number == "6.7" & year == 2014 ~ "prog_eval_bioacoustics",
    pre_post == "post" & q_number == "6.8" & year == 2015 ~ "prog_eval_bioacoustics",
    pre_post == "post" & q_number == "6.6" & year == 2016 ~ "prog_eval_bioacoustics",
    pre_post == "post" & q_number == "7.4" & (year == 2017 | year == 2018) ~ "prog_eval_bioacoustics",
    
    
    pre_post == "post" & q_number == "6.7" & (year == 2013 | year == 2015) ~ "prog_eval_urchinfertilization",
    pre_post == "post" & q_number == "6.5" & (year == 2014 | year == 2016) ~ "prog_eval_urchinfertilization",
    pre_post == "post" & q_number == "7.3" & year <= 2017 & year <= 2018 ~ "prog_eval_urchinfertilization",
    
    pre_post == "post" & q_number == "6.8" & year == 2013 ~ "prog_eval_spawning",
    
    pre_post == "post" & q_number == "6.4" & year == 2014 ~ "prog_eval_planktoneco",
    
    pre_post == "post" & q_number == "6.5" & year == 2015 ~ "prog_eval_squid",
    pre_post == "post" & q_number == "6.4" & year == 2016 ~ "prog_eval_squid",
    
    
    pre_post == "post" & q_number == "6.10" & (year == 2013 | year == 2015) ~ "prog_eval_scimethod",
    pre_post == "post" & q_number == "6.9" & year == 2014 ~ "prog_eval_scimethod",
    pre_post == "post" & q_number == "6.7" & year == 2016 ~ "prog_eval_scimethod",
    pre_post == "post" & q_number == "7.5" & (year == 2017 | year == 2018) ~ "prog_eval_scimethod",
    
    pre_post == "post" & q_number == "6.11" & (year == 2013 | year == 2015) ~ "prog_eval_careerday",
    pre_post == "post" & q_number == "6.10" & year == 2014 ~ "prog_eval_careerday",
    pre_post == "post" & q_number == "6.8" & year == 2016 ~ "prog_eval_careerday",
    pre_post == "post" & q_number == "7.6" & (year == 2017 | year == 2018) ~ "prog_eval_careerday",
    
    pre_post == "post" & q_number == "7.1" & year >= 2013 & year <= 2016 ~ "prog_eval_groupresearch",
    pre_post == "post" & q_number == "8.1" & year >= 2017 & year <= 2018 ~ "prog_eval_groupresearch",
    
    pre_post == "post" & q_number == "7.2" & year >= 2013 & year <= 2016 ~ "prog_eval_confidentresearch",
    pre_post == "post" & q_number == "8.2" & year >= 2017 & year <= 2018 ~ "prog_eval_confidentresearch",
    
    
    # Questions that change numbers in different years: at least two queries for each pre and post set
    pre_post == "pre" & q_number == "2.1" & year >= 2013 & year <= 2016 ~ "pre_skill_developh0",
    pre_post == "pre" & q_number == "3.1" & year >= 2017 & year <= 2018 ~ "pre_skill_developh0",
    pre_post == "post" & q_number == "2.1" & year >= 2013 & year <= 2016 ~ "post_skill_developh0",
    pre_post == "post" & q_number == "3.1" & year >= 2017 & year <= 2018 ~ "post_skill_developh0",
    
    pre_post == "pre" & q_number == "2.2" & year >= 2013 & year <= 2016 ~ "pre_skill_testh0",
    pre_post == "pre" & q_number == "3.2" & year >= 2017 & year <= 2018 ~ "pre_skill_testh0",
    pre_post == "post" & q_number == "2.2" & year >= 2013 & year <= 2016 ~ "post_skill_testh0",
    pre_post == "post" & q_number == "3.2" & year >= 2017 & year <= 2018 ~ "post_skill_testh0",
    
    pre_post == "pre" & q_number == "2.3" & year >= 2013 & year <= 2016 ~ "pre_skill_evaluateh0",
    pre_post == "pre" & q_number == "3.3" & year >= 2017 & year <= 2018 ~ "pre_skill_evaluateh0",
    pre_post == "post" & q_number == "2.3" & year >= 2013 & year <= 2016 ~ "post_skill_evaluateh0",
    pre_post == "post" & q_number == "3.3" & year >= 2017 & year <= 2018 ~ "post_skill_evaluateh0",
    
    pre_post == "pre" & q_number == "2.5" & year >= 2013 & year <= 2016 ~ "pre_skill_withothers",
    pre_post == "pre" & q_number == "3.6" & year >= 2017 & year <= 2018 ~ "pre_skill_withothers",
    pre_post == "post" & q_number == "2.5" & year >= 2013 & year <= 2016 ~ "post_skill_withothers",
    pre_post == "post" & q_number == "3.6" & year >= 2017 & year <= 2018 ~ "post_skill_withothers",
    
    pre_post == "pre" & q_number == "3.1" & year >= 2013 & year <= 2016 ~ "pre_attitude_enthusiastic",
    pre_post == "pre" & q_number == "4.1" & year >= 2017 & year <= 2018 ~ "pre_attitude_enthusiastic",
    pre_post == "post" & q_number == "3.1" & year >= 2013 & year <= 2016 ~ "post_attitude_enthusiastic",
    pre_post == "post" & q_number == "4.1" & year >= 2017 & year <= 2018 ~ "post_attitude_enthusiastic",
    
    pre_post == "pre" & q_number == "3.2" & year >= 2013 & year <= 2016 ~ "pre_attitude_discussing",
    pre_post == "pre" & q_number == "4.2" & year >= 2017 & year <= 2018 ~ "pre_attitude_discussing",
    pre_post == "post" & q_number == "3.2" & year >= 2013 & year <= 2016 ~ "post_attitude_discussing",
    pre_post == "post" & q_number == "4.2" & year >= 2017 & year <= 2018 ~ "post_attitude_discussing",
    
    pre_post == "pre" & q_number == "3.3" & year >= 2013 & year <= 2016 ~ "pre_attitude_career",
    pre_post == "pre" & q_number == "4.3" & year >= 2017 & year <= 2018 ~ "pre_attitude_career",
    pre_post == "post" & q_number == "3.3" & year >= 2013 & year <= 2016 ~ "post_attitude_career",
    pre_post == "post" & q_number == "4.3" & year >= 2017 & year <= 2018 ~ "post_attitude_career",
    
    pre_post == "pre" & q_number == "3.4" & year >= 2013 & year <= 2016 ~ "pre_attitude_understand",
    pre_post == "pre" & q_number == "4.4" & year >= 2017 & year <= 2018 ~ "pre_attitude_understand",
    pre_post == "post" & q_number == "3.4" & year >= 2013 & year <= 2016 ~ "post_attitude_understand",
    pre_post == "post" & q_number == "4.4" & year >= 2017 & year <= 2018 ~ "post_attitude_understand",
    
    pre_post == "pre" & q_number == "3.5" & year >= 2013 & year <= 2016 ~ "pre_attitude_execute",
    pre_post == "pre" & q_number == "4.5" & year >= 2017 & year <= 2018 ~ "pre_attitude_execute",
    pre_post == "post" & q_number == "3.5" & year >= 2013 & year <= 2016 ~ "post_attitude_execute",
    pre_post == "post" & q_number == "4.5" & year >= 2017 & year <= 2018 ~ "post_attitude_execute",
    
    pre_post == "pre" & q_number == "3.6" & year >= 2013 & year <= 2016 ~ "pre_attitude_withothers",
    pre_post == "pre" & q_number == "4.6" & year >= 2017 & year <= 2018 ~ "pre_attitude_withothers",
    pre_post == "post" & q_number == "3.6" & year >= 2013 & year <= 2016 ~ "post_attitude_withothers",
    pre_post == "post" & q_number == "4.6" & year >= 2017 & year <= 2018 ~ "post_attitude_withothers",
    
    pre_post == "pre" & q_number == "4.1" & year >= 2013 & year <= 2016 ~ "pre_integration_connecting",
    pre_post == "pre" & q_number == "5.1" & year >= 2017 & year <= 2018 ~ "pre_integration_connecting",
    pre_post == "post" & q_number == "4.1" & year >= 2013 & year <= 2016 ~ "post_integration_connecting",
    pre_post == "post" & q_number == "5.1" & year >= 2017 & year <= 2018 ~ "post_integration_connecting",
    
    pre_post == "pre" & q_number == "4.2" & year >= 2013 & year <= 2016 ~ "pre_integration_applying",
    pre_post == "pre" & q_number == "5.2" & year >= 2017 & year <= 2018 ~ "pre_integration_applying",
    pre_post == "post" & q_number == "4.2" & year >= 2013 & year <= 2016 ~ "post_integration_applying",
    pre_post == "post" & q_number == "5.2" & year >= 2017 & year <= 2018 ~ "post_integration_applying",
    
    pre_post == "pre" & q_number == "5.6" & year >= 2013 & year <= 2016 ~ "pre_major_college_specific",
    pre_post == "pre" & q_number == "6.6" & year == 2017 ~ "pre_major_college_specific",
    pre_post == "pre" & q_number == "6.2" & year == 2018 ~ "pre_major_college_specific",
    pre_post == "post" & q_number == "5.6" & year >= 2013 & year <= 2016 ~ "post_major_college_specific",
    pre_post == "post" & q_number == "6.6" & year == 2017  ~ "post_major_college_specific",
    pre_post == "post" & q_number == "6.2" & year == 2018 ~ "post_major_college_specific",
    
    pre_post == "pre" & q_number == "5.6" & year >= 2013 & year <= 2014 ~ "pre_major_major_specific",
    pre_post == "pre" & q_number == "5.7" & year == 2015 ~ "pre_major_major_specific",
    pre_post == "pre" & q_number == "6.7" & year == 2017 ~ "pre_major_major_specific",
    pre_post == "pre" & q_number == "6.3" & year == 2018 ~ "pre_major_major_specific",
    pre_post == "post" & q_number == "5.6" & year >= 2013 & year <= 2014 ~ "post_major_major_specific",
    pre_post == "post" & q_number == "5.7" & year >= 2015 & year <= 2016 ~ "post_major_major_specific",
    pre_post == "post" & q_number == "6.7" & year == 2017  ~ "post_major_major_specific",
    pre_post == "post" & q_number == "6.3" & year == 2018 ~ "post_major_major_specific",
    
    
    # Questions that switch questions numbers and then switch from YES/NO to multiple choice 
    pre_post == "pre" & q_number == "5.1" & year >= 2013 & year <= 2016 ~ "pre_major_marsci_yn",
    pre_post == "pre" & q_number == "6.1" & year >= 2017  ~ "pre_major_marsci_yn",
    pre_post == "pre" & q_number == "6.1" & year >= 2018  ~ "pre_major_marsci_multichoice",
    pre_post == "post" & q_number == "5.1" & year >= 2013 & year <= 2016 ~ "post_major_marsci_yn",
    pre_post == "post" & q_number == "6.1" & year >= 2017  ~ "post_major_marsci_yn",
    pre_post == "post" & q_number == "6.1" & year >= 2018  ~ "post_major_multichoice",
    
    pre_post == "pre" & q_number == "5.2" & year >= 2013 & year <= 2016 ~ "pre_major_science_yn",
    pre_post == "pre" & q_number == "6.2" & year >= 2017  ~ "pre_major_science_yn",
    pre_post == "post" & q_number == "5.2" & year >= 2013 & year <= 2016 ~ "post_major_science_yn",
    pre_post == "post" & q_number == "6.2" & year >= 2017  ~ "post_major_science_yn",
    
    pre_post == "pre" & q_number == "5.3" & year >= 2013 & year <= 2016 ~ "pre_major_notscience_yn",
    pre_post == "pre" & q_number == "6.3" & year >= 2017  ~ "pre_major_notscience_yn",
    pre_post == "post" & q_number == "5.3" & year >= 2013 & year <= 2016 ~ "post_major_notscience_yn",
    pre_post == "post" & q_number == "6.3" & year >= 2017  ~ "post_major_notscience_yn",
  
    pre_post == "pre" & q_number == "5.4" & year >= 2013 & year <= 2016 ~ "pre_major_undecided_yn",
    pre_post == "pre" & q_number == "6.4" & year >= 2017  ~ "pre_major_undecided_yn",
    pre_post == "post" & q_number == "5.4" & year >= 2013 & year <= 2016 ~ "post_major_undecided_yn",
    pre_post == "post" & q_number == "6.4" & year >= 2017  ~ "post_major_undecided_yn",
    
    pre_post == "pre" & q_number == "5.5" & year >= 2013 & year <= 2016 ~ "pre_major_nocollege_yn",
    pre_post == "pre" & q_number == "6.5" & year >= 2017  ~ "pre_major_nocollege_yn",
    pre_post == "post" & q_number == "5.5" & year >= 2013 & year <= 2016 ~ "post_major_nocollege_yn",
    pre_post == "post" & q_number == "6.5" & year >= 2017  ~ "post_major_nocollege_yn",
    
    
    
    # Questions that are split vs pooled in different years
    pre_post == "pre" & q_number == "1.3" & year >= 2013 & year <= 2016 ~ "pre_understanding_importance_pooled",
    pre_post == "post" & q_number == "1.3" & year >= 2013 & year <= 2016 ~ "post_understanding_importance_pooled",
    
    
    pre_post == "pre" & q_number == "2.2" & year >= 2017 & year <= 2018 ~ "pre_understanding_importance_split",
    pre_post == "post" & q_number == "2.2" & year >= 2017 & year <= 2018 ~ "post_understanding_importance_split",
    
    pre_post == "pre" & q_number == "2.3" & year >= 2017 & year <= 2018 ~ "pre_understanding_importance_split",
    pre_post == "post" & q_number == "2.3" & year >= 2017 & year <= 2018 ~ "post_understanding_importance_split",
    
    
    
    pre_post == "pre" & (q_number == "1.1.4" | q_number == "1.1.3") & year >= 2017 & year <= 2018 ~ "pre_understanding_oceanacid_split",
    pre_post == "post" & (q_number == "1.1.4" | q_number == "1.1.3") & year >= 2017 & year <= 2018 ~ "post_understanding_oceanacid_split",
    pre_post == "pre" & q_number == "1.1.3" & year >= 2013 & year <= 2016 ~ "pre_understanding_oceanacid_pooled",
    pre_post == "post" & q_number == "1.1.3" & year >= 2013 & year <= 2016 ~ "post_understanding_oceanacid_pooled",
    
    
    pre_post == "pre" & q_number == "1.1.6" & year >= 2013 & year <= 2016 ~ "pre_understanding_sound_pooled",
    pre_post == "post" & q_number == "1.1.6" & year >= 2013 & year <= 2016  ~ "post_understanding_sound_pooled",
    pre_post == "pre" & (q_number == "1.1.6" | q_number == "1.1.7") & year >= 2017 & year <= 2018 ~ "pre_understanding_sound_split",
    pre_post == "post" & (q_number == "1.1.6" | q_number == "1.1.7") & year >= 2017 & year <= 2018 ~ "post_understanding_sound_split",
    
    
    pre_post == "pre" & (q_number == "3.4" | q_number == "3.5") & year >= 2017 & year <= 2018 ~ "pre_skill_communicate_split",
    pre_post == "post" & (q_number == "3.4" | q_number == "3.5") & year >= 2017 & year <= 2018 ~ "post_skill_communicate_split",
    pre_post == "pre" & q_number == "2.4" & year >= 2013 & year <= 2016 ~ "pre_skill_communicate_pooled",
    pre_post == "post" & q_number == "2.4" & year >= 2013 & year <= 2016 ~ "post_skill_communicate_pooled",

    # Guest researcher INITIALS evaluations (only asked post) :
    # AS
    pre_post == "post" & q_number == "6.2" & year == 2013 ~ "prog_eval_guestspeaker_as",
    # KR
    pre_post == "post" & q_number == "6.4" & year == 2013 ~ "prog_eval_guestspeaker_kr",
    # AP
    pre_post == "post" & q_number == "6.4" & year == 2013 ~ "prog_eval_guestspeaker_ap",
    pre_post == "post" & q_number == "6.9" & year == 2015 ~ "prog_eval_guestspeaker_ap",
    # CT
    pre_post == "post" & q_number == "6.9" & year == 2013 ~ "prog_eval_guestspeaker_ct",
    # KB
    pre_post == "post" & q_number == "6.3" & year == 2014 ~ "prog_eval_guestspeaker_kb",
    pre_post == "post" & q_number == "6.4" & year == 2015 ~ "prog_eval_guestspeaker_kb",
    # JF
    pre_post == "post" & q_number == "6.6" & year == 2014 ~ "prog_eval_guestspeaker_jf",
    # AS
    pre_post == "post" & q_number == "6.8" & year == 2014 ~ "prog_eval_guestspeaker_as",
    # KG
    pre_post == "post" & q_number == "6.2" & (year == 2015 | year == 2016) ~ "prog_eval_guestspeaker_kg",
    # CC
    pre_post == "post" & q_number == "6.6" & year == 2015 ~ "prog_eval_guestspeaker_cc",
    
    
    ))

write.csv(question_id_list, "question_id_data_structure.csv", row.names = FALSE)
# drive_upload("question_id_data_structure.csv", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for questions id list: 1UHFoY0kfIT2p2kHD9hzdNJLQWf2pGtvl
drive_update(file = as_id("1VlgrGN4FKbaXEdquaK993dbYY6rlSOts"), media = "question_id_data_structure.csv")
file.remove("question_id_data_structure.csv")
