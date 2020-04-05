# kdgorospe@gmail.com
# Clean REMS data

require(tidyverse)
require(googledrive)

### THe following creates a list of question IDs to be used for specifying the data structure
question_id_list = list()
# Create list of questions and ids
# start with i=2 because i=1 is ID column
for (i in 2:length(names(question_dt))){
  question_id_set <- tibble(id = question_dt$id,
                            question = question_dt[,i]) %>%
    drop_na() %>%
    mutate(q_number = names(question_dt[i])) %>%
    mutate(id = str_replace(id, pattern="q_", replacement = "")) %>%
    separate(id, into = c("year", "test"))
  question_id_list[[i]] <- question_id_set
}

question_id_dt <- rbindlist(question_id_list)



# note: file IDs saved as text file in google drive
# post data:
post_ids <- c("1Rg82pkzL6lOMi1QuSeOLBda6Gox4-BSk", 
              "1TeSyc8oH2ElQvwuBZMke60h5w0mhdZJz",
              "1xYknTNdXLhCEHSamQtnB9ksk1_Sm6N8_",
              "199SByfuEhAp0tatoU3iLcYuiRyg2fmoC",
              "1-hIf5bHKwz2-kmzur18h_Qhs6Q-c_lxj",
              "15bde4B0mFTlI1Hu6EwSW5_z3bNR5HAzx")

# pre data:
pre_ids <- c("1BMd7zVU2NPK6SC8FTs29vcCzABZKZHc0",
             "1NZbFXSr0xiQCzhydtQsfnm3TWCvcPf-D",
             "1c2VJv8W8JbEiDQX_C2oPRUSJ37jsl_Yf",
             "1TrXDAZwVeaOXUSVLpYwctwze0tCin7DE",
             "1kX_Bl9E9xj8Zs4SfmvPl8ZH1-42XP2mg",
             "1Qz1tUDXELybr81liRP8KvQglve_n57A9")

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
  dat_object <- paste("a", dat_year, "post", sep = "_")
  assign(dat_object, read.csv(all_filenames[i]))
}

# Read pre data files into R
pre_grep <- grep("PRE", all_filenames)

for (i in pre_grep) {
  dat_year <- strsplit(all_filenames[i], split = " ")[[1]][1]
  dat_object <- paste("a", dat_year, "pre", sep = "_")
  assign(dat_object, read.csv(all_filenames[i]))
}

# Remove files from local computer
for (i in post_grep) {
  file.remove(all_filenames[i])
}
for (i in pre_grep) {
  file.remove(all_filenames[i])
}

# Combine and reshape answer data into tidy format 
answer_objs <- apropos("a_20") # find all the "answer" objects
answer_list = list()
for (i in answer_objs){
  answer_raw_i <- get(i)
  answer_tidy_i <- answer_raw_i %>% 
    select(-Elapsed) %>%
    select_if(is.numeric) %>%
    pivot_longer(cols = -Number,
                 names_to = "q_number", 
                 values_to = "answer") %>%
    mutate(id = i) %>%
    mutate(id = str_replace(id, pattern="a_", replacement = "")) %>%
    separate(id, into = c("year", "test")) %>%
    mutate(q_number = str_replace(q_number, pattern="X", replacement=""))
  answer_list[[i]] <- answer_tidy_i
}

answer_dt <- rbindlist(answer_list)


# Bind answer data to questions
full_dat <- answer_dt %>%
  inner_join(question_id_dt, by = c("year", "test", "q_number")) %>% # inner_join, check that dim(full_dat) == dim(answer_dt)
  arrange(q_number, year, test)

# START ASSIGNING "CONCEPTS" to each question based on grepping wording
coded_dat <- full_dat %>% 
  mutate(concept = case_when(# question heading: "UNDERSTANDING"
                             question=="The ecology of coral reefs" ~ "understanding_ecology",
                             question=="The scientific process" ~ "understandng_sciprocess",
                             question=="How increasing carbon dioxide in the atmosphere contributes to ocean acidification and how this might affect the health of corals and many marine organisms" ~ "understanding_oceanacid_pooled",
                             question=="The effects of ocean acidification on marine organisms" | question=="The effects of atmospheric carbon dioxide on ocean chemistry" ~ "understanding_oceanacid_split",
                             question=="The challenges of growing and harvesting fish in captivity, also known as aquaculture" ~ "understanding_aquaculture",
                             question=="How an ethologist records data about animal behavior and how the form of an animal and where it lives can influence its behavior" ~ "understanding_ethology",
                             question=="The effects of water quality on the fertilization processes of marine organisms, particularly sea urchins" ~ "understanding_fertilization",
                             question=="The physics of sound and how many marine organisms, such as snapping shrimp, use sound in the ocean for different purposes" ~ "understanding_sound_pooled",
                             question=="The general physics of sound" | question=="How many marine organisms, such as snapping shrimp, use sound in the ocean" ~ "understanding_sound_split",
                             question=="The significance of organism diversity in ecosystem health" ~ "understanding_diversity",
                             question=="The role of coral skeletal structure in reef ecology" ~ "understanding_coralskel",
                             question=="The effects of climate change on coral growth and survival" ~ "understanding_coralclimate",
                             question=="How ideas we will explore in this class relate to your own everyday life" | question=="Marine science concepts relate to my own everyday life" ~ "understanding_relatetolife",
                             question=="How studying this subject is important to society and helps people address real world issues" | question=="How studying marine science is important to society and helps people address real world issues" ~ "understanding_society_pooled",
                             question=="The study of marine science helps people address real world issues" | question=="The study of marine science is important to society" ~ "understanding_society_split",
                             
                             # question heading: "SKILLS"
                             question=="Use the scientific process to ask a question and develop a hypothesis" ~ "skills_developH0",
                             question=="Develop an experiment to test a hypothesis" ~ "skills_testH0",
                             question=="Analyze and interpret experimental data to evaluate a hypothesis" ~ "skills_evalH0",
                             question=="Communicate the results of a research project in written and/or oral format" ~ "skills_communicate_pooled",
                             question=="Communicate the results of a research project in written format" | question=="Communicate the results of a research project in oral format" ~ "skills_communicate_split",
                             question=="Work effectively with others" ~ "skills_withothers",
                             
                             # question heading: "ATTITUDES"
                             question=="Enthusiastic about the marine science" | question=="Enthusiastic about marine science" ~ "attitudes_enthusiastic",
                             question=="Interested in discussing marine science with friends or family" ~ "attitudes_discussing",
                             question=="Interested in taking or planning to pursue a career in marine science" | question=="Interested in a career in marine science" ~ "attitudes_career",
                             question=="Confident that I understand marine science" ~ "attitudes_confidentunderstanding",
                             question=="Confident that I can use the scientific process to execute a research project" ~ "attitudes_confidentresearch",
                             question=="Willing to work with others to accomplish a research project" ~ "attitudes_workwithothers",
                             
                             # question heading: "INTEGRATION"
                             question=="Connecting key ideas I learn in my classes with other knowledge" ~ "integration_connectingknowledge",
                             question=="Applying what I learn in classes to other situations" ~ "integration_applyingknowledge",
                             
                             # question heading: "MAJOR"
                             question=="Marine science" ~ "major_marinesci_yesno",
                             question=="Something in the sciences" ~ "major_science_yesno",
                             question=="A major not in the sciences" ~ "major_notscience_yesno",
                             question=="Undecided as of now" ~ "major_undecided_yesno",
                             question=="Not sure I want to attend college" ~ "major_unsurecollege_yesno",
                             question=="What do you want to major in college?" ~ "major_multiplechoice",
                             
                             # question heading: "EVALUATION"
                             question=="Coral reef ecology lecture/ photoquad reef monitoring activity" ~ "evaluation_coralreefeco",
                             question=="Ocean acidification lecture/lab activity" ~ "evaluation_oceanacid",
                             question=="Plankton ecology lecture/field collecting/lab activity" ~ "evaluation_planktoneco",
                             question=="Hawaiian bobtail squid ethology lecture/lab activity" ~ "evaluation_ethology",
                             question=="The bioacoustics of snapping shrimp lecture/lab activity" ~ "evaluation_bioacoustics",
                             question=="Opakapaka spawning lecture/field collecting/lab activity" ~ "evaluation_aquaculture",
                             question=="Sea urchin fertilization lecture/field collecting/ lab activity" ~ "evaluation_fertilization",
                             question=="Scientific method lecture and scientific paper homework assignment and in class discussion" | question=="Scientific method lecture and guided inquiry activity" | question=="Scientific method lecture and guided inquiry group activity" ~ "evaluation_scimethod",
                             question=="My participation in the group research project was enjoyable" ~ "evaluation_groupresearch_enjoyable",
                             question=="As a result of my group research project experience, I feel confident that I can execute a research project on my own" ~ "evaluation_groupresearch_confident",
                             
                             # question heading: "EVALUATION (Guest lecturers and career day)"
                             question=="Guest researcher lecture Ariana Snow and her presentation on coral reef monitoring" ~  "evalution_guestspeaker_coraleco_as",
                             question=="Guest researcher lecture Kelvin Gorospe and his presentation on coral reef ecology" ~ "evaluation_guestspeaker_coraleco_kg",
                             question=="Guest researcher lecture Keisha Bahr and her presentation on ocean acidification and reef growth" ~ "evaluation_guestspeaker_oceanacid_kb",
                             question=="Guest researcher lecture Ku'ulei Rogers and her presentation on ocean acidification and reef growth" ~ "evaluation_guestspeaker_oceanacid_kr",
                             question=="Guest researcher lecture Christine Carrier and her presentation on ethology and shark sensory" ~ "evaluation_guespseaker_ethology_cc", 
                             question=="Guest researcher lecture Jenny Fung and her presentation on sea urchin fertilzation and water quality" ~ "evaluation_guestspeaker_fertilization_jf",
                             question=="Guest researcher lecture Adam Smith and his presentation on the bioacoustics of marine mammals" ~ "evaluation_guestspeaker_bioacoustics_as",
                             question=="Guest researcher lecture Clyde Tamaru and his presentation on aquaponics and aquaculture" ~ "evaluation_guestspeaker_aquaculture_ct",
                             question=="Guest researcher lecture Aude Pacini and her presentation on the bioacoustics of marine mammals" ~ "evaluation_guestspeaker_bioacoustics_ap",
                             grepl(pattern="Career day in class presentations*", full_dat$question) ~ "evaluation_careerday"))


write.csv(coded_dat, "coded_dat.csv", row.names = FALSE)
#drive_upload("coded_dat.csv", path = as_dribble("REMS_SALG/")) # for initial upload
# FILE ID for coded_dat: 1ztPIrSmNdy0o-0yJpL_oDybcEUEj1vYEy2zrLm0PhyE
drive_update(file = as_id("1ztPIrSmNdy0o-0yJpL_oDybcEUEj1vYEy2zrLm0PhyE"), media = "coded_dat.csv")
file.remove("coded_dat.csv")

