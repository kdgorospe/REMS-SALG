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

question_list <- question_list %>%
  mutate_if(.predicate = is.factor, .funs = as.character)

write.csv(question_list, "question_list.csv", row.names = FALSE)
# FIX IT - publish as google xlsx? format or keep as csv
drive_upload("question_list.csv", path = as_dribble("REMS_SALG/"))


file.remove("question_list.csv")
