
#Load libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(tidytext)


#Load & clean data
getwd()
data_dir <- file.path(".", 'data')
list.files(path = file.path(data_dir))
data <- read_excel(
  path = file.path("data/PitchBook_Search_Result_Columns_2025_09_15_11_04_10.xlsx"),
  sheet = "Data",
  skip = 7,
  col_names = TRUE,
) %>%
  rename_with(~ gsub(" ", "_", .x)) %>%   # replace spaces with underscores
  rename_with(tolower)                    # lowercase


# create tidy-text object of deal descriptions
df_description <- data %>% select(deal_id,companies,description) %>% unnest_tokens("word", description)

#remove stopwords 
data("stop_words")
df_description<-df_description %>%
  anti_join(stop_words)
#count of stopwords
df_description %>%
  count(word) %>%
  arrange(desc(n))
