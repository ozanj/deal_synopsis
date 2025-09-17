
library(tidyverse)
library(readxl)
library(tidytext)

getwd()
data_dir <- file.path('.','data')
list.files(path = file.path(data_dir))

df <- read_excel(
  path = file.path(data_dir,'PitchBook_Search_Result_Columns_2025_09_15_11_04_10.xlsx'),
  sheet = "Data",
  skip = 7,
  col_names = TRUE
) %>%
  rename_with(~ gsub(" ", "_", .x)) %>%   # replace spaces with underscores
  rename_with(tolower)                    # lowercase


df %>% glimpse()
df %>% group_by(companies) %>% summarize(n_per_group = n()) %>% ungroup %>% count(n_per_group)
df %>% group_by(deal_id) %>% summarize(n_per_group = n()) %>% ungroup %>% count(n_per_group)

df_description <- df %>% select(deal_id,companies,description) %>% unnest_tokens("word",description)

df_description %>%
  count(word) %>%
  arrange(desc(n)) %>% print(n=100)

df_description %>% print(n=200)

# remove stopwords
 stop_words

temp <- data('stop_words')
temp



df_description<-df_description %>%
  anti_join(stop_words)

stop_words %>% View()
