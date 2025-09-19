################################################################################
## [ PROJ ] < Comapny description, applying lda>
## [ FILE ] < lda_edtech_ai_oj.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 9/17/2025
## [ DESC ] < load data on deals + companies in the edtech+AI space. process and then apply structural topic modeling>
################################################################################

# -------------------------------
# Big picture
# -------------------------------
# Workflow pipeline: quanteda → topicmodels → tidytext
# (prepare texts → fit LDA → tidy/visualize results)

# -------------------------------
# Overview of packages
# -------------------------------
# quanteda: a text analysis framework for managing corpora and preprocessing 
#           (tokenization, cleaning, building document–feature matrices).
#
# topicmodels: provides statistical engines for fitting topic models 
#              (e.g., Latent Dirichlet Allocation, Correlated Topic Models).
#
# tidytext: tools to convert model output into tidy data frames, making results 
#           easy to analyze and visualize with dplyr/ggplot2.

# -------------------------------
# Broad workflow
# -------------------------------
# 1. Use quanteda to prepare texts and represent them as a sparse document–term matrix.  
# 2. Pass the document–term matrix to topicmodels, which fits the LDA model.  
# 3. Use tidytext to reshape the model output into a tidy format for further 
#    exploration, visualization, and interpretation.
#
# -------------------------------
# Why this division of labor?
# -------------------------------
# - quanteda excels at efficient, modern preprocessing and corpus handling.  
# - topicmodels is a mature, stable implementation of LDA/CTM.  
# - tidytext plugs the model into the tidyverse ecosystem, making it seamless 
#   to analyze results alongside other data and produce ggplot visualizations.

library(tidyverse)
library(readxl)
library(lubridate)
library(haven)
library(labelled)

library(quanteda) # pre-processing
library(topicmodels) # LDA (topic modeling)
library(tidytext) # post topic modeling analysis


### DIRECTORY PATHS
data_dir <- file.path('.','data') # main data directory
list.files(path = data_dir)

df <- read_excel(
  path = file.path(data_dir,'PitchBook_Search_Result_Columns_2025_09_17_09_38_28.xlsx'),
  sheet = "Data",
  skip = 7,
  col_names = TRUE
) %>%
  # fix variable names
  rename_with(~ gsub(" ", "_", .x)) %>%   # replace spaces with underscores
  rename_with(~ gsub("-", "_", .x)) %>%   # replace dashes with underscores
  rename_with(~ gsub("/", "_", .x)) %>%   # replace forward slashes with underscores
  rename_with(~ gsub("/", "_", .x)) %>%   # replace forward slashes with underscores  
  rename_with(~ gsub("\\(", "", .x)) %>%   # replace open parentheses with nothing
  rename_with(~ gsub("\\)", "", .x)) %>%   # replace close parentheses with nothing
  rename_with(~ gsub("%", "pct", .x)) %>%   # replace percent symbol with 'pct'
  rename_with(tolower) %>%                     # lowercase
  # drop missing values of company_id
  filter(!is.na(company_id)) %>%     
  mutate(deal_date = as_date(deal_date, origin = "1899-12-30")) %>% 
  mutate(deal_year = year(deal_date)) %>% 
  # drop deal year prior to 2015
  filter(deal_year >= 2015)

df %>% count(deal_year)

# investigate units of observation

df %>% group_by(company_id) %>% summarize(n_per_group = n()) %>% ungroup() %>% count(n_per_group)
df %>% group_by(deal_id) %>% summarize(n_per_group = n()) %>% ungroup() %>% count(n_per_group)

# check whether company description is always the same within each value of company_id
df %>% group_by(company_id) %>% summarise(n_desc = n_distinct(description)) %>% count(n_desc)

df_bc <- df %>% filter(deal_year<2023)
df_ad <- df %>% filter(deal_year>=2023)

df %>% glimpse()
df %>% select()

# Function to get most recent deal per company given a year filter
get_recent_company <- function(data, year_filter) {
  data %>%
    filter({{ year_filter }}) %>%
    arrange(company_id, desc(deal_date)) %>%
    group_by(company_id) %>%
    mutate(company_obs = row_number()) %>%
    ungroup() %>%
    filter(company_obs == 1) %>%
    select(
      company_id, companies, description, year_founded,
      current_financing_status, current_business_status,
      universe, hq_location, deal_year
    )
}

# Use the function for each subset
# separates deals into those before 2023 and those 2023+; then keeps the most recent deal per company; 
# results in dataframe with one obs per company
# one dataframe keeps obs where most recent deal is before 2023
# another dataframe keeps obs where most recent deal is after 2023
# companies that received deals pre and post 2023 would be in both data frames
df_company_bc <- get_recent_company(df, deal_year < 2023)
df_company_ad <- get_recent_company(df, deal_year >= 2023)

# -------------------------------
# 1. Text preprocessing
# -------------------------------
# Tokenize, clean, and build a document-feature matrix (dfm)
dfm_ad <- df_company_ad$description %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  dfm()

dfm_ad %>% str()

# Convert to topicmodels format (DocumentTermMatrix from tm)
dtm_ad <- convert(dfm_ad, to = "topicmodels")

# -------------------------------
# 2. Search for K
# -------------------------------
# LDA models need to be fit separately for each K
# (unlike stm::searchK, there’s no built-in grid search).
# You can loop across K and compare metrics like perplexity or coherence.

results_ad <- data.frame(K = integer(), perplexity = numeric())

for (k in 5:25) {
  lda_k <- LDA(dtm_ad, k = k, control = list(seed = 1234))
  perp  <- perplexity(lda_k, dtm_ad)
  results_ad <- rbind(results_ad, data.frame(K = k, perplexity = perp))
}

plot(results_ad$K, results_ad$perplexity, type = "b",
     xlab = "Number of topics (K)", ylab = "Perplexity")

results_ad

# -------------------------------
# 3. Fit LDA at chosen K
# -------------------------------
# Fit final LDA model at selected K (say K = 8)
lda_ad <- LDA(dtm_ad, k = 5, control = list(seed = 1234)) 

# Extract tidy output for interpretation
library(tidytext)

tidy_beta <- tidy(lda_ad, matrix = "beta")   # word-topic probabilities; which words are most representative of each topic.
tidy_gamma <- tidy(lda_ad, matrix = "gamma") # doc-topic probabilities; how much each topic contributes to each document.

tidy_beta %>% glimpse()
tidy_gamma %>% glimpse()


####### Top words per topic
tidy_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  arrange(topic, -beta) %>% print(n=50)

top_terms <- tidy_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%   # top 10 words per topic
  ungroup()

# Barplot of top words
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Words per Topic", y = "Word Probability (β)", x = "Word")

######### 2. Topic Prevalence Across Documents (interpreting γ matrix)

tidy_gamma <- tidy(lda_ad, matrix = "gamma")  # document-topic probabilities

# Average prevalence per topic
avg_prevalence <- tidy_gamma %>%
  group_by(topic) %>%
  summarize(mean_gamma = mean(gamma))
avg_prevalence
ggplot(avg_prevalence, aes(x = factor(topic), y = mean_gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Average Topic Prevalence", x = "Topic", y = "Average γ")

tidy_gamma %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>% print(n=200)