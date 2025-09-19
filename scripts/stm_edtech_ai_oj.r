################################################################################
## [ PROJ ] < Comapny description, applying STM>
## [ FILE ] < stm_edtech_ai_oj.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 9/17/2025
## [ DESC ] < load data on deals + companies in the edtech+AI space. process and then apply structural topic modeling>
################################################################################

### SETTINGS
#rm(list = ls())
options(max.print=1000)
#options(width = 160)

### LIBRARIES
library(tidyverse)
library(readxl)
library(lubridate)
library(haven)
library(labelled)
library(stm)
library(tidytext)

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


############## RECOMMENDATIONS ON PRE-PROCESSING

# Short answer:
# Yes—strip boilerplate that appears in most PitchBook descriptions.
# Do it data-driven (by document frequency) plus a light human pass,
# so you don’t toss informative domain terms.

# What experts generally do (best practice)
# 1) Start with standard stopwords.
#    Remove function words (the, and, of…).

# 2) Trim by document frequency (DF).
#    - Rare terms add noise → drop tokens appearing in only a few docs.
#    - Ubiquitous terms carry little signal → drop tokens in a very high share of docs (e.g., >80–95%).
#    This is the principled way to remove template-y words without hand-curating huge lists.

# 3) Detect and keep informative phrases; then prune the generic parts.
#    Identify bigrams like artificial_intelligence, machine_learning, learning_platform.
#    Keep the phrase token; consider removing singletons (artificial, intelligence, platform)
#    if they are nearly ubiquitous and not discriminative once the phrase is kept.

# 4) Add a small domain stoplist for boilerplate.
#    After a DF pass, skim high-DF tokens and add truly generic, non-substantive terms:
#    developer, provider, company, platform, application, designed, intended,
#    offers, enabling, services, solution(s), technology/technologies.
#    (These appear in most PB blurbs and don’t define topics.)

# 5) Don’t remove content-bearing domain terms.
#    Keep words that split the corpus into subthemes:
#    Arabic, K-12, Bangladesh, VR/AR, career, math, tutoring, SaaS, chatbot,
#    admissions, mentoring, healthcare, enterprise, etc.

# 6) Evaluate with topic quality + human read.
#    Refit STM after trimming and check coherence & exclusivity.
#    Do a quick human scan of top words.
#    If topics look mushy → tighten upper DF cut / expand boilerplate list.
#    If topics look too narrow → relax lower DF cut.

# Rules of thumb for PitchBook blurbs
# - Remove: “developer of…”, “provider of…”, “the company…”, “designed to…”, “intended to…”,
#           “offers…”, “enabling…”, “platform”, “application”, “solution(s)”, “service(s)”.
# - Keep/phrase: artificial_intelligence, machine_learning, augmented_reality, K_12, Arabic,
#                Bangladesh, career_counseling, math_learning, VR_training, entrance_exam, etc.
# - Let DF decide ambiguous terms (e.g., education, learning). If they appear in nearly every doc,
#   they won’t help separate topics—drop or down-weight by pruning.

# Outcome:
# DF-based pruning + tiny domain stoplist + phrase keeping
# → cleaner, more interpretable STM topics without over-engineering preprocessing.

# on words like developer,operator, provider:

# one pushback on your advice: words like "operator" "developer" "provider" seem to mean substantive things, like a developer means they are creating the product whereas operator or provider seem to mean something different. so maybe those words should be retained?

# Totally fair pushback—the words **developer / operator / provider** do carry semantics. The issue is **utility**, not meaning:
#   
#   * **They’re near-ubiquitous in PB blurbs**, so they usually *don’t* help separate topics. They lower exclusivity and can swamp more informative terms (VR, K-12, chatbot, Arabic, etc.).
# * **What’s your goal?**
#   
#   * If you want topics about **offerings/markets/tech**, **drop** these role words.
# * If you want to study **business role/positioning**, **keep** them (or, better, encode them explicitly).
# 
# Best compromise (common in expert workflows):
#   
# 1. **Extract the role as metadata** (e.g., `role ∈ {developer, operator, provider}` from the first clause).
# 2. **Remove the role words from the text tokens** so they don’t dominate topics.
# 3. **Use the role as a prevalence covariate in STM** (`prevalence = ~ role + …`).
# 
# * This lets you ask: *How does topic prevalence differ for developers vs providers vs operators?*
#   4. (Optional) **Run stratified models** by role if that’s substantively central.
# 
# How to decide empirically:
#   
#   * Check **document frequency** (if DF > \~0.7–0.9, likely not discriminative).
# * Compare **coherence/exclusivity** and **top words** with vs without these tokens.
# * If keeping them, consider **phrases** like `developer_of`, `provider_of_services` to retain some specificity without flooding.
# 
# If you want, I can give you a small R snippet to (a) regex-extract the lead role as a factor and (b) drop those tokens from the STM text while using `role` in `prevalence`.


############# RUN STM SEPARATELY ON: COMPANIES WHOSE MOST RECENT DEAL IS BEFORE 2023; COMPANIES WHOSE MOST RECENT DEAL IS AFTER 2023

# BEFORE CHATGPT 3
  processed_bc <- textProcessor(documents = df_company_bc$description, metadata = df_company_bc)

  out_bc <- prepDocuments(
    documents = processed_bc$documents, 
    vocab = processed_bc$vocab, 
    meta = processed_bc$meta,
    lower.thresh = 1,
    upper.thresh = Inf,
    verbose = TRUE
  ) # process loaded data to make sure it is in right format, and remove infrequent terms
  

  findingk_bc <- searchK(
    documents = out_bc$documents, 
    vocab = out_bc$vocab, 
    K = c(5:25),
    prevalence = NULL, 
    data = out_bc$meta,
    init.type = "Spectral",
    verbose=TRUE
  )
  
  plot(findingk_bc)
  findingk_bc$results
  
  stm_bc <- stm(
    documents = out_bc$documents, 
    vocab = out_bc$vocab,
    K = 10, 
    prevalence = NULL,
    max.em.its = 500,
    data = out_bc$meta,
    init.type = "Spectral"
  )  
  
  # frex words
  class(stm_bc)
  labelTopics(stm_bc, n = 10)$frex
  plot(stm_bc, type = "labels", labeltype = "frex", n = 10)
  
  # Plot
  plot(stm_bc)
  
  findThoughts(
    stm_bc, 
    texts = df_company_bc$description,
    n = 2,
    #topics = 3
  )
  
###### AFTER CHATGPT3
  
  # -------------------------------
  # 1. Text preprocessing
  # -------------------------------
  processed_ad <- textProcessor(
    documents = df_company_ad$description,
    metadata  = df_company_ad
  )
  
  out_ad <- prepDocuments(
    documents    = processed_ad$documents, 
    vocab        = processed_ad$vocab, 
    meta         = processed_ad$meta,
    lower.thresh = 1,
    upper.thresh = Inf,
    verbose      = TRUE
  )
  
  # -------------------------------
  # 2. Search for K
  # -------------------------------
  findingk_ad <- searchK(
    documents  = out_ad$documents, 
    vocab      = out_ad$vocab, 
    K          = 5:25,
    prevalence = NULL, 
    data       = out_ad$meta,
    init.type  = "Spectral",
    verbose    = TRUE
  )
  
  plot(findingk_ad)
  findingk_ad$results
  
  # -------------------------------
  # 3. Fit STM at chosen K
  # -------------------------------
  stm_ad <- stm(
    documents   = out_ad$documents, 
    vocab       = out_ad$vocab,
    K           = 8,               # replace with your chosen K
    prevalence  = NULL,
    max.em.its  = 500,
    data        = out_ad$meta,
    init.type   = "Spectral"
  )
  
  stm_ad %>% str(max.level = 1) 
  
  # a matrix that has documents as rows.for each document, shows each "document's topic proportions" across the 8 topics. always sums to 1.
  stm_ad$theta %>% str()
  
  theta_ad <- stm_ad$theta %>% as_tibble() %>% rename_with(tolower) %>% 
    mutate(
      max_prop  = do.call(pmax, select(., starts_with("v"))),
      max_topic = paste0("v", max.col(select(., starts_with("v")), ties.method = "first"))
    ) 
  
  
  # merge og dataframe with results from stm
  df_company_ad %>% glimpse()
  theta_ad %>% glimpse()
  
  df_company_ad_with_topics <- bind_cols(df_company_ad, theta_ad)
  
  df_company_ad_with_topics %>% glimpse()
  theta_ad %>%
    tibble::rowid_to_column("doc_id")
  
  df_company_ad_with_topics %>% count(max_topic) %>% mutate(pct = n/sum(n))
  
  df_company_ad_with_topics %>% count(current_financing_status) %>% mutate(pct = n/sum(n))
  
  df_company_ad_with_topics %>% filter(
    current_financing_status %in% c('Accelerator/Incubator Backed','Venture Capital-Backed')
  ) %>% group_by(current_financing_status) %>% count(max_topic) %>% mutate(pct = n/sum(n))

  
  library(ggplot2)
  
  df_plot <- df_company_ad_with_topics %>%
    filter(current_financing_status %in% c("Accelerator/Incubator Backed", "Venture Capital-Backed")) %>%
    group_by(current_financing_status) %>%
    count(max_topic) %>%
    mutate(pct = n / sum(n))
  
  ggplot(df_plot, aes(x = max_topic, y = pct, fill = current_financing_status)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "Dominant Topic",
      y = "Percent of Companies",
      fill = "Financing Status",
      title = "Distribution of Dominant Topics by Financing Status"
    ) +
    theme_minimal()
  
  # -------------------------------
  # 4. Inspect topics using labelTopics() function
  # -------------------------------
  # EXPLANATION OF THE FOUR WAYS OF IDENTIFYING WORDS ASSOCIATECD WITH A TOPIC: prob; frex; lift; score
  
    # prob: Lists the most frequent words in the topic; these are common within the topic 
    # but may also appear across many topics. Good for seeing what dominates the topic overall.  
    #
    # frex: Balances frequency and exclusivity, surfacing words that are both common in the 
    # topic and relatively unique to it. Often the best single list for labeling topics.  
    # ➡️ Compared to lift, FREX doesn’t over-emphasize rare words; compared to score, 
    # it’s more explicit about weighting exclusivity.  
    #
    # lift: Highlights words that are rare in the whole corpus but disproportionately likely 
    # in this topic. These can be very distinctive but sometimes too obscure.  
    # ➡️ Unlike frex or score, lift may produce unusual or low-frequency words that 
    # aren’t always intuitive as labels.  
    #
    # score: Picks words that strike a balance between being frequent in the topic and not 
    # spread widely across others. These are often strong, interpretable descriptors.  
    # ➡️ Compared to frex, score leans more on frequency; compared to lift, score avoids 
    # overly rare words while still favoring distinctiveness.    
  
  labelTopics(
    model = stm_ad,
    topics = NULL, # all
    n = 7 # 7 is default
  ) %>% str()
  # List of 5
  # $ prob     : chr [1:8, 1:7] "platform" "develop" "improv" "learn" ... # highest probability words
  # $ frex     : chr [1:8, 1:7] "career" "read" "write" "materi" ... # words that are both frequent to the topic and exclusive to the topic
  # $ lift     : chr [1:8, 1:7] "alumni" "comic" "non-nat" "stealth" ... # highest scoring words by lift
  # $ score    : chr [1:8, 1:7] "career" "librarian" "paraphras" "stealth" ...
  # $ topicnums: int [1:8] 1 2 3 4 5 6 7 8
  # - attr(*, "class")= chr "labelTopics"  
  
  labelTopics(stm_ad, n = 8)$frex %>% str() # matrix
  
  
  # Plot just FREX labels (note: plot.stm still gives all panels)
  plot(stm_ad, type = "labels", labeltype = "frex", n = 10)
  
  # Default summary plot
  plot(stm_ad)
  
  # -------------------------------
  # 5. Example: see example docs per topic
  # -------------------------------
  findThoughts(
    stm_ad, 
    texts = df_company_ad$description,
    n     = 2
    # topics = 3
  )
  
############# STANDARD CHECKS AFTER RUNNING STM
  
# CONVERGENCE & FIT
  # ELBO trace: look for monotone increase and a flat tail
  stm_ad$convergence$bound
  
  plot(stm_ad$convergence$bound, type = "l", xlab = "EM iteration", ylab = "ELBO")
  
  
  # Held-out likelihood (train/test split)
  set.seed(1)
  ho <- make.heldout(out_ad$documents, out_ad$vocab)
  stm_ho <- stm(ho$documents, ho$vocab, K = 8, init.type = "Spectral", data = out_ad$meta)
  eval_ho <- eval.heldout(stm_ho, ho$missing)
  eval_ho$expected.heldout  # higher is better
  
  # PROCESS: TL;DR: You got -5.825 at K=8. Next step: run this across multiple K and compare curves. That’s how you see if 8 is the “sweet spot” or whether another K gives a stronger fit.
  
# TOPIC QUALITY (COHERENCE & EXCLUSIVITY)
  
  # In Structural Topic Models (STM), "topic quality" is usually judged with two commonsense criteria:  
  
  # Coherence
  # - Think of this as: Do the top words in a topic actually make sense together?
  # - A topic has high coherence if, when you read its most probable words, they form a meaningful theme
  #   that a human can easily recognize.
  # - Example: If a topic’s top words are doctor, hospital, nurse, patient, surgery,
  #   most people would say, “Ah, that’s clearly about healthcare.” That’s coherent.
  # - By contrast, if the top words are apple, gravity, lawyer, concert, shoe,
  #   the topic would feel incoherent.
  
  # Exclusivity
  # - Think of this as: Are the top words unique to this topic, or do they also show up in other topics?
  # - A topic has high exclusivity if its defining words appear mainly in that topic and not spread across many others.
  # - Example: If "doctor" mostly appears in the healthcare topic and rarely elsewhere, it’s exclusive.
  #   If "people" shows up in nearly every topic, it’s not exclusive.
  # - Exclusivity helps separate topics from one another, so they don’t all blur together.
  
  # In short:
  # - Coherence = words in a topic “stick together” and make sense as a group.
  # - Exclusivity = words in a topic are “specialized” to that topic and don’t just appear everywhere.
  
  # Per-topic semantic coherence & exclusivity
  sc  <- semanticCoherence(stm_ad, out_ad$documents)
  ex  <- exclusivity(stm_ad)
  qdf <- tibble(topic = 1:nrow(stm_ad$beta$logbeta[[1]]), coherence = sc, exclusivity = ex)
  qdf %>% arrange(desc(coherence))  # top coherent topics
    # coherence: log-likelihood style measure; values are usually negative; closer to zero = better coherence
      # Topic 4 (-29.5) looks the most coherent (easiest for a human to interpret).
      # Topic 5 (-76.2) looks the least coherent.
    # exclusivity: higher value = more unique words in the topic (less overlap with other topics)
    # 
  
  # Built-in joint plot [y-axis = exclusivity; x-axis = coherence]
  topicQuality(model = stm_ad, documents = out_ad$documents)
  # Interpretation: prefer topics that are high coherence and reasonably exclusive. Very exclusive + low coherence often = junk.
  
# INTERPRETABILITY: LABELS & EXAMPLES
  # Multiple label types
  lt <- labelTopics(stm_ad, n = 10)
  lt
  
  # Top documents for human reading
  set.seed(1)
  ex <- findThoughts(stm_ad, texts = df_company_ad$description, n = 3)
  ex %>% str()
  ex$docs[[1]] # some examples of topic1  # read a few per topic  
  ex$docs[[4]] # some examples of topic4  # read a few per topic  
  
# TOPIC PREVALENCE SUMMARIES (OVERALL & BY GROUPS)
  
  theta_ad <- stm_ad$theta %>% as_tibble() %>% rename_with(~paste0("v", seq_along(.)))
  # Overall mean prevalence per topic
  colMeans(theta_ad) %>% round(3)
  
  # By metadata group (post-hoc)
  bind_cols(df_company_ad, theta_ad) %>%
    pivot_longer(starts_with("v"), names_to = "topic", values_to = "gamma") %>%
    filter(current_financing_status %in% c("Accelerator/Incubator Backed","Venture Capital-Backed")) %>%
    group_by(current_financing_status, topic) %>%
    summarise(mean_gamma = mean(gamma), .groups = "drop") %>%
    arrange(current_financing_status, desc(mean_gamma))  



# REDUNDANCY AND CORRELATION AMONG TOPICS
# Topic correlation structure
tc <- topicCorr(stm_ad)
tc
plot(tc)  # network of positively correlated topics

# what sort of correlation values imply that something is wrong with our model

  # 1) Dense positive cliques (lots of strong positive edges)
  # 
  # Symptom: Many pairs with r > ~0.4–0.5, big connected components, “hairball” graph.
  # 
  # Meaning: Topics are not distinct; you likely split one theme into many.
  # 
  # Fixes: Lower K; improve phrase detection; remove boilerplate; re-init with different seeds; consider hierarchical modeling.
  
  # LISTS 7 OTHER KINDS OF CORRELATIONS WE DON'T WANT TO SEE!

# Interpretation of correlations
# 
  # The correlation is computed across documents on the document-level topic proportions (θ).
  # 
  # A value of −0.1884 between topics 4 and 6 means: in documents where topic 4’s share is higher than typical, topic 6’s share tends to be a bit lower than typical, and vice versa.
  # 
  # It’s modest, and because θ’s live on a simplex (shares must sum to 1), mild negatives are common—so don’t over-interpret small values.
  # 
  # Technically it’s on the logistic-normal scale used by STM, but the practical reading above is correct.


# (Optional) quick duplicate check: cosine between beta vectors
library(Matrix)
B <- exp(stm_ad$beta$logbeta[[1]])  # topic x vocab probabilities
normB <- B / sqrt(rowSums(B^2))
cos_sim <- normB %*% t(normB)
diag(cos_sim) <- NA
which(cos_sim > 0.95, arr.ind = TRUE)  # near-duplicate topics to inspect  

# if above line returns no output it means we don't have any near-duplicate topics.

# STABILITY/SENSITIVITY

  # OF INTEREST: WHEN WE SET DIFFERENT SEED, ARE THE RESULTS BASICALLY THE SAME?

m1 <- stm(out_ad$documents, out_ad$vocab, K = 8, init.type = "Spectral")
m2 <- stm(out_ad$documents, out_ad$vocab, K = 8, init.type = "Spectral")

library(proxy)

# Extract topic-word distributions (β) from both models
beta1 <- exp(m1$beta$logbeta[[1]])
beta2 <- exp(m2$beta$logbeta[[1]])

# Compute cosine similarity between topics
sim_matrix <- proxy::simil(beta1, beta2, method = "cosine")
sim_matrix <- as.matrix(sim_matrix)

# For each topic in m1, find best match in m2
topic_match <- apply(sim_matrix, 1, which.max)

topic_match

# Nice — that result
# topic_match
# [1] 1 2 3 4 5 6 7 8
# 
# means your two STM runs (m1 and m2) are essentially identical:
#   
#   Topic 1 in m1 best matches Topic 1 in m2,
# Topic 2 matches Topic 2,
# … and so on.
# That’s a good sign — your initialization + corpus were stable enough that the model didn’t permute or drift.

  # NEIGHBORING K VALUES
mods <- manyTopics(out_ad$documents, out_ad$vocab, K = c(7,8,9), data = out_ad$meta, init.type="Spectral")
# Compare coherence/exclusivity across K’s:
lapply(mods$models, function(m) c(mean(semanticCoherence(m, out_ad$documents)),
                                  mean(exclusivity(m)))) %>% do.call(rbind, .)

    # THIS DIDN'T WORK

  # Preprocessing sensitivity
  # Refit after tweaking: stopword list, stemming, min term frequency (e.g., lower.thresh = 2), bigrams.
  # Check whether core topics persist.

# COMMUNICATION PLOTS
  # THIS IS VERY HELPFUL!!!!
  # Label panels (FREX often nicest)
  plot(stm_ad, type = "labels", labeltype = "frex", n = 10)
  
  # Topic prevalence (with CIs) – if you fit with prevalence + estimateEffect()
  # prep <- estimateEffect(1:8 ~ current_financing_status, stm_ad, meta = out_ad$meta)
  # plot(prep, "current_financing_status", method = "difference",
  #      cov.value1 = "Venture Capital-Backed", cov.value2 = "Accelerator/Incubator Backed")

# SANITY CHECKS ON EXTREMES

  # Docs most/least associated with a topic
  top_docs <- apply(stm_ad$theta, 2, function(x) order(x, decreasing = TRUE)[1:3])
  bad_docs <- apply(stm_ad$theta, 2, function(x) order(x, decreasing = FALSE)[1:3])
  # Manually read those descriptions to confirm topic meaning / leakage.

  top_docs
  
  bad_docs

############# OG, RUN STM ON ALL DEALS
  
  # textProcessor() function
  # Function that takes in a vector of raw texts (in a variety of languages) and performs basic operations. This function is essentially a wrapper tm package where various user specified options can be selected.
  # does stuff like: converts to lowercase, removes "stop words", removes numbers, removes punctuation, turns all words into "stem" words
  processed <- textProcessor(documents = df_company$description, metadata = df_company)
  
  # investigate object created by textProcessor() function
  processed %>% str(max.level = 1)
    # processed$docs.removed 
  
  processed$vocab %>% str() # character vector of 4,053
  processed$meta %>% str() # dataframe w/ x columns
  processed$documents %>% str() # list of 1,988 elements; one element per company
    processed$documents[[1]] %>% str() # this is an integer matrix. that has 2 rows and 35 columns. 
      # one column for each word in the description field that has not been removed by textProcessor() function
      # two rows
        # row 1 = word IDs = the element number for the word in the processed$vocab object, which is the "dictionary"
        # row 2 = word counts = number of times that word appears in that document [the description for an individual company]
    
    processed$documents[[1]]
  
    # prepDocuments() function: 
    # Performs several corpus manipulations including removing words and renumbering word indices (to correct for zero-indexing and/or unused words in the vocab vector).
    out <- prepDocuments(
            documents = processed$documents, 
            vocab = processed$vocab, 
            meta = processed$meta,
            lower.thresh = 1,
            upper.thresh = Inf,
            verbose = TRUE
          ) # process loaded data to make sure it is in right format, and remove infrequent terms
    # Removing 1883 of 4053 terms (1883 of 64050 tokens) due to frequency 
    # Your corpus now has 1988 documents, 2170 terms and 62167 tokens.
    # terms = unique words in your "vocabulary"
    # tokens = total word occurences (across all documents)
    # own interpretation. removed 1,883 words that appear only once. 
    # prior to removal. there were 64,050 tokens and 4053 terms
    # after removal. there were 64,050 - 1,883 X 1 = 62,167
    
    
      # term
    
  # investigate out object
  out %>% str(max.level = 1)
  # $ documents     :List of 1988
  # $ vocab         : chr [1:2170] "--date" "--job" "--one" "-app" ...
  # $ meta          :'data.frame':	1988 obs. of  8 variables:
  # $ words.removed : chr [1:1883] "--profit" "--shelf" "-action" "-built" ...
  # $ docs.removed  : NULL
  # $ tokens.removed: int 1883
  # $ wordcounts    : int [1:4053] 3 4 6 1 1 1 3 1 1 2 ...
  
  # look at words removed
  out$words.removed
  
  # look at words still in vocab
  out$vocab
  
 out$documents %>% str()
 out$documents[[4]] 
 processed$documents[[4]] # matrix has more columns (terms)

# create objects for stm   
 docs <- out$documents # documents in desired matrix form
 docs %>% str()
 
 vocab <- out$vocab
 vocab %>% str()
 
 meta <-out$meta
 meta %>% str()

 findingk <- searchK(
   documents = out$documents, 
   vocab = out$vocab, 
   K = c(5:30),
   prevalence = NULL, 
   data = meta,
   init.type = "Spectral",
   verbose=TRUE
 )
 
 findingk %>% str()
 findingk
 plot(findingk)
 # plot of Held-out likelihood
 # y =  probability of words appearing in document when words have been removed from the document in the estimation step
 # plot of residuals
 # plot of semantic coherence
 # plot of lower bound
 
 findingk$results
 #exclus (exclusivity): higher = topics use more distinctive words (less overlap between topics). You want higher exclusivity.
 
 #semcoh (semantic coherence): how well the top words in each topic co-occur in documents. More negative means worse coherence. You want values closer to zero (less negative).
 
 #heldout (held-out likelihood): measures predictive performance on unseen words. Closer to zero is better (−5.9 is a common range). If it worsens sharply, the model is overfitting.
 
 #residual: unexplained variance in the data. Lower is better.
 
 #bound / lbound: variational bounds on the model fit. More positive (less negative) is better. The trend matters more than the absolute value.
 
 #em.its: how many EM iterations the model needed to converge.  
 
 
  stm <- stm(
    documents = out$documents, 
    vocab = out$vocab,
    K = 13, 
    prevalence = NULL,
    max.em.its = 500,
    data = out$meta,
    init.type = "Spectral"
  )  
 

  
  # frex words
  plot(stm)
  
  findThoughts(
    stm, 
    texts = df_company$description,
    n = 2,
    #topics = 3
  )
  